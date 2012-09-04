package controllers

import java.util.Date
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList
import scala.collection.mutable.Map
import org.joda.time.format.ISODateTimeFormat
import github._
import models._
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.Logger
import play.api.libs.concurrent.{ Redeemed, Thrown }
import play.api.libs.concurrent.Akka
import play.api.Play.current
import scala.collection.mutable.ListBuffer
import scala.actors.threadpool.TimeoutException
import play.api.mvc.Result
import play.api.libs.concurrent.Akka
import akka.actor.Props
import akka.util.duration._
import java.util.Calendar
import org.codehaus.jackson.JsonNode
import com.codahale.jerkson.ParsingException
import math._

/**
 */

object CoordBot extends Controller {
  type Label = String; type NewComment = Comment

  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/" + gitHubUser + "/" + gitHubRepo
  //val hookUrl = "http://requestb.in/1imhe4b1"
  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue]()
  val reviewerList = List("@taolee", "@adriaan", "@odersky", "@lukas", "@heather", "@vlad")
  val reviewMsgList = List("Review", "review", "REVIEW")
  val reviewedMsgList = List("LGTM", "lgtm")
  val defaultLabelList = List("tested", "reviewed")
  var specifiedReviewer = new ListBuffer[String]
  var issueAction, issueTitle, issueBody = ""
  var totalLabelList = new ListBuffer[String]()

  val waitMinInterval = 12 * 48 // 48 hours = (12 * 5) * 48 minutes
  val waitHourInterval = 6
  val waitDayInterval = 2
  val updateFrequency = 10 minutes
  val initialDelay = 1 minutes
  val enableActor = true
  var coordBotInit = true

  val coordActor = Akka.system.actorOf(Props[CoordActor])
  if (enableActor) {
    Akka.system.scheduler.schedule(initialDelay, updateFrequency, coordActor, "refresh")
    Logger.debug("Coordination Actor is activated")
  }

  /**
   * The main page
   */

  def index() = Action { implicit request =>
    handleTimeout(e => "Timeout when reading list of open issues " + e.toString) {
      if (coordBotInit) {
        try {
          // setup web hooks
          setupGithubEnv
          // initialize issue-comment view
          issueMap.clear
          initIssueCommentView
        } catch {
          case e: MalFormedJSONPayloadException =>
            Logger.error(e.toString())
          case e: MissingDefaultLabelsException =>
            Logger.error(e.toString())
          case e: ParsingException =>
            Logger.error(e.getMessage())
          case _ =>
            Logger.error("Expecting JSON data")
        }
      }
      // show open issues
      val issues = issueMap.values.toList.sortWith((a, b) => a.Number < b.Number)
      Ok(views.html.index(issues))
    }
  }

  /**
   * Some tools
   */

  def handleTimeout(timeoutMsg: TimeoutException => String)(page: => Result) = {
    try { page }
    catch {
      case e: TimeoutException =>
        RequestTimeout(views.html.error(timeoutMsg(e)))
    }
  }

  val dateParser = ISODateTimeFormat.dateTimeNoMillis();
  def parseISO8601(date: String): Date = {
    dateParser.parseDateTime(date).toDate()
  }

  def milliSecToSec(milliSec: Long): Long = { milliSec / 1000 }
  def milliSecToMin(milliSec: Long): Long = { milliSec / (1000 * 60) }
  def milliSecToHour(milliSec: Long): Long = { milliSec / (1000 * 60 * 60) }
  def milliSecToDay(milliSec: Long): Long = { milliSec / (1000 * 60 * 60 * 24) }

  /**
   * The coordination bot methods
   */

  def receiveGithubMsg = Action { msg =>
    var issueNumber: Long = -1
    var issueAction, issueTitle, issueBody = ""
    def processIssue = {
      Logger.debug("An issue is " + issueAction)
      Logger.debug("Issue title: " + issueTitle)
      Logger.debug("Issue body: " + issueBody)
      var body = ""
      if (missingJIRALinks(issueBody, JIRATickets(issueTitle)).map { link => body += "\n" + link }.size > 0) {
        Logger.debug("Add JIRA links to the body of issue " + issueNumber)
        issueBody += body
        GithubAPI.editIssueBody(issueNumber, issueBody)
      }
    }
    def updateIssue = {
      issueMap.get(issueNumber) match {
        case Some(issue) =>
          "Update this issue"
          if (issueAction != "closed") {
            processIssue
            issue.updateTitle(issueTitle)
            issue.updateBody(issueBody)
          } else // issue closed
            issueMap = issueMap.-(issueNumber)
        case None =>
          "Create a new issue"
          if (issueAction != "closed") {
            processIssue
            issueMap = issueMap.+((issueNumber, new Issue(issueNumber, issueTitle, issueBody)))
          }
      }
    }
    def receive = {
      Logger.info("Receive message: " + msg.body.toString())
      msg.body.asFormUrlEncoded.map { urlenc =>
        val urlencPayload = (urlenc.get("payload")) match {
          case Some(jsonStringSeq) =>
            val jsonString = jsonStringSeq.head;
            Logger.info("Receive JSON payload: " + jsonString)
            try {
              val json = Json.parse(jsonString)
              // action
              (json \ "action").asOpt[String] match {
                case Some(action) =>
                  issueAction = action
                  // issue
                  val issueString = (json \ "issue").toString()
                  if (issueString != null) {
                    val issueJson = Json.parse(issueString)
                    (issueJson \ "number").asOpt[Long] match {
                      case Some(number) => issueNumber = number
                      case None => throw new MalFormedJSONPayloadException("Missing issue number")
                    }
                    (issueJson \ "title").asOpt[String] match {
                      case Some(title) => issueTitle = title
                      case None => throw new MalFormedJSONPayloadException("Illegal issue title")
                    }
                    (issueJson \ "body").asOpt[String] match {
                      case Some(body) => issueBody = body
                      case None => throw new MalFormedJSONPayloadException("Illegal issue body")
                    }
                    updateIssue
                    val commentString = (json \ "comment").toString()
                    if (commentString != null) {
                      "update issue-comment view"
                      updateIssueCommentView(issueNumber)
                    }
                  }
                case None => "Do nothing"
              }
            } catch {
              case e: MalFormedJSONPayloadException =>
                Logger.error(e.toString())
              case e: ParsingException =>
                Logger.error(e.getMessage())
              case _ =>
                Logger.error("Expecting JSON data")
            }
          case None =>
            BadRequest("Expecting URL-encoded payload")
        }
        Ok("We are done!")
      }.getOrElse {
        BadRequest("Expecting URL-encoded data")
      }
    }
    receive
  }

  def checkExpiredReviewWarning(reviewWarning: Comment): Boolean = {
    def checkDay: Boolean = { milliSecToDay(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > waitDayInterval }
    def checkHour: Boolean = { milliSecToHour(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > waitHourInterval }
    def checkMin: Boolean = { milliSecToMin(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > waitMinInterval }
    def check: Boolean = checkHour
    check
  }

  def actorCheckIssueCommentView = {
    def refreshIssueCommentView = {
      issueMap.clear
      initIssueCommentView
    }
    def checkIssueCommentView = {
      issueMap.keySet.map { key =>
        issueMap.get(key) match {
          case Some(issue) =>
            issue.getRStatus match {
              case ReviewWarning =>
                "Scan review comments"
                val latestReviewWarning = issue.commentList.filter(comment => getReviewStatus(comment.Body) == ReviewWarning).maxBy(_.getCreateTime)
                if (checkExpiredReviewWarning(latestReviewWarning)) {
                  GithubAPI.addCommentOnIssue(issue.Number, latestReviewWarning.Body) // this would trigger an issue update to delete the expired warning
                  Logger.debug("Actor adds new review warning comment")
                }
              case _ => "Do nothing"
            }
          case None => "Do nothing"
        }
      }
    }
    refreshIssueCommentView
    checkIssueCommentView
  }

  def getCommentsOnIssue(issueNumber: Long): List[Comment] = {
    val commentString = GithubAPI.getCommentsOnIssue(issueNumber)
    val comments = com.codahale.jerkson.Json.parse[List[GithubAPIComment]](commentString)
    comments.map { comment =>
      new Comment(CommentCreated, comment.id, comment.body, parseISO8601(comment.created_at), parseISO8601(comment.updated_at), comment.user.login)
    }
  }

  def checkIssueCommentView(issueNumber: Long): (BuildState, TestState, ReviewStatus, List[Label], List[Review], List[Comment], List[NewComment]) = {
    var labelList = new ListBuffer[String]()
    var newCommentList = new ListBuffer[NewComment]()
    var buildCommentList = new ListBuffer[Comment]()
    var testCommentList = new ListBuffer[Comment]()
    var reviewCommentList = new ListBuffer[Comment]()
    var bState: BuildState = BuildNone
    var tState: TestState = TestNone
    var rStatus: ReviewStatus = ReviewNone
    var commentList = new ListBuffer[Comment]()
    var reviewList = new ListBuffer[Review]()
    var rOpenList = new ListBuffer[Comment]()
    var rWarnList = new ListBuffer[Comment]()
    var bSuccList = new ListBuffer[Comment]()
    var tSuccList = new ListBuffer[Comment]()
    var reviewWarning = ""

    def scanComments = {
      commentList.++=(getCommentsOnIssue(issueNumber).sortWith((a, b) => a.getCreateTime < b.getCreateTime))
      commentList.map { comment =>
        processComment(comment.Body) match {
          case (bstate, tstate, rstatus) =>
            // get comment type
            getCommentType(bstate, tstate, rstatus) match {
              case BuildComment => buildCommentList.+=(comment)
              case TestComment => testCommentList.+=(comment)
              case ReviewComment => reviewCommentList.+=(comment)
              case UnknownComment => "Do nothing"
            }
            // update build state
            bstate match {
              case BuildSuccess =>
                "Check test success, and save comment id for redundancy check";
                tState match {
                  case TestSuccess =>
                    "Add the tested label"
                    labelList.+=("tested")
                  case _ => "Do nothing"
                }
                bSuccList.+=(comment)
                bState = bstate
                Logger.debug("Build success")
              case BuildFailure =>
                "Delete the tested label";
                labelList.-=("tested")
                bState = bstate
                Logger.debug("Build failure")
              case _ => "Do nothing"
            }
            // update test state
            tstate match {
              case TestSuccess =>
                "Check build success, and save comment id for redundancy check"
                bState match {
                  case BuildSuccess =>
                    "Add the tested label"
                    labelList.+=("tested")
                  case _ => "Do nothing"
                }
                tSuccList.+=(comment)
                tState = tstate
                Logger.debug("Test success")
              case TestFailure =>
                "Delete the tested label";
                labelList.-=("tested")
                tState = tstate
                Logger.debug("Test failure")
              case _ => "Do nothing"
            }
            // update review status
            rstatus match {
              case ReviewOpen =>
                "Delete the reviewed label, and save comment id for redundancy check"
                labelList.-=("reviewed")
                reviewList = specifiedReviewer.map { reviewer => new Review(reviewer, ReviewOpen) }
                rOpenList.+=(comment)
                rStatus = rstatus
              case ReviewFault =>
                "Add illegal reviewer comment"
                var reviewers = ""
                specifiedReviewer.filter { reviewer => !reviewerList.contains(reviewer) }.map { reviewer => reviewers += reviewer.drop(1) + " " }
                reviewWarning = "Web Bot: unrecognized reviewers by @" + comment.User + " : " + reviewers
                rStatus = rstatus
              case ReviewDone =>
                "Add the reviewed label"
                reviewList = reviewList.map { review => if (review.getReviewer == comment.User) { review.setRStatus(ReviewDone); review } else review }
                if (reviewList.forall { review => review.getRStatus == ReviewDone }) {
                  // wait until all reviewers add LGTM comments 
                  labelList.+=("reviewed")
                  rStatus = rstatus
                }
              case ReviewWarning =>
                "Save comment id for redundancy check"
                Logger.debug("Review warning found")
                rWarnList.+=(comment)
                rStatus = rstatus
              case _ => "Do nothing"
            }
        }
      }
    }

    def addComments = {
      rStatus match {
        case ReviewFault =>
          "add review warning comment"
          newCommentList.+=(new Comment(CommentCreated, -1, reviewWarning, null, null, ""))
        case _ => "Do nothing"
      }
    }

    def deleteComments = {
      rStatus match {
        case ReviewOpen =>
          "Delete all previous review comments"
          val latestOpenReview = rOpenList.maxBy(_.getCreateTime)
          reviewCommentList.filter { _.Id != latestOpenReview.Id }.map { comment =>
            newCommentList.+=(new Comment(CommentDeleted, comment.Id, "", null, null, ""))
          }
        case ReviewWarning =>
          "Delete the expired warning"
          rWarnList.map { comment =>
            if (checkExpiredReviewWarning(comment))
              newCommentList.+=(new Comment(CommentDeleted, comment.Id, "", null, null, ""))
          }
        case _ => "Do nothing"
      }
      (bState, tState) match {
        case (BuildSuccess, TestSuccess) =>
          "Delete old build/test messages, only keep the latest build/test messages"
          val latestBuildSucc = bSuccList.maxBy(_.getCreateTime)
          buildCommentList.filter { _.Id != latestBuildSucc.Id }.map { comment =>
            newCommentList.+=(new Comment(CommentDeleted, comment.Id, "", null, null, ""))
          }
          val latestTestSucc = tSuccList.maxBy(_.getCreateTime)
          testCommentList.filter { _.Id != latestTestSucc.Id }.map { comment =>
            newCommentList.+=(new Comment(CommentDeleted, comment.Id, "", null, null, ""))
          }
        case _ => "Do nothing"
      }
    }

    scanComments
    addComments
    deleteComments
    (bState, tState, rStatus, labelList.toList, reviewList.toList, commentList.toList, newCommentList.toList)
  }

  def processComment(msg: String): (BuildState, TestState, ReviewStatus) = {
    (getBuildState(msg), getTestState(msg), getReviewStatus(msg))
  }

  def getCommentType(bState: BuildState, tState: TestState, rStatus: ReviewStatus): CommentType = {
    bState match {
      case BuildNone => "Not build comment"
      case _ => return BuildComment
    }
    tState match {
      case TestNone => "Not test comment"
      case _ => return TestComment
    }
    rStatus match {
      case ReviewNone => "Not review comment"
      case _ => return ReviewComment
    }
    UnknownComment
  }

  def printIssueMap = {
    for (key <- issueMap.keySet) {
      issueMap.get(key) match {
        case Some(issue) => Logger.debug(issue.toString())
        case None => "Do nothing"
      }
    }
  }

  def getReviewStatus(msg: String): ReviewStatus = {
    if (coordBotMsg(msg)) {
      val reviewWarning = "unrecognized reviewers"
      if (msg.contains(reviewWarning)) {
        return ReviewWarning
      }
      return ReviewNone
    }
    if (msg.contains("@") && reviewMsg(msg)) {
      var tokens = new ListBuffer[String]
      tokens.++=(msg.split(" "))
      specifiedReviewer = tokens.filter(token => token.contains("@")) // assume all reviewers are specified in one comment
      var reviewers = ""
      specifiedReviewer.map(reviewer => reviewers += reviewer + " ")
      Logger.debug("Specified reviewers: " + reviewers)
      for (token <- specifiedReviewer; if !reviewerList.contains(token))
        return ReviewFault
      ReviewOpen
    } else if (reviewedMsg(msg))
      ReviewDone
    else
      ReviewNone
  }

  def reviewMsg(msg: String): Boolean = {
    for (token <- reviewMsgList; if (msg.contains(token)))
      return true
    false
  }

  def reviewedMsg(msg: String): Boolean = {
    for (token <- reviewedMsgList; if (msg.contains(token)))
      return true
    false
  }

  def JIRATickets(title: String): Array[String] = {
    val tokens = title.split(" ")
    tokens.filter(token => token.contains("SI-"))
  }

  def missingJIRALinks(body: String, tickets: Array[String]): Array[String] = {
    val links = for (ticket <- tickets; if (!body.contains(ticket)))
      yield "[" + ticket + "]" + "(https://issues.scala-lang.org/browse/" + ticket + ")"
    links
  }

  def coordBotMsg(msg: String): Boolean = {
    return msg.contains("Web Bot:")
  }

  def getBuildState(msg: String): BuildState = {
    if (coordBotMsg(msg)) {
      Logger.debug("Ignore web bot message")
      return BuildNone
    }
    val botMsg = "jenkins job"
    val buildMsg = "pr-rangepos"
    val startMsg = "Started"
    val successMsg = "Success"
    if (msg.contains(botMsg)) {
      if (msg.contains(buildMsg)) {
        if (msg.contains(startMsg))
          BuildStart
        else if (msg.contains(successMsg))
          BuildSuccess
        else
          BuildFailure
      } else
        BuildNone
    } else
      BuildNone
  }

  def getTestState(msg: String): TestState = {
    if (coordBotMsg(msg)) {
      Logger.debug("Ignore web bot message")
      return TestNone
    }
    val botMsg = "jenkins job"
    val testMsg = "pr-scala-testsuite-linux-opt"
    val startMsg = "Started"
    val successMsg = "Success"
    if (msg.contains(botMsg)) {
      if (msg.contains(testMsg)) {
        if (msg.contains(startMsg))
          TestStart
        else if (msg.contains(successMsg))
          TestSuccess
        else
          TestFailure
      } else
        TestNone
    } else
      TestNone
  }

  def updateIssueCommentView(issueNumber: Long) = {
    issueMap.get(issueNumber) match {
      case Some(issue) =>
        // update issue view
        val issueString = GithubAPI.getIssue(issueNumber)
        val issueJson = Json.parse(issueString)
        (issueJson \ "number").asOpt[Long] match {
          case Some(number) => "Do nothing"
          case None => throw new MalFormedJSONPayloadException("Missing issue number")
        }
        (issueJson \ "title").asOpt[String] match {
          case Some(title) => issue.updateTitle(title)
          case None => throw new MalFormedJSONPayloadException("Missing issue title")
        }
        (issueJson \ "body").asOpt[String] match {
          case Some(body) => issue.updateBody(body)
          case None => throw new MalFormedJSONPayloadException("Missing issue body")
        }
        // update comment view
        checkIssueCommentView(issueNumber) match {
          case (bstate, tstate, rstatus, labelList, reviewList, commentList, newCommentList) =>
            issue.updateBState(bstate)
            issue.updateTState(tstate)
            issue.updateRStatus(rstatus)
            issue.labels.--=(List("tested", "reviewed"))
            issue.labels.++=(labelList)
            updateLabelsOnIssue(issueNumber, labelList)
            issue.reviewList.clear
            issue.reviewList.++=(reviewList)
            issue.commentList.clear
            issue.commentList.++=(commentList)
            updateCommentsOnIssue(issueNumber, newCommentList)
            Logger.debug(issue.toString())
        }
      case None => "Do nothing"
    }
  }

  def initIssueCommentView = {
    val issueString = GithubAPI.getOpenIssues
    val issues = com.codahale.jerkson.Json.parse[List[GithubAPIIssue]](issueString)
    for (issue <- issues) {
      var body = ""
      if (missingJIRALinks(issue.body, JIRATickets(issue.title)).map { link => body += "\n" + link }.size > 0) {
        Logger.debug("Add JIRA links to the body of issue " + issue.number)
        GithubAPI.editIssueBody(issue.number, issue.body + body)
      }
      var newIssue = new Issue(issue.number, issue.title, issue.body + body)
      if (issue.comments > 0 && issue.number > 0) {
        Logger.debug("Found comments on issue " + issue.number.toString())
        checkIssueCommentView(issue.number) match {
          case (bstate, tstate, rstatus, labelList, reviewList, commentList, newCommentList) =>
            newIssue.updateBState(bstate)
            newIssue.updateTState(tstate)
            newIssue.updateRStatus(rstatus)
            updateLabelsOnIssue(issue.number, labelList)
            newIssue.labels.++=(labelList)
            newIssue.reviewList.++=(reviewList)
            newIssue.commentList.++=(commentList)
            updateCommentsOnIssue(issue.number, newCommentList)
        }
      }
      issueMap = issueMap.+((issue.number, newIssue))
    }
    printIssueMap
  }

  def updateLabelsOnIssue(issueNumber: Long, labelList: List[Label]) = {
    val existedLabelsSet = getLabelsOnIssue(issueNumber).toSet
    val newLabelsSet = labelList.toSet
    val labelsToAdd = newLabelsSet.diff(existedLabelsSet)
    val labelsToDelete = existedLabelsSet.diff(newLabelsSet)
    deleteLabelsOnIssue(issueNumber, labelsToDelete.toList)
    addLabelsOnIssue(issueNumber, labelsToAdd.toList)
  }

  def updateCommentsOnIssue(issueNumber: Long, comments: List[NewComment]) = {
    for (comment <- comments) {
      comment.Action match {
        case CommentCreated =>
          GithubAPI.addCommentOnIssue(issueNumber, comment.Body)
        case CommentDeleted =>
          Logger.info("Delete comment " + comment.Id.toString())
          GithubAPI.deleteCommentOnIssue(comment.Id)
      }
    }
  }

  def getLabels = {
    val labelString = GithubAPI.getLabels
    val labels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    for (label <- labels) {
      totalLabelList.+=(label.name)
    }
  }

  def missingLabels: List[String] = {
    val missingLabels = for (label <- defaultLabelList; if (!totalLabelList.contains(label)))
      yield label
    missingLabels
  }

  def addLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    // in case adding an already existed issue would produce a HTTP error
    val labels = githublabels.map { githublabel => githublabel.name }
    if (!labels.contains(label))
      GithubAPI.addLabelOnIssue(issueNumber, label)
  }

  def addLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels)
      addLabelOnIssue(issueNumber, label)
  }

  def getLabelsOnIssue(issueNumber: Long): List[Label] = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    val labels = githublabels.map { githublabel => githublabel.name }
    labels
  }

  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    // in case adding an already existed issue would produce a HTTP error
    val labels = githublabels.map { githublabel => githublabel.name }
    if (labels.contains(label)) {
      Logger.debug("Call GithubAPi to delete label " + label)
      GithubAPI.deleteLabelOnIssue(issueNumber, label)
    }
  }

  def deleteLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) {
      Logger.debug("Delete label " + label)
      deleteLabelOnIssue(issueNumber, label)
    }
  }

  def getAllHooks: List[GithubAPIHook] = {
    val hookString = GithubAPI.getHooks
    val githubhooks = com.codahale.jerkson.Json.parse[List[GithubAPIHook]](hookString).sortWith((a, b) => a.id < b.id)
    githubhooks
  }

  def deleteAllHooks = {
    getAllHooks.map { hook => GithubAPI.deleteHook(hook.id) }
  }

  def setupEnv = {
    GithubAPI.initParameters(gitHubUser, gitHubPassword, gitHubRepo, gitHubUrl, hookUrl)
    Logger.info("Github Webook parameters: " + "\nUser: " + gitHubUser + "\nRepository: " + gitHubRepo + "\nHook url: " + hookUrl)
    getLabels
    Logger.debug("Repo labels: " + totalLabelList.mkString(" "))
    val missinglabels = missingLabels
    if (missinglabels.size != 0) {
      throw new MissingDefaultLabelsException("Default label(s) missing: " + missinglabels.mkString(" "))
    }
  }

  def setupGithubEnv = {
    setupEnv
    deleteAllHooks
    GithubAPI.setupAllRepoHooks
    coordBotInit = false
  }
}
