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
import play.api.libs.concurrent.{Redeemed, Thrown}
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

/**
 * */

object CoordBot extends Controller {
  type Label = String; type NewComment = Comment
  
  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  //val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/"+gitHubUser+"/"+gitHubRepo
  val hookUrl = "http://requestb.in/1imhe4b1"
  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue]()
  val reviewerList = List("@taolee", "@adriaan", "@odersky", "@lukas", "@heather", "@vlad")
  val reviewMsgList = List("Review", "review", "REVIEW")
  val reviewedMsgList = List("LGTM", "lgtm")
  val defaultLabelList = List("tested", "reviewed")
  var specifiedReviewer = new ListBuffer[String]
  var issueAction, issueTitle, issueBody = "" 
  var totalLabelList = new ListBuffer[String]()
               
  val waitInterval = 12 * 48 // 48 hours = (12 * 5) * 48 minutes
  val updateFrequency = 5 minutes
  val initialDelay = 1 minute
  val enableActor = false
  
  val coordActor = Akka.system.actorOf(Props[CoordActor])
  if (enableActor) {
    Akka.system.scheduler.schedule(initialDelay, updateFrequency, coordActor, "refresh")
    Logger.info("Coordination Actor is activated")
  }
    
  /**
   * The main page
   */
  
  def index() = Action { implicit request =>
    handleTimeout(e => "Timeout when reading list of open issues "+ e.toString) {
      try {
        // setup web hooks
        CoordBot.setupGithubHooks
        // initialize issue-comment view
        issueMap.clear
        initIssueCommentView
      }
      catch {
        case e: MalFormedJSONPayloadException =>
          Logger.error(e.toString())
        case e: MissingDefaultLabelsException =>
          Logger.error(e.toString())
        case _ =>
          Logger.error("Expecting JSON data")
      }
      // show open issues
      val issues = issueMap.values.toList.sortWith((a, b) => a.Number < b.Number)
      Ok(views.html.index2(issues))
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
  
  /**
   * The coordination bot methods
   */
  
  def receiveGithubMsg = Action { msg =>
    Logger.info("Receive message: " + msg.body.toString())
    msg.body.asFormUrlEncoded.map { urlenc =>
      val urlencPayload = (urlenc.get("payload")) match {
        case Some(jsonStringSeq) => val jsonString = jsonStringSeq.head; 
          Logger.info("Receive JSON payload: " + jsonString)
          try {
            val json = Json.parse(jsonString)
            var issueNumber: Long = -1
            var issueAction, issueTitle, issueBody = ""
            // action
            (json \ "action").asOpt[String] match {  
              case Some(action)   => issueAction = action  
                // issue
                (json \ "issue").asOpt[String] match {
                  case Some(issue) => val issueJson = Json.parse(issue)
                    (issueJson \ "number").asOpt[Long] match { 
                      case Some(number) => issueNumber = number
                      case None         => throw new MalFormedJSONPayloadException("Missing issue number")
                    }
                    (issueJson \ "title").asOpt[String] match {
                      case Some(title)  => issueTitle = title
                      case None         => throw new MalFormedJSONPayloadException("Illegal issue title")
                    }
                    (issueJson \ "body").asOpt[String] match { 
                      case Some(body)   => issueBody = body
                      case None         => throw new MalFormedJSONPayloadException("Illegal issue body")
                    }
                    (json \ "comment").asOpt[String] match {
                      case Some(comment) => "update issue-comment view"
                        updateIssueCommentView(issueNumber)
                      case None => 
                        if (issueAction == "opened" || issueAction == "reopened") {
                          Logger.info("An issue is " + issueAction)
                          Logger.info("Issue title: " + issueTitle)
                          Logger.info("Issue body: " + issueBody)
                          issueMap = issueMap.+((issueNumber, new Issue(issueNumber, issueTitle, issueBody)))
                        }
                        else if (issueAction == "closed") { // issue closed
                          issueMap = issueMap.-(issueNumber)
                        }
                        else 
                          throw new MalFormedJSONPayloadException("Illegal issue action: " + issueAction) 
                    }
                  case None => "Do nothing"
                }
              case None     => "Do nothing"
            }
          }
          catch {
            case e: MalFormedJSONPayloadException =>
              Logger.error(e.toString())
            case _ =>
              Logger.error("Expecting JSON data")
          }
        case None => 
          BadRequest("Expecting URL-encoded payload")
      }
      Ok("We are done")
    }.getOrElse {
      BadRequest("Expecting URL-encoded data")
    }
  }
  
  def actorCheckIssueCommentView = {
    issueMap.keySet.map { key =>
      issueMap.get(key) match {
        case Some(issue)  => issue.getRStatus match {
          case ReviewOpen => "Check counter" 
            issue.rCounter -= 1 
            if (issue.rCounter < 0) {
              var names = ""
              issue.reviewList.map {review => if (review.getRStatus != ReviewDone) names += review.getReviewer + " "} // get the names of the reviewers
              val commentString = "Web Bot: still waiting for the reviews " + names
              GithubAPI.addCommentOnIssue(issue.Number, commentString)
              Logger.info("Actor sends review expired warning at " + Calendar.getInstance().getTime)
              issue.rCounter = waitInterval
            }  
            else  
                             "Do nothing"
          case _          => "Do nothing"
        }
        case None         => "Do nothing"
      }
    }
  }
  
  def getCommentsOnIssue(issueNumber: Long): List[Comment] = {
    val commentString = GithubAPI.getCommentsOnIssue(issueNumber)
    val comments = com.codahale.jerkson.Json.parse[List[GithubAPIComment]](commentString)
    comments.map { comment =>
      new Comment(CommentCreated, comment.id, comment.body, comment.created_at, comment.updated_at, comment.user.get("login").toString()) // FIXME: why cannot use comment.user.login?
    } 
  }
  
  def checkIssueCommentView(issueNumber: Long): (BuildState, TestState, ReviewStatus, List[Label], List[Review], List[Comment], List[NewComment]) = {
    var labelList = new ListBuffer[String]()
    var newCommentList = new ListBuffer[NewComment]()
    var bState: BuildState = BuildNone
    var tState: TestState = TestNone
    var rStatus: ReviewStatus = ReviewNone
    var commentList = new ListBuffer[Comment]() 
    var reviewList = new ListBuffer[Review]()
    commentList.++=(getCommentsOnIssue(issueNumber).sortWith((a, b) => a.Id < b.Id)) // FIXME: should be sorted by time
    commentList.map {comment =>
       processComment(comment.Body) match {
         case (bstate, tstate, rstatus) => 
           // update build state
           bstate match {
             case BuildSuccess         => "Check test success";
               tState match {
                 case TestSuccess      => "Add the tested label"
                   labelList.+=("tested")
                 case _                => "Do nothing"
               }
               bState = bstate
               Logger.debug("Build success")
             case BuildFailure         => "Remove the tested label";
               labelList.-=("tested")
               bState = bstate
               Logger.debug("Build failure")
             case _                    => "Do nothing"
           }
           // update test state
           tstate match {
             case TestSuccess          => "Check build success"
               bState match {
                 case BuildSuccess => "Add the tested label"
                   labelList.+=("tested")
                 case _            => "Do nothing"
               }
               tState = tstate
               Logger.debug("Test success")
             case TestFailure          => "Remove the tested label";
               //labelList = labelList.filter(label => label != "tested")
               tState = tstate
               Logger.debug("Test failure")
             case _                    => "Do nothing"
           }
           // update review status
           rstatus match {
             case ReviewOpen           => "Remove the reviewed label"
               labelList.-=("reviewed")
               reviewList = specifiedReviewer.map {reviewer => new Review(reviewer, ReviewOpen)}
               rStatus = rstatus
             case ReviewFault          => "Add illegal reviewer comment"
               var reviewers = ""
               specifiedReviewer.filter{reviewer => !reviewerList.contains(reviewer)}.map {reviewer => reviewers += reviewer.drop(1) + " "}
               val newcomment = "Web Bot: unrecognized reviewers by @" + comment.User + " : " + reviewers
               newCommentList.+=(new Comment(CommentCreated, -1, newcomment, "", "", ""))
               rStatus = rstatus
             case ReviewDone           => "Add the reviewed label"
               reviewList = reviewList.map {review => if (review.getReviewer == comment.User) {review.setRStatus(ReviewDone); review} else review}
               if (reviewList.forall {review => review.getRStatus == ReviewDone}) {
                 // wait until all reviewers add LGTM comments 
                 labelList.+=("reviewed")
                 rStatus = rstatus 
               }
             case _                    => "Do nothing"
           }
       } 
    } 
    (bState, tState, rStatus, labelList.toList, reviewList.toList, commentList.toList, newCommentList.toList)
  } 
  
  def processComment(msg: String): (BuildState, TestState, ReviewStatus) = {
    (getBuildState(msg), getTestState(msg), getReviewStatus(msg))
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
    if (coordBotComment(msg))
      return ReviewNone
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
    }
    else if (reviewedMsg(msg))
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
  
  def coordBotComment(msg: String): Boolean = {
    return msg.contains("Web Bot:")
  }
  
  def getBuildState(msg: String): BuildState = {
    if (coordBotComment(msg))
      return BuildNone
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
      }
      else
        BuildNone
    }
    else 
      BuildNone
  }
  
  def getTestState(msg: String): TestState = {
    if (coordBotComment(msg))
      return TestNone
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
      }
      else
        TestNone
    }
    else 
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
          case None         => throw new MalFormedJSONPayloadException("Missing issue number")
        }  
        (issueJson \ "title").asOpt[String] match {
          case Some(title)  => issue.updateTitle(title)
          case None         => throw new MalFormedJSONPayloadException("Missing issue title")
        }  
        (issueJson \ "body").asOpt[String] match {
          case Some(body)   => issue.updateBody(body)
          case None         => throw new MalFormedJSONPayloadException("Missing issue body")
        }  
        // update comment view
        checkIssueCommentView(issueNumber) match {
          case (bstate, tstate, rstatus, labelList, reviewList, commentList, newCommentList) => 
            issue.updateBState(bstate)
            issue.updateTState(tstate)
            issue.updateRStatus(rstatus)
            issue.labels.--=(List("tested", "reviewed"))
            issue.labels.++=(labelList)
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
      var newIssue = new Issue(issue.number, issue.title, issue.body)
      deleteLabelsOnIssue(issue.number, List("tested", "reviewed")) // clean the labels
      if (issue.comments > 0 && issue.number > 0) {
        Logger.debug("Found comments on issue " + issue.number.toString()) 
        checkIssueCommentView(issue.number) match {
          case (bstate, tstate, rstatus, labelList, reviewList, commentList, newCommentList) =>
            newIssue.updateBState(bstate)
            newIssue.updateTState(tstate)
            newIssue.updateRStatus(rstatus)
            addLabelsOnIssue(issue.number, labelList)
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
  
  def updateCommentsOnIssue(issueNumber: Long, comments: List[NewComment]) = {
    for (comment <- comments) {
      comment.Action match {
        case CommentCreated =>
          GithubAPI.addCommentOnIssue(issueNumber, comment.Body)
        case CommentDeleted => 
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
  
  def getLabelsOnIssue(issueNumber: Long): List[Label] =  {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val labels = com.codahale.jerkson.Json.parse[List[String]](labelString)
      labels.map {label => 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => name
          case None => throw new MalFormedJSONPayloadException("Missing name field")
        } 
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
    val labels = githublabels.map {githublabel => githublabel.name}
    if (!labels.contains(label))
      GithubAPI.addLabelOnIssue(issueNumber, label)
  }
  
  def addLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) 
      addLabelOnIssue(issueNumber, label)
  }
  
  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    // in case adding an already existed issue would produce a HTTP error
    val labels = githublabels.map {githublabel => githublabel.name}
    if (labels.contains(label))
      GithubAPI.deleteLabelOnIssue(issueNumber, label)
  }
  
  def deleteLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) 
      deleteLabelOnIssue(issueNumber, label)
  }
  
  def setupEnv = {
    GithubAPI.initParameters(gitHubUser, gitHubPassword, gitHubRepo, gitHubUrl, hookUrl)
    Logger.info("Github Webook parameters: " + "\nUser: " + gitHubUser + "\nRepository: " + gitHubRepo + "\nHook url: " + hookUrl)
    getLabels
    var labels = ""
    totalLabelList.map {label => labels += label + " "}
    Logger.info("Repo labels: " + labels)
    val missinglabels = missingLabels
    if (missinglabels.size != 0) {
      labels = ""
      missinglabels.map {label => labels += label + " "}
      throw new MissingDefaultLabelsException("Default label(s) missing: " + labels)
    }
  }
  
  def setupGithubHooks = {
    setupEnv
    GithubAPI.setupAllRepoHooks
  }
}
