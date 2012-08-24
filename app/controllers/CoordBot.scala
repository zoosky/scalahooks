package controllers

import java.util.Date
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList
import scala.collection.mutable.Map
import org.joda.time.format.ISODateTimeFormat
import github.GithubAPI
import models._
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.Logger
import play.api.libs.concurrent.{Redeemed, Thrown}
import play.api.libs.concurrent.Akka
import play.api.Play.current
import models.CommentAction
import models.CommentCreated
import models.ReviewStatus
import models.ReviewNone
import scala.collection.mutable.ListBuffer
import scala.actors.threadpool.TimeoutException
import play.api.mvc.Result
import play.api.libs.concurrent.Akka
import akka.actor.Props
import akka.util.duration._

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
  var specifiedReviewer = new ListBuffer[String]
  var issueAction, issueTitle, issueBody = "" 
  var totalLabelList = new ListBuffer[String]()
               
  val waitInterval = 12 * 48 // 48 hours = (12 * 5) * 48 minutes
  val updateFrequency = 5 minutes
  val initialDelay = 1 minute
  val enableActor = false
  
  val coordActor = Akka.system.actorOf(Props[CoordActor])
  if (enableActor)
    Akka.system.scheduler.schedule(initialDelay, updateFrequency, coordActor, "refresh")
    
  /**
   * The main page
   * */
  
  def index() = Action { implicit request =>
      val issues = issueMap.values.toList
      handleTimeout(e => "Timeout when reading list of refreshing commits: "+ e.toString) {
        // setup web hooks
        Logger.info("Setting up web hooks...")
        CoordBot.setupGithubHooks
        // initialize issue-comment view
        initIssueCommentView
        // show open issues
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
   * */
  
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
            case ex: MalFormedJSONPayloadException =>
              Logger.error(ex.toString())
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
  
  def actorCheckIssueCommentView() = {
    issueMap.keySet.map { key =>
      issueMap.get(key) match {
        case Some(issue)  => issue.getRStatus match {
          case ReviewOpen => "Check counter" 
            issue.rCounter -= 1 
            if (issue.rCounter < 0) {
              var names = ""
              issue.reviewList.map {review => if (review.getRStatus != ReviewDone) names += review.getReviewer + " "}
              val commentString = "Web Bot: still waiting for the reviews " + names
              GithubAPI.addCommentOnIssue(issue.Number, commentString)
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
    val comments = com.codahale.jerkson.Json.parse[List[String]](commentString)
    comments.map { comment =>
      var cmtId: Long = -1
      var cmtBody, cmtCreateTime, cmtUpdateTime, cmtUserLogin = ""
      // parse a new comment
      val commentJson = Json.parse(comment)
      (commentJson \ "id").asOpt[Long] match {
        case Some(id) => cmtId = id
        case None     => throw new MalFormedJSONPayloadException("Missing comment id")
      }
      (commentJson \ "body").asOpt[String] match {
        case Some(body) => cmtBody = body
        case None       => throw new MalFormedJSONPayloadException("Missing comment body")
      }
      (commentJson \ "created_at").asOpt[String] match {
        case Some(createTime) => cmtCreateTime = createTime
        case None             => throw new MalFormedJSONPayloadException("Missing comment created_at time stamp")
      }
      (commentJson \ "updated_at").asOpt[String] match {
        case Some(updateTime) => cmtUpdateTime = updateTime
        case None             => throw new MalFormedJSONPayloadException("Missing comment updated_at time stamp")
      }
      (commentJson \ "user").asOpt[String] match {
        case Some(user) => val userJson = Json.parse(user)
          (userJson \ "login").asOpt[String] match {
            case Some(login) => cmtUserLogin = login
            case None        => throw new MalFormedJSONPayloadException("Missing user login")
          }
        case None       => throw new MalFormedJSONPayloadException("Missing comment user")
      }
      new Comment(CommentCreated, cmtId, cmtBody, cmtCreateTime, cmtUpdateTime, cmtUserLogin)
    } 
  }
  
  def checkIssueCommentView(issueNumber: Long): (BuildBotState, ReviewStatus, List[Label], List[Review], List[Comment], List[NewComment]) = {
    var labelList = new ListBuffer[String]()
    var newCommentList = new ListBuffer[NewComment]()
    var bState: BuildBotState = BuildNone
    var rStatus: ReviewStatus = ReviewNone
    var commentList = new ListBuffer[Comment]() 
    var reviewList = new ListBuffer[Review]()
    commentList.++=(getCommentsOnIssue(issueNumber))
    commentList.map {comment =>
       processComment(comment.Body) match {
         case (bstate, rstatus) => 
           // update buildbot state
           var bSuccess, tSuccess = false
           bstate match {
             case BuildSuccess         => "Wait for test success";
               bSuccess = true
               bState = bstate
             case BuildFailure         => "Remove the tested label";
               bSuccess = false
               labelList.-=("tested")
               bState = bstate
             case TestSuccess          => "Check build success, and add the tested label"
               tSuccess = true
               if (bSuccess)
                 labelList.+=("tested")
               bState = bstate
             case TestFailure          => "Remove the tested label";
               tSuccess = false
               labelList = labelList.filter(label => label != "tested")
               bState = bstate
             case _                    => "Do nothing"
           }
           // update review status
           rstatus match {
             case ReviewOpen           => "Remove the reviewed label"
               labelList.-=("reviewed")
               reviewList = specifiedReviewer.map {reviewer => new Review(reviewer, ReviewOpen)}
               rStatus = rstatus
             case ReviewFault          => "Add illegal reviewer comment"
               var names = ""
               specifiedReviewer.map {reviewer => names += reviewer.drop(1) + " "}
               val newcomment = "Web Bot: unrecognized reviewers by @" + comment.User + " : " + names
               newCommentList.+=(new Comment(CommentCreated, -1, newcomment, "", "", ""))
               rStatus = rstatus
             case ReviewDone           => "Add the reviewed label"
               labelList.+=("reviewed")
               reviewList = reviewList.map {review => if (review.getReviewer == comment.User) {review.setRStatus(ReviewDone); review} else review}
               if ((reviewList.filter {review => review.getRStatus != ReviewDone}).size == 0)
                 rStatus = rstatus // wait until all reviewers add LGTM comments 
             case _                    => "Do nothing"
           }
       } 
    } 
    (bState, rStatus, labelList.asInstanceOf[List[Label]], reviewList.asInstanceOf[List[Review]], commentList.asInstanceOf[List[Comment]], newCommentList.asInstanceOf[List[NewComment]])
  } 
  
  def processComment(msg: String): (BuildBotState, ReviewStatus) = {
    (getBuildBotState(msg), getReviewStatus(msg))
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
      var str = ""
      specifiedReviewer.map(r => str += r + " ") 
      Logger.debug("Specified reviewers: " + str)
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
  
  def getBuildBotState(msg: String): BuildBotState = {
    if (coordBotComment(msg))
      return BuildNone
    val botMsg = "jenkins job"
    val buildMsg = "pr-rangepos"
    val testMsg = "pr-scala-testsuite-linux-opt"
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
      else if (msg.contains(testMsg)) {
        if (msg.contains(startMsg))
          TestStart
        else if (msg.contains(successMsg))
          TestSuccess
        else
          TestFailure  
      }
      else
        BuildNone
    }
    else 
      BuildNone
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
          case (bstate, rstatus, labelList, reviewList, commentList, newCommentList) => 
            issue.updateBState(bstate)
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
    try {
      val issues = com.codahale.jerkson.Json.parse[List[String]](issueString) 
      issueMap.clear()
      for (issue <- issues) {
        var iseNumber: Long = -1
        var iseTitle, iseBody = ""
        // parse a new issue
        val issueJson = Json.parse(issue)
        (issueJson \ "number").asOpt[Long] match {
          case Some(number) => iseNumber = number
          case None => throw new MalFormedJSONPayloadException("Missing issue number")
        }
        (issueJson \ "title").asOpt[String] match {
          case Some(title) => iseTitle = title
          case None => throw new MalFormedJSONPayloadException("Missing issue title")
        }
        (issueJson \ "body").asOpt[String] match {
          case Some(body) => iseBody = body
          case None => throw new MalFormedJSONPayloadException("Missing issue body")
        }
        var newIssue = new Issue(iseNumber, issueTitle, issueBody)
        (issueJson \ "comment").asOpt[Long] match {
          case Some(nOfcomment) =>
            if (nOfcomment > 0 && iseNumber > 0) {
              checkIssueCommentView(iseNumber) match {
                case (bstate, rstatus, labelList, reviewList, commentList, newCommentList) =>
                  newIssue.updateBState(bstate)
                  newIssue.updateRStatus(rstatus)
                  removeLabelsOnIssue(iseNumber, List("tested", "reviewed"))
                  addLabelsOnIssue(iseNumber, labelList)
                  newIssue.labels.++=(getLabelsOnIssue(iseNumber))
                  newIssue.reviewList.++=(reviewList)
                  newIssue.commentList.++=(commentList)
                  updateCommentsOnIssue(iseNumber, newCommentList)
              }
            }
          case None => throw new MalFormedJSONPayloadException("Missing comment field")
        }
        issueMap = issueMap.+((iseNumber, newIssue))
      }
      printIssueMap
    }
    catch {
      case ex: MalFormedJSONPayloadException =>
        Logger.error(ex.toString())
      case _ =>
        Logger.error("Expecting JSON data")
    }
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
    try {
      val labels = com.codahale.jerkson.Json.parse[List[String]](labelString)
      for (label <- labels) { 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => totalLabelList.+=(name)
          case None => throw new MalFormedJSONPayloadException("Missing name field")
        } 
      }
    }
    catch {
      case ex: MalFormedJSONPayloadException =>
        Logger.error(ex.toString())
      case _ =>
        Logger.error("Expecting JSON data")
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
  
  def defaultLabelsAvailable: Boolean = {
    totalLabelList.contains("tested") && totalLabelList.contains("reviewed")
  }
  
  def labelAvailable(label: String): Boolean = {
    totalLabelList.contains(label) 
  }
  
  def addLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    try {
      val labels = com.codahale.jerkson.Json.parse[List[String]](labelString)
      var labelList = new ListBuffer[String]()
      for (label <- labels) { 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => labelList.+=(name)
          case None => throw new MalFormedJSONPayloadException("Missing name field")
        } 
      }
      // in case adding an already existed issue would produce a http error
      if (!labelList.contains(label))
        GithubAPI.addLabelOnIssue(issueNumber, label)
      else {
        var str = "Label " + label + " already exists: "
        labelList.map (label => str += " " + label)
        Logger.info(str)
      }
    }
    catch {
      case ex: MalFormedJSONPayloadException =>
        Logger.error(ex.toString())
      case _ =>
        Logger.error("Expecting JSON data")
    }
  }
  
  def addLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) 
      addLabelOnIssue(issueNumber, label)
  }
  
  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    GithubAPI.deleteLabelOnIssue(issueNumber, label)
  }
  
  def removeLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) 
      deleteLabelOnIssue(issueNumber, label)
  }
  
  def setupEnv = {
    GithubAPI.initParameters(gitHubUser, gitHubPassword, gitHubRepo, gitHubUrl, hookUrl)
    Logger.info("Github Webook parameters: " + "\nUser: " + gitHubUser + "\nRepository: " + gitHubRepo + "\nHook url: " + hookUrl)
    getLabels
    var str = ""
    totalLabelList.map {label => str += " " + label}
    Logger.info("Repo labels: " + str)
    if (!defaultLabelsAvailable)
      Logger.error("Default labels missing?!")
  }
  
  def setupGithubHooks = {
    setupEnv
    GithubAPI.setupRepoHooks
  }
}
