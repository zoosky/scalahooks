package controllers

import play.api.mvc.Action
import play.api.mvc.Controller
import java.util.Date
import org.joda.time.format.ISODateTimeFormat
import cc.spray.json._
import dispatch._
import Application.silentHttp
import github._
import jenkins._
import models._
import Config._
import play.api.Logger
import play.api.libs.json
import play.api.libs.json.Json
import java.util.Calendar
import com.codahale.jerkson.Json._
import scala.collection.mutable._

/**
 * */

case class MalFormedJSONPayloadException(ex: String) extends RuntimeException {
  override def toString: String = ex
}

class Comment (id: Long, body: String, createTime: String, updateTime: String) {
  def Id: Long = {id}
  def Body: String = {body}
  def CreateTime: String = {createTime}
  def UpdateTime: String = {updateTime}
}

class Issue (number: Long, title: String, body: String) {
  val minInterval = 60
  val waitInterval = minInterval * 48
  var rStatus: ReviewStatus = ReviewNone
  var rCounter: Long = -1
  var bState: BuildBotState = UnknownBuildBotState
  var commentList = new LinkedList[Comment]()
  def insertComment(comment: Comment) = {commentList = commentList.:+(comment)}
  def Number: Long = {number}
  def Title: String = {title}
  def Body: String = {body}
  def getBState: BuildBotState = {bState}
  def updateBState(state: BuildBotState) = {bState = state}
  def getRStatus: ReviewStatus = {rStatus}
  def updateRStatus(status: ReviewStatus) = {rStatus = status}
  def setRCounter(counter: Long) = {rCounter = counter}
  def checkRStatus: ReviewStatus = {
    rStatus match {
      case ReviewNone => "Do nothing";        rStatus
      case ReviewOpen => "Wait until tested"; rStatus
      case ReviewWait => rCounter -= minInterval 
        if (rCounter < 0) {
          rStatus = ReviewExpired; rStatus
        } 
        else  
          rStatus
      case ReviewExpired => rStatus 
      case ReviewDone => rStatus
    }
  }
  override def toString(): String = {
    val issueString = "Issue title: " + this.title + "\n" +
                      "Issue body: "  + this.body  + "\n"
    var commentString = (commentList.foldLeft(new Comment(-1, "", "", "")) {(x, y) => new Comment(-1, x.Body + "\n" + y.Body, "", "")}).Body
    if (commentString != "") commentString = "Comment body: " + commentString
    issueString + commentString
  }
}

sealed trait BuildBotState {
  override def toString() = BuildBotState.mapToString(this)

  def describe =
    this match {
      case BuildStart             => "build started"
      case BuildSuccess           => "build successful"
      case BuildFailure           => "build failed"
      case TestStart              => "test started"
      case TestSuccess            => "test successful"
      case TestFailure            => "test failed"
      case SameBuildBotState      => "same state"
      case UnknownBuildBotState   => "unknown state"
    }
}

object BuildBotState {
  val mapToString: Map[BuildBotState, String] = Map(
    BuildStart             -> "build started",
    BuildSuccess           -> "build successful",
    BuildFailure           -> "build failed",
    TestStart              -> "test started",
    TestSuccess            -> "test successful",
    TestFailure            -> "test failed",
    UnknownBuildBotState   -> "unknown state"
  )

  def apply(s: String): BuildBotState = mapToString.map(_.swap).apply(s)
}

case object BuildStart            extends BuildBotState 
case object BuildSuccess          extends BuildBotState
case object BuildFailure          extends BuildBotState
case object TestStart             extends BuildBotState
case object TestSuccess           extends BuildBotState
case object TestFailure           extends BuildBotState
case object SameBuildBotState     extends BuildBotState
case object UnknownBuildBotState  extends BuildBotState

sealed trait ReviewStatus {}

object ReviewStatus {}

case object ReviewNone            extends ReviewStatus
case object ReviewOpen            extends ReviewStatus
case object ReviewWait            extends ReviewStatus
case object ReviewExpired         extends ReviewStatus
case object ReviewDone            extends ReviewStatus

object CoordBot extends Controller {
  
  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  //val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/"+gitHubUser+"/"+gitHubRepo
  val hookUrl = "http://requestb.in/106k14o1"
  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue]()
  val reviewerList = List("@tao", "@adriaan", "@odersky", "@lukas", "@heather", "@vlad")
  var specifiedReviewer = new ArrayBuffer[String](1)
  var issueAction, issueTitle, issueBody = "" 
  var commentBody, commentCreateTime, commentUpdateTime, commentUserLogin = ""
  var issueNumber, commentId: Long = -1
  def resetIssueCommentFields = {issueAction = ""; issueTitle = ""; issueBody = ""; commentBody = ""; commentCreateTime = ""; commentUpdateTime = ""; commentUserLogin = ""; issueNumber = -1; commentId = -1}
  
  val dateParser = ISODateTimeFormat.dateTimeNoMillis();
  def parseISO8601(date: String): Date = {
    dateParser.parseDateTime(date).toDate()
  }
  
  def receiveGithubMsg = Action { msg =>
    Logger.info(msg.body.toString())
    msg.body.asFormUrlEncoded.map { urlenc =>
      val urlencPayload = (urlenc.get("payload")) match {
        case Some(jsonStringSeq) => val jsonString = jsonStringSeq.head; 
          Logger.info("Receive JSON payload: " + jsonString)
          resetIssueCommentFields
          val json = Json.parse(jsonString)
          // action
          (json \ "action").asOpt[String].map { action => 
            issueAction = action
          }
          // issue
          val issue = Json.parse((json \ "issue").toString()) 
          if (issue != null) {
            (issue \ "number").asOpt[Long].map {number => 
              issueNumber = number
            }
            (issue \ "title").asOpt[String].map {title => 
              issueTitle = title
            }
            (issue \ "body").asOpt[String].map {body => 
              issueBody = body
            }
          }
          // issue comment
          val comment = Json.parse((json \ "comment").toString())
          if (comment != null) {
            (comment \ "id").asOpt[Long].map {id => 
              commentId = id
            }
            (comment \ "body").asOpt[String].map {body => 
            commentBody = body
          }
          (comment \ "created_at").asOpt[String].map {time => 
            commentCreateTime = time
          }
          (comment \ "updated_at").asOpt[String].map {time => 
            commentUpdateTime = time
          }
          val user = Json.parse((comment \ "user").toString())
          if (user != null) {
            (user \ "login").asOpt[String].map {login =>
              commentUserLogin = login
            }  
          }
        }
        if (issue != null) {
          if (comment == null) {
            if (issueAction == "opened" || issueAction == "reopened") {
              Logger.info("An issue is " + issueAction)
              Logger.info("Issue title: " + issueTitle)
              Logger.info("Issue body: " + issueBody)
              issueMap = issueMap.+((issueNumber, new Issue(issueNumber, issueTitle, issueBody)))
            }
            else {
              issueMap = issueMap.-(issueNumber)
            }
          }
          else {
            Logger.info("An issue comment is " + issueAction + " on issue: " + issueTitle)
            Logger.info("Comment body: " + commentBody)
            // check reviewer information
            if (illegalReviewer(commentBody)) {
              Logger.error("Illegal reviewers: " + specifiedReviewer.toString())
              var str = ""
              specifiedReviewer.map(_.drop(1)).map(r => str += r + " ")
              val comment = "Warning: unrecognized reviewers by @" + commentUserLogin + " : " + str
              GithubAPI.addIssueComment(issueNumber, comment)
            }
            // update data structure
            updateIssueCommentView
            // check review status
            if (issueReviewed(commentBody)) {
              Logger.info("Label \"reviewed\" added to issue " + issueNumber.toString())
              GithubAPI.addLabelOnIssue(issueNumber, "reviewed")
            } 
          }
          printIssueMap
        }
        case None => 
          BadRequest("Expecting URL-encoded payload")
      }
      Ok("We are done")
    }.getOrElse {
      BadRequest("Expecting URL-encoded data")
    }
  }
  
  def updateIssueCommentView = {
    // check build bot state and update issue & comment
    issueMap.get(issueNumber) match {
      case Some(issue) => issue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
        //check build and test successful
        getBuildBotState(commentBody) match {
          case BuildStart           => "Do nothing"
            Logger.info("Build started")
            issue.updateBState(BuildStart)
          case BuildSuccess         => "Wait for test success"; 
            Logger.info("Build successful")
            issue.updateBState(BuildSuccess)
          case BuildFailure         => "Remove label";
            Logger.info("Build failed")
            GithubAPI.removeLabelOnIssue(issueNumber, "tested")
            issue.updateBState(BuildFailure)
          case TestStart            => "Do nothing"
            Logger.info("Test started")
            issue.updateBState(TestStart)
          case TestSuccess          => "Check build success"
            Logger.info("Test successful")
            if (issue.getBState == BuildSuccess) {
              Logger.info("Build and test successful!")
              Logger.info("Label \"tested\" added to issue " + issueNumber.toString())
              GithubAPI.addLabelOnIssue(issueNumber, "tested")
            }
            issue.updateBState(TestSuccess)
          case TestFailure          => "Remove label"; 
            Logger.info("Test failed")
            Logger.info("Label \"tested\" removed from issue " + issueNumber.toString())
            GithubAPI.removeLabelOnIssue(issueNumber, "tested")
          case SameBuildBotState    => "Do nothing"
          case UnknownBuildBotState => "Do nothing"
        }
      case None => Logger.error("One opened issue missing?")
        var issue = new Issue(issueNumber, issueTitle, issueBody)
        issue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
        issueMap = issueMap.+((issueNumber, issue))
    }
  }
  
  def printIssueMap = {
    for (key <- issueMap.keySet) {
      issueMap.get(key) match {
        case Some(issue) => Logger.debug(issue.toString()) 
        case None => "Do nothing"
      }
    }
  } 
  
  def illegalReviewer(msg: String): Boolean = {
    if (msg.contains("@")) {
      var tokens = new ArrayBuffer[String](1)
      tokens = tokens.++:(msg.split(" "))
      specifiedReviewer = tokens.filter(token => token.contains("@"))
      var str = ""
      specifiedReviewer.map(r => str += r + " ") 
      Logger.debug("Specified reviewers: " + str)
      for (token <- specifiedReviewer; if !reviewerList.contains(token)) 
        return true
      false
    }
    else
      false
  }
  
  def issueReviewed(msg: String): Boolean = {
    val reviewedMsg = "LGTM" // Looks Good To Me
    if (msg.contains(reviewedMsg))
      true
    else
      false
  }
  
  def getBuildBotState(msg: String): BuildBotState = {
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
        UnknownBuildBotState
    }
    else 
      SameBuildBotState
  }
  
  def initIssueCommentView = {
    val issueString = GithubAPI.getOpenIssues
    try {
      val issues = com.codahale.jerkson.Json.parse[List[String]](issueString) 
      for (issue <- issues) {
        resetIssueCommentFields
        // parse a new issue
        val issueJson = Json.parse(issue)
        (issueJson \ "number").asOpt[Long] match {
          case Some(number) => issueNumber = number
          case None => throw new MalFormedJSONPayloadException("Missing issue number")
        }
        (issueJson \ "title").asOpt[String] match {
          case Some(title) => issueTitle = title
          case None => throw new MalFormedJSONPayloadException("Missing issue title")
        }
        (issueJson \ "body").asOpt[String] match {
          case Some(body) => issueBody = body
          case None => throw new MalFormedJSONPayloadException("Missing issue body")
        }
        var newIssue = new Issue(issueNumber, issueTitle, issueBody)
        (issueJson \ "comment").asOpt[Long] match {
          case Some(nOfcomment) =>
            if (nOfcomment > 0 && issueNumber > 0) {
              val commentString = GithubAPI.getCommentsOnIssue(issueNumber)
              val comments = com.codahale.jerkson.Json.parse[List[String]](commentString)
              for (comment <- comments) {
                // parse a new comment
                val commentJson = Json.parse(comment)
                (commentJson \ "id").asOpt[Long] match {
                  case Some(id) => commentId = id
                  case None => throw new MalFormedJSONPayloadException("Missing comment id")
                }
                (commentJson \ "body").asOpt[String] match {
                  case Some(body) => commentBody = body
                  case None => throw new MalFormedJSONPayloadException("Missing comment body")
                }
                (commentJson \ "created_at").asOpt[String] match {
                  case Some(createTime) => commentCreateTime = createTime
                  case None => throw new MalFormedJSONPayloadException("Missing comment created_at time stamp")
                }
                (commentJson \ "updated_at").asOpt[String] match {
                  case Some(updateTime) => commentUpdateTime = updateTime
                  case None => throw new MalFormedJSONPayloadException("Missing comment updated_at time stamp")
                }
                newIssue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
              }
            }
          case None => throw new MalFormedJSONPayloadException("Missing comment field")
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
  
  def setupGithubHooks = {
    GithubAPI.initParameters(gitHubUser, gitHubPassword, gitHubRepo, gitHubUrl, hookUrl)
    Logger.info("Github Webook parameters: " + "\nUser: " + gitHubUser + "\nRepository: " + gitHubRepo + "\nHook url: " + hookUrl)
    GithubAPI.setupRepoHooks
  }
  
  /*
   * JIRA drivers
   * */
  
  def checkJIRAIssue() = TODO
  
  def addCommentToJIRAIssue() = TODO
}
