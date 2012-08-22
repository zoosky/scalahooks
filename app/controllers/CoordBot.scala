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
import models.CommentAction
import models.CommentCreated
import models.ReviewStatus
import models.ReviewNone

/**
 * */

object CoordBot extends Controller {
  
  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  //val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/"+gitHubUser+"/"+gitHubRepo
  val hookUrl = "http://requestb.in/1754sxr1"
  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue]()
  val reviewerList = List("@taolee", "@adriaan", "@odersky", "@lukas", "@heather", "@vlad")
  var specifiedReviewer = new ArrayBuffer[String](1)
  var issueAction, issueTitle, issueBody = "" 
  var commentBody, commentCreateTime, commentUpdateTime, commentUserLogin = ""
  var issueNumber, commentId: Long = -1
  def resetIssueCommentFields = {issueAction = ""; issueTitle = ""; issueBody = ""; commentBody = ""; commentCreateTime = ""; commentUpdateTime = ""; commentUserLogin = ""; issueNumber = -1; commentId = -1}
  def resetIssueFields = {issueNumber = -1; issueAction = ""; issueTitle = ""; issueBody = "";}
  def resetCommentFields = {commentId = -1; commentBody = ""; commentCreateTime = ""; commentUpdateTime = ""; commentUserLogin = "";}
  var totalLabelList = new LinkedList[String]()
  
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
            if (illegalReviewer(commentBody) 
             && !coordBotComment(commentBody) // avoid comments by Coord Bot (possible infinite-loops)
             ) {
              Logger.error("Illegal reviewers: " + specifiedReviewer.toString())
              var str = ""
              specifiedReviewer.map(_.drop(1)).map(r => str += r + " ")
              val comment = "Web Bot: unrecognized reviewers by @" + commentUserLogin + " : " + str
              GithubAPI.addIssueComment(issueNumber, comment)
            }
            // update data structure
            //updateIssueCommentView
            // check review status
            if (issueReviewed(commentBody)) {
              Logger.info("Label \"reviewed\" added to issue " + issueNumber.toString())
              if (labelAvailable("reviewed"))
                addLabelOnIssue(issueNumber, "reviewed")
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
  
  def checkIssueView(issueNumber: Long) = {
    
  }
  
  def updateIssueView(issueNumber: Long): (BuildBotState, ReviewStatus, List[String], List[Comment]) = {
    var labelList = new LinkedList[String]()
    var bState = UnknownBuildBotState
    var rStatus = UnknownReviewStatus
    val commentString = GithubAPI.getCommentsOnIssue(issueNumber)
    try {
      val comments = com.codahale.jerkson.Json.parse[List[String]](commentString)
      var commentList = for (comment <- comments) yield {
        resetCommentFields
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
        new Comment(CommentCreated, commentId, commentBody, commentCreateTime, commentUpdateTime)
      } 
      
    }
    catch {
      case _ =>
        Logger.error("Expecting JSON data")
    }
    (UnknownBuildBotState, UnknownReviewStatus, List(), List())
  }
  
  /*
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
            if (labelAvailable("tested"))
              removeLabelOnIssue(issueNumber, "tested")
            issue.updateBState(BuildFailure)
          case TestStart            => "Do nothing"
            Logger.info("Test started")
            issue.updateBState(TestStart)
          case TestSuccess          => "Check build success"
            Logger.info("Test successful")
            if (issue.getBState == BuildSuccess) {
              Logger.info("Build and test successful!")
              Logger.info("Label \"tested\" added to issue " + issueNumber.toString())
              if (labelAvailable("tested"))
                addLabelOnIssue(issueNumber, "tested")
            }
            issue.updateBState(TestSuccess)
          case TestFailure          => "Remove label"; 
            Logger.info("Test failed")
            Logger.info("Label \"tested\" removed from issue " + issueNumber.toString())
            if (labelAvailable("tested"))
              removeLabelOnIssue(issueNumber, "tested")
            issue.updateBState(TestFailure)
          case SameBuildBotState    => "Do nothing"
          case UnknownBuildBotState => "Do nothing"
        }
      case None => Logger.error("One opened issue missing?")
        var issue = new Issue(issueNumber, issueTitle, issueBody)
        issue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
        issueMap = issueMap.+((issueNumber, issue))
    }
  }
  */
  
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
  
  def coordBotComment(msg: String): Boolean = {
    return msg.contains("Web Bot:")
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
      issueMap.clear()
      for (issue <- issues) {
        resetIssueFields
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
                resetCommentFields
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
                newIssue.insertComment(new Comment(CommentCreated, commentId, commentBody, commentCreateTime, commentUpdateTime))
              }
            }
          case None => throw new MalFormedJSONPayloadException("Missing comment field")
        }
        issueMap = issueMap.+((issueNumber, newIssue))
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
  
  def getLabels = {
    val labelString = GithubAPI.getLabels
    try {
      val labels = com.codahale.jerkson.Json.parse[List[String]](labelString)
      for (label <- labels) { 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => totalLabelList = totalLabelList.:+(name)
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
      var labelList = new LinkedList[String]()
      for (label <- labels) { 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => labelList = labelList.:+(name)
          case None => throw new MalFormedJSONPayloadException("Missing name field")
        } 
      }
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
  
  def removeLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    try {
      val labels = com.codahale.jerkson.Json.parse[List[String]](labelString)
      var labelList = new LinkedList[String]()
      for (label <- labels) { 
        val labelJson = Json.parse(label);  
        (labelJson \ "name").asOpt[String] match {
          case Some(name) => labelList = labelList.:+(name)
          case None => throw new MalFormedJSONPayloadException("Missing name field")
        } 
      }
      if (labelList.contains(label))
        GithubAPI.removeLabelOnIssue(issueNumber, label)
      else {
        var str = "Label " + label + " does not exist: "
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
  
  /*
   * JIRA drivers
   * */
  
  def checkJIRAIssue() = TODO
  
  def addCommentToJIRAIssue() = TODO
}
