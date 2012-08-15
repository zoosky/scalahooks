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

class Comment (id: Long, body: String, createTime: String, updateTime: String) {
  def Body: String = {body}
}

class Issue (number: Long, title: String, body: String) {
  var commentList = new LinkedList[Comment]()
  def insertComment(comment: Comment) = {commentList = commentList.:+(comment)}
  override def toString(): String = {
    val issueString = "Issue title: " + this.title + "\n" +
                      "Issue body: "  + this.body  + "\n"
    var commentString = (commentList.foldLeft(new Comment(-1, "", "", "")) {(x, y) => new Comment(-1, x.Body + "\n" + y.Body, "", "")}).Body
    if (commentString != "") commentString = "Comment body: " + commentString
    issueString + commentString
  }
}

object CoordBot extends Controller {
  
  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  //val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/"+gitHubUser+"/"+gitHubRepo+"/hooks"
  val hookUrl = "http://requestb.in/1l1iohm1"
  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue]()
  val reviewerList = List("@tao", "@adriaan", "@odersky", "@lukas", "@heather", "@vlad")
  val dateParser = ISODateTimeFormat.dateTimeNoMillis();
  def parseISO8601(date: String): Date = {
    dateParser.parseDateTime(date).toDate()
  }
  
  def receiveGithubMsg = Action { msg =>
    Logger.info(msg.body.toString())
    msg.body.asJson.map { json =>
      Logger.info("Receive JSON payload: " + Json.stringify(json))
      var issueAction, issueTitle, issueBody, commentBody, commentCreateTime, commentUpdateTime = ""
      var issueNumber, commentId: Long = -1
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
          //if (!legalReviewer(commentBody))
            //addIllegalReviewerWarning(issueNumber, author, list)
          issueMap.get(issueNumber) match {
            case Some(issue) => issue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
            //check build and test successful
            case None => Logger.error("One opened issue missing?")
              var issue = new Issue(issueNumber, issueTitle, issueBody)
              issue.insertComment(new Comment(commentId, commentBody, commentCreateTime, commentUpdateTime))
              issueMap = issueMap.+((issueNumber, issue))
          }
        }
      }
      // 
      printIssueMap
      // 
      
      // 
      
      // 
      Ok("")
    }.getOrElse {
      BadRequest("Expecting JSON data")
    }
  }
  
  def setupGithubHooks = {
    /* 
     * create a generic web hook 
       supported events: "push", "issues", "issue_comment", "commit_comment", "pull_request", "gollum", "watch", "download", "fork", "fork_apply", "member", "public" 
     */
    // URL request
    val req = url(gitHubUrl)
    // JSON payload
    val jsonObject = generate(Map(
                                   "name" -> "web", 
                                   "events" -> List(
                                       "push", "issues", "issue_comment", "commit_comment", "pull_request", "gollum", "watch", "download", "fork", "fork_apply", "member", "public"
                                   ), 
                                   "active" -> true,
                                   "config" -> Map(
                                       "url" -> hookUrl
                                   )
                                 )
                             )
    // turn the request into POST 
    val reqWithData = req << (jsonObject, "application/json")
    // send HTTP request with authentication information
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        try {
          (Json.parse(response) \ "id").asOpt[Long] match {
            case Some(id) => 
              Logger.info("A generic web hook established with id = " + id.toString())
            case None =>
              Logger.error("No id information?!")
          }
        }
        catch {
          case _ =>
            Logger.error("Expecting JSON data")
          }
        }
    ) 
  }
  
  def printIssueMap = {
    for (key <- issueMap.keySet) {
      issueMap.get(key) match {
        case Some(issue) => println(issue.toString()) 
        case None => "Do nothing"
      }
    }
  } 
  
  def legalReviewer(msg: String): Boolean = {
    if (msg.contains("@")) {
      val tokens = msg.split("@")
      var tokens2 = new Array[String](1)
      for (token <- tokens)
        tokens2 = tokens2.++:(token.split(" "))
      for (token <- tokens2) {
        if (token.contains("@")) {
          if (reviewerList.filter(reviewer => reviewer == token).size == 0)
            false
        }
      }
      true
    }
    else
      true
  }
  
  
  def addLabelOnIssue(PullReq: String, label: String) = TODO
  
  def removeLabelOnIssue(PullReq: String) = TODO
  
  def parseBodyMsg(msg: String) = TODO
  
  def setMileStone() = TODO
  
  def checkJIRAIssue() = TODO
  
  def addCommentToJIRAIssue() = TODO
}
