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

/**
 * */

object CoordBot extends Controller {
  
  /**
   * Github related
   * */
  
  val gitHubUser = "taolee"
  val gitHubRepo = "scala"
  //val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  //val gitHubUrl = "https://api.github.com/repos/"+gitHubUser+"/"+gitHubRepo+"/hooks"
  val hookUrl = "http://requestb.in/1l1iohm1"
  val gitHubUrl = hookUrl
   
  def receiveGithubMsg = Action { msg =>
    Logger.info(msg.body.toString())
    msg.body.asJson.map { json =>
      Logger.info("Receive JSON payload: " + Json.stringify(json))
      var issueAction, issueNumber, issueTitle, issueBody, commentBody = ""
      // type
      /*(json \ "type").asOpt[String].map { eventType => 
        Logger.info("Type: " + eventType)
      }*/
      // action
      (json \ "action").asOpt[String].map { action => 
        Logger.info("Action: " + action)
        issueAction = action
      }
      // issue
      val issue = (json \ "issue").toString()
      (Json.parse(issue) \ "number").asOpt[String].map {number => issueNumber = number}
      (Json.parse(issue) \ "title").asOpt[String].map {title => issueTitle = title}
      (Json.parse(issue) \ "body").asOpt[String].map {body => issueBody = body}
      // comment
      val comment = (json \ "comment").toString()
      (Json.parse(comment) \ "body").asOpt[String].map {body => commentBody = body}
      /*
      if (issueAction == "open") {
        Logger.info("An issue is opened")
      }
      else
        Logger.info("An issue event is discarded")
      if (issueBody != "") {
        Logger.info("Issue body: " + issueBody)
        if (issueBody.contains("help"))
          Logger.info("Help needed")
      }
      else
        Logger.info("No issue body found") 
      if (commentBody != "") { 
        Logger.info("Issue comment: " + commentBody)
        if (commentBody.contains("@"))
          Logger.info("Reviewer information found")
      }
      else
        Logger.info("Not an issue comment")   
        */ 
      Ok("")
    }.getOrElse {
      BadRequest("Expecting JSON data")
    }
  }
  
  def setupGithubHook() {
    
    /* create a generic web hook 
       supported events: "push", "issues", "issue_comment", "commit_comment", "pull_request", "gollum", "watch", "download", "fork", "fork_apply", "member", "public" 
     */
    
    // URL request
    //val req = url(gitHubUrl)
    val req = url("https://api.github.com/repos/scala/scala/hooks")
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
    Logger.info("JSON payload: " + jsonObject)
    // turn the request into POST 
    //val reqWithData = req << (jsonObject, "application/json")
    val reqWithData = url("https://api.github.com/repos/scala/scala/pulls")
    // send HTTP request, return a string
    var result = silentHttp( reqWithData >- { jsonString =>
        Logger.error("Returned string: " + jsonString)
        jsonString
      }
    ) 
    // check the returned result
    try {
      (Json.parse(result) \ "id").asOpt[String] match {
        case Some(id) => 
          Logger.info("id = " + id + " returned from web hook")
        case None =>
          Logger.error("No id information returned from web hook " + "\n" + result)
      }
    }
    catch {
      case _ =>
        Logger.error("Expecting JSON data: " + result)
    }
  }
  
  def addLabelOnPullReq(PullReq: String, label: String) = TODO
  
  def removeLabelOnPullReq(PullReq: String) = TODO
  
  def parseCommitMsg(msg: String) = TODO
  
  def parseJenkinsBuildLog(log: String) = TODO
  
  def checkReviewer() = TODO
  
  def setMileStone() = TODO
  
  def checkJIRAIssue() = TODO
  
  def addCommentToJIRAIssue() = TODO
}
