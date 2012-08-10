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

/**
 * */

object CoordBot extends Controller {
  
  /**
   * Github related
   * */
  
  def setupGithubHook {
    val hookEvent = GithubTools.setupGithubHook(hookUrl)
    Logger.info("Hooked events: " + hookEvent.body)
  }
   
  def receiveGithubMsg = Action { msg =>
    msg.body.asJson.map { json =>
      // type
      (json \ "type").asOpt[String].map { msgType => msgType match {
          // issue event
          case "issues" => {
            (json \ "payload").asOpt[String].map { payload => 
              (Json.parse(payload) \ "action").asOpt[String].map { state  => state match {
                case "open" => {
                  (Json.parse(payload) \ "issue").asOpt[String].map {issue =>
                    var issueNumber, issueTitle, issueBody = ""
                    (Json.parse(issue) \ "number").asOpt[String].map {number => issueNumber = number}
                    (Json.parse(issue) \ "title").asOpt[String].map {title => issueTitle = title}
                    (Json.parse(issue) \ "body").asOpt[String].map {body => issueBody = body}
                    val today = Calendar.getInstance().getTime()
                    val hookEvent = HookEvent(today, issueNumber, issueTitle, issueBody, Issues, IssueOpen)
                    Logger.info("A new issue: " + hookEvent.title + " with issue number = " + hookEvent.number)
                  }
                }
              }
              }
            }
          }
          // issue comment
          case "issue_comment" => {
            (json \ "payload").asOpt[String].map { payload => 
              var issueNumber = ""
              (Json.parse(payload) \ "issue").asOpt[String].map { issue =>
                (Json.parse(issue) \ "number").asOpt[String].map {number => issueNumber = number}
              }
              var issueCommentBody = ""
              (Json.parse(payload) \ "comment").asOpt[String].map { comment =>
                (Json.parse(comment) \ "body").asOpt[String].map {body => issueCommentBody = body}
              }
              val today = Calendar.getInstance().getTime()
              val hookEvent = HookEvent(today, issueNumber, "Comment", issueCommentBody, IssueComment, NoState)
              Logger.info("A comment: " + hookEvent.body + " on issue number = " + hookEvent.number)
            }
          }
        }
        Ok("Github message: " + Json.parse(msgType))
      }.getOrElse {
        BadRequest("Missing parameter [type]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }
  
  def addLabelOnPullReq(PullReq: String, label: String) = TODO
  
  def removeLabelOnPullReq(PullReq: String) = TODO
  
  def parseCommitMsg(msg: String) = TODO
  
  def parseJenkinsBuildLog(log: String) = TODO
  
  def checkReviewer() = TODO
  
  def setMileStone() = TODO
  
  def checkJIRATicket() = TODO
  
  def addCommentToJIRATicket() = TODO
}
