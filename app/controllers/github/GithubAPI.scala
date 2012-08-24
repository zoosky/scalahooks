package controllers
package github
import dispatch.url
import Application.silentHttp
import com.codahale.jerkson.Json._
import play.api.Logger
import play.api.libs.json.Json

/**
 * */

object GithubAPI {
  
  var gitHubUser = ""
  var gitHubPassword = ""
  var gitHubRepo = ""
  var gitHubUrl = ""
  var hookUrl = ""
    
  def initParameters(user: String, password: String, repository: String, gitHubUrl: String, hookUrl: String) = {
    this.gitHubUser = user
    this.gitHubPassword = password
    this.gitHubRepo = repository
    this.gitHubUrl = gitHubUrl
    this.hookUrl = hookUrl
  }
  
  def setupRepoHooks = {
    /* 
     * create a generic web hook   
     {
       "name": "web",
       "events": [
       "push", "issues", "issue_comment", "commit_comment", "pull_request", "gollum", "watch", "download", "fork", "fork_apply", "member", "public" 
       ],
       "active": true,
       "config": {
         "url": "http://something.com/webhook"
       }
     }
     */
    val req = url(gitHubUrl+"/hooks")
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
    val reqWithData = req << (jsonObject, "application/json")
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
  
  def getOpenIssues: String = {
    /* 
     * list issues
       {
         "state": "open" (default)
       }
     */
    val req = url(gitHubUrl+"/issues")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def getIssue(issueNumber: Long) = {
    val req = url(gitHubUrl+"/issues/"+issueNumber.toString())
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def getCommentsOnIssue(issueNumber: Long): String = {
    /* 
     * no input parameters
     */
    val req = url(gitHubUrl+"/issues/"+issueNumber.toString()+"/comments")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def getLabelsOnIssue(issueNumber: Long): String = {
    /* 
     * no input parameters
     */
    val req = url(gitHubUrl+"/issues/"+issueNumber.toString()+"/labels")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def getLabels: String = {
    /* 
     * no input parameters
     */
    val req = url(gitHubUrl+"/labels")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def addCommentOnIssue(issueNumber: Long, comment: String) = {
    /* 
     * create an issue comment
       {
         "body": "comment"
       }
     */
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString() + "/comments")
    val jsonObject = generate(Map(
                                   "body" -> comment
                                 )
                             )
    val reqWithData = req << (jsonObject, "application/json")
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        try {
          (Json.parse(response) \ "id").asOpt[Long] match {
            case Some(id) => 
              Logger.info("An issue comment created with id = " + id.toString())
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
  
  def deleteCommentOnIssue(id: Long) = {
    val req = url(gitHubUrl+"/issues/comments/"+id.toString())
    val reqWithData = req
    silentHttp( reqWithData.DELETE.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
      }
    ) 
  }
  
  def addLabelOnIssue(issueNumber: Long, label: String) = {
    /* 
     * create an issue label
       [
         "label"
       ]
     */
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString() + "/labels")
    val jsonObject = generate(List(label)
                             )
    val reqWithData = req << (jsonObject, "application/json")
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
      }
    ) 
  }
  
  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString() + "/labels/" + label)
    val reqWithData = req
    silentHttp( reqWithData.DELETE.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        try {
          (Json.parse(response) \ "name").asOpt[String] match {
            case Some(name) => 
              Logger.info("Label " + label + " is deleted from issue " + issueNumber.toString())
            case None =>
              Logger.error("Label " + label + " does not exist?!")
          }
        }
        catch {
          case _ =>
            Logger.error("Expecting JSON data")
        }
      }
    ) 
  }
  
  def closeIssue(issueNumber: Long) = {
    /* 
     * close an issue
       {
         "state": "closed"
       }
     */
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString())
    val jsonObject = generate(Map("state" -> "closed"))
    val reqWithData = req << (jsonObject, "application/json")
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Response: " + response)
        try {
          (Json.parse(response) \ "number").asOpt[Long] match {
            case Some(number) => 
              Logger.info("Issue " + issueNumber + " is closed")
            case None =>
              Logger.error("Issue " + issueNumber + " does not exist?!")
          }
        }
        catch {
          case _ =>
            Logger.error("Expecting JSON data")
        }
      }
    ) 
  }
  
  def setMileStone() = {}
}