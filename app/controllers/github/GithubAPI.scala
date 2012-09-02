package controllers
package github
import dispatch.url
import Application.silentHttp
import com.codahale.jerkson.Json._
import play.api.Logger
import play.api.libs.json.Json
import org.codehaus.jackson.JsonNode

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
  
  def setupAllRepoHooks = {
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
        Logger.info("Setup web hooks response: " + response)
      }
    ) 
  }
  
  def getHooks: String = {
    val req = url(gitHubUrl+"/hooks")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Get all hooks response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def deleteHook(id: Long) = {
    val req = url(gitHubUrl+"/hooks/"+id.toString())
    val reqWithData = req
    silentHttp( reqWithData.DELETE.as_!(gitHubUser, gitHubPassword) >|) 
    Logger.debug("Hook " + id.toString() + " deleted")
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
        Logger.info("Get all open issues response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def getIssue(issueNumber: Long) = {
    val req = url(gitHubUrl+"/issues/"+issueNumber.toString())
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Get issue "+ issueNumber.toString() + " response: " + response)
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
        Logger.info("Get comments on issue " + issueNumber.toString() + " response: " + response)
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
        Logger.info("Get label on issue " + issueNumber.toString() + " response: " + response)
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
        Logger.info("Get labels response: " + response)
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
        Logger.info("Add comment " + comment + " on issue " + issueNumber.toString() + " response: " + response)
      }
    ) 
  }
  
  def deleteCommentOnIssue(id: Long) = {
    val req = url(gitHubUrl+"/issues/comments/"+id.toString())
    val reqWithData = req
    silentHttp( reqWithData.DELETE.as_!(gitHubUser, gitHubPassword) >|)
    Logger.info("Comment " + id.toString() + " deleted")
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
        Logger.info("Add label " + label + " on issue " + issueNumber.toString() + " response: " + response)
      }
    ) 
  }
  
  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString() + "/labels/" + label)
    val reqWithData = req
    silentHttp( reqWithData.DELETE.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Delete label " + label + " on issue " + issueNumber.toString() + " response: " + response)
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
        Logger.info("Close issue " + issueNumber.toString() + " response: " + response)
      }
    ) 
  }
  
  def editIssueBody(issueNumber: Long, body: String) = {
    /* 
     * edit an issue body
       {
         "body": "body content"
       }
     */
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString())
    val jsonObject = generate(Map("body" -> body))
    val reqWithData = req << (jsonObject, "application/json")
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Edit issue " + issueNumber.toString() + " response: " + response)
      }
    ) 
  }
  
  def getMilestones: String = {
    /* 
     * list issues
       {
         "state": "open" (default)
       }
     */
    val req = url(gitHubUrl+"/milestones")
    val reqWithData = req
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Get all milestones response: " + response)
        response
        // response to be parsed by CoordBot
      }
    ) 
  }
  
  def editIssueMilestone(issueNumber: Long, milestoneNumber: Long) = {
    /* 
     * edit an issue milestone
       {
         "milestone": "milestone number"
       }
     */
    val req = url(gitHubUrl+"/issues/" + issueNumber.toString())
    val jsonObject = generate(Map("milestone" -> milestoneNumber.toString()))
    val reqWithData = req << (jsonObject, "application/json")
    silentHttp( reqWithData.as_!(gitHubUser, gitHubPassword) >- { response =>
        Logger.info("Edit issue " + issueNumber.toString() + " response: " + response)
      }
    ) 
  }
}

/**
 * */

case class GithubAPILabel(url: String, 
                          name: String, 
                          color: String)
                          
case class GithubAPIUser(login: String, 
                         id: Long, 
                         avatar_url: String, 
                         gravatar_id: String, 
                         url: String) 
                         
case class GithubAPIAssignee(login: String, 
                             id: Long, 
                             avatar_url: String, 
                             gravatar_id: String, 
                             url: String)
                             
case class GithubAPICreator(login: String, 
                            id: String, 
                            avatar_url: String, 
                            gravatar_id: String, 
                            url: String)
                            
case class GithubAPIMilestone(url: String, 
                              number: Long, 
                              state: String, 
                              title: String, 
                              description: String, 
                              creator: GithubAPICreator, 
                              open_issues: Long, 
                              closed_issues: Long, 
                              created_at: String, 
                              due_on: Option[String])
                              
case class GithubAPIPullRequest(html_url: String, 
                                diff_url: String, 
                                patch_url: String) 
                                
case class GithubAPIIssue(url: String, 
                          html_url: String, 
                          number: Long, 
                          state: String, 
                          title: String, 
                          body: String, 
                          user: GithubAPIUser,               
                          labels: List[GithubAPILabel], 
                          assignee: JsonNode, 
                          milestone: JsonNode, 
                          comments: Long, 
                          pull_request: JsonNode, 
                          closed_at: JsonNode, 
                          created_at: String, 
                          updated_at: String) 
                          
case class GithubAPIComment(id: Long,
                            url: String,
                            body: String,
                            user: GithubAPIUser,            
                            created_at: String, 
                            updated_at: String)
                        
case class GithubAPIHookConfig(url: String, content_type: String)

case class GithubAPIHook(url: String,
                         updated_at: String,
                         created_at: String,
                         name: String,
                         events: List[String],
                         active: Boolean,
                         config: JsonNode,
                         last_response: JsonNode,
                         id: Long)
                            