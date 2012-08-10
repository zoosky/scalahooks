package controllers
package github

import java.util.Date
import org.joda.time.format.ISODateTimeFormat
import cc.spray.json._
import dispatch._
import Application.silentHttp
import models._
import play.libs.Json
import java.util.Calendar
import java.text.SimpleDateFormat

private[github] case class GithubAuthor(email: String, name: String, date: String)
private[github] case class GithubUser(url: String, gravatar_id: String, avatar_url: String, login: String, id: Int)
private[github] case class GithubCommitDetails(url: String, committer: GithubAuthor, message: String, author: GithubAuthor, tree: JsValue)

/* For fetchRevisions, when getting a list of commits */
//private[github] case class GithubCommit(committer: Option[GithubUser], url: String, author: Option[GithubUser], parents: JsValue, commit: GithubCommitDetails, sha: String)

/* For revisionInfo, when reading a single commit's info */
private[github] case class DetailedGithubCommit(committer: Option[GithubUser], url: String, author: Option[GithubUser], parents: JsValue, commit: GithubCommitDetails, sha: String, stats: JsValue, files: JsValue)
/* For initGithubHook, when reading the Github response */
private[github] case class DetailedGithubHook(url: String, updated_at: String, created_at: String, name: String, events: String, active: JsValue, config: JsValue, id: String)

private[github] object GithubJsonProtocol extends DefaultJsonProtocol {
  implicit val githubAuthorFormat = jsonFormat3(GithubAuthor)
  implicit val githubUserFormat = jsonFormat5(GithubUser)
  implicit val githubCommitDetailsFormat = jsonFormat5(GithubCommitDetails)
//  implicit val githubCommitFormat = jsonFormat6(GithubCommit)
  implicit val detailedGithubCommitFormat = jsonFormat8(DetailedGithubCommit)
  implicit val detailedGithubHookFormat = jsonFormat8(DetailedGithubHook)
}

object GithubTools {
  import GithubJsonProtocol._
  import Config._

  // Date example: 2012-03-16T02:01:25-07:00
  // http://stackoverflow.com/questions/2201925/converting-iso8601-compliant-string-to-java-util-date
  val dateParser = ISODateTimeFormat.dateTimeNoMillis();
  def parseISO8601(date: String): Date = {
    dateParser.parseDateTime(date).toDate()
  }
  
  def revisionInfo(sha: String): Commit = {
    val req = url("https://api.github.com/repos/"+githubUser+"/"+githubRepo+"/commits/"+ sha)
    val commit = silentHttp(req >:+ { (headers, req) =>
      // todo: check stuff with header, fail if problem

      // handle request
      req >- { jsonString =>
        JsonParser(jsonString).convertTo[DetailedGithubCommit]
      }

/* spray does not support InputStream
      req >> { in =>
        JsonParser(in).convertTo[List[GithubCommit]]
      } */
    })
    Commit(
      commit.sha,
      parseISO8601(commit.commit.committer.date),
      commit.author.map(u => u.login),
      commit.commit.author.name,
      New,
      None,
      None,
      None
    )
  }

  def setupGithubHook(hookUrl: String): HookEvent = {
    /* create a generic web hook 
       supported events: "push", "issues", "issue_comment", "commit_comment", "pull_request", "gollum", "watch", "download", "fork", "fork_apply", "member", "public" 
     */
    val req = url("https://api.github.com/repos/"+githubUser+"/"+githubRepo+"/hooks/" )
    
    
    val reqWithData = req << Json.toJson(Map("name" -> "web", 
                                    "events" -> "[\"push\", \"issues\", \"issue_comment\", \"commit_comment\", \"pull_request\", \"gollum\", \"watch\", \"download\", \"fork\", \"fork_apply\", \"member\", \"public\"]", 
                                    "active" -> true,
                                    "url" -> hookUrl)).toString

   val result = silentHttp( reqWithData >- { jsonString =>
        Json.parse(jsonString)
      }
   ) 
   
    val today = Calendar.getInstance().getTime()
    HookEvent( 
        today, 
        hook.id,
        "Register web hooks",
        hook.events, 
        Hooks, 
        NoState
        )
  }
    
/*
fetching new revisions is not done through github. there are problems with the pagination scheme due to the graph nature of commits.

in the list of commits (https://github.com/scala/scala/commits/) we have the following order:

eb8afde6882a945caa029a2ea9daeb43c590f5ca
7c5c06f5f421453c46972d079d0eab5550d848fc
62422f3a54d157fb3280ee51a62b7cfe5b759acb
fb44bb28b8b3e7861b96c874dc79072f89fec10b


using
  https://api.github.com/repos/scala/scala/commits?per_page=30&sha=master
we get all these commits.

using
  https://api.github.com/repos/scala/scala/commits?last_sha=eb8afde6882a945caa029a2ea9daeb43c590f5ca&per_page=5&sha=master&top=master
we get fb44bb28b8b3e7861b96c874dc79072f89fec10b, the two commits in between are skipped.

    
    val page = lastSha.map(sha => "last_sha="+ sha +"&").getOrElse("")
    val args = "per_page="+ num +"&sha="+ githubBranch + "&top="+ githubBranch
*/

/*
  def fetchRevisions(/*lastSha: Option[String],*/ num: Int): List[Commit] = {
    val urlStr = "https://api.github.com/repos/"+githubUser+"/"+githubRepo+"/commits?"


    val args = "per_page="+ num +"&sha="+ githubBranch
    val req = url(urlStr + args)
    val res = silentHttp(req >:+ { (headers, req) =>
      // todo: check stuff with header, fail if problem

      // handle request
      req >- { jsonString =>
        JsonParser(jsonString).convertTo[List[GithubCommit]]
      }
    })
    res.map(commit => Commit(
      commit.sha,
      parseISO8601(commit.commit.committer.date),
      commit.author.map(u => u.login),
      commit.commit.author.name,
      Missing,
      None,
      None,
      None
    ))
  }

  def revisionStream(/*lastSha: Option[String] = None,*/ i: Int = 0, avail: List[Commit] = Nil): Stream[Commit] = {
    avail match {
      case Nil =>
        fetchRevisions(/*lastSha,*/ i + 5).drop(i) match {
          case Nil => Stream.empty
          case x :: xs => Stream.cons(x, revisionStream(/*Some(x.sha),*/ i+1, xs))
        }
      case x :: xs =>
        Stream.cons(x, revisionStream(/*Some(x.sha),*/ i+1, xs))
    }
  }
*/

}
