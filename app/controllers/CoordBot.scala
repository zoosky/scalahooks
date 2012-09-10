package controllers

import scala.actors.threadpool.TimeoutException
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import com.codahale.jerkson.ParsingException
import akka.actor.Props
import akka.util.duration.intToDurationInt
import github._
import models._
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.json.Json
import play.api.mvc._
import play.api.Logger

/**
 * The Coordination Web Bot Ver 0.001
 */

object CoordBot extends Controller {

  var issueMap: Map[Long, Issue] = new HashMap[Long, Issue] with SynchronizedMap[Long, Issue]
  var totalLabelList = new ListBuffer[String]
  var milestoneMap: Map[String, Long] = new HashMap[String, Long]
  var coordBotInit = true

  /**
   * Actor
   */

  val coordActor = Akka.system.actorOf(Props[CoordActor])
  if (Config.enableActor) {
    Akka.system.scheduler.schedule(Config.initialDelay, Config.updateFrequency, coordActor, "refresh")
    Logger.debug("Coordination Actor is activated")
  }

  /**
   * The main page
   */

  def index() = Action { implicit request =>
    handleTimeout(e => "Timeout when reading the list of open issues " + e.toString) {
      if (coordBotInit) {
        try {
          // setup web hooks
          setupGithubEnv
          // initialize issue-comment view
          issueMap.clear
          initIssueCommentView
        } catch {
          case e: MalFormedJSONPayloadException =>
            Logger.error(e.toString())
          case e: MissingDefaultLabelsException =>
            Logger.error(e.toString())
          case e: ParsingException =>
            Logger.error(e.getMessage())
          case e: MissingMilestoneMappingException =>
            Logger.error(e.getMessage())
          case _ =>
            Logger.error("Expecting JSON data")
        }
      }
      // show open issues
      val issues = issueMap.values.toList.sortWith((a, b) => a.Number < b.Number)
      Ok(views.html.index(issues))
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
        case Some(jsonStringSeq) =>
          val jsonString = jsonStringSeq.head;
          Logger.info("Receive JSON payload: " + jsonString)
          try {
            val json = Json.parse(jsonString)
            // action
            (json \ "action").asOpt[String] match {
              case Some(action) =>
                var msgAction = ""
                msgAction = action
                // issue
                val issueString = (json \ "issue").toString()
                if (issueString != null) {
                  val issue = com.codahale.jerkson.Json.parse[GithubAPIIssue](issueString)
                  if (msgAction != "closed") {
                    Logger.debug("Update issue " + issue.number.toString())
                    issueMap.update(issue.number, updatedIssueCommentView(issue.number))
                  } else {
                    Logger.debug("Close issue " + issue.number.toString())
                    issueMap = issueMap.-(issue.number)
                  }
                }
              case None => "Do nothing"
            }
          } catch {
            case e: MalFormedJSONPayloadException =>
              Logger.error(e.toString())
            case e: ParsingException =>
              Logger.error(e.getMessage())
            case _ =>
              Logger.error("Expecting JSON data")
          }
        case None =>
          BadRequest("Expecting URL-encoded payload")
      }
      Ok("We are done!")
    }.getOrElse {
      BadRequest("Expecting URL-encoded data")
    }
  }

  def editIssue(issue: GithubAPIIssue): GithubAPIIssue = {
    var newIssueBody = issue.body
    val links = CoordBotUtil.missingJIRALinks(issue.body, CoordBotUtil.JIRATickets(issue.title))
    if (links.size > 0) {
      Logger.debug("Add JIRA links to the body of issue " + issue.number)
      newIssueBody = links.mkString("", "\n", "\n") + issue.body
      GithubAPI.editIssueBody(issue.number, newIssueBody)
    }
    issue.copy(body = newIssueBody)
  }

  def actorCheckIssueCommentView = {
    def refreshView = {
      issueMap.clear
      initIssueCommentView
      totalLabelList.clear
      CoordBotUtil.getLabels
      CoordBotUtil.getMilestones
    }
    def checkView = {
      issueMap.keySet.map { key =>
        issueMap.get(key) match {
          case Some(issue) =>
            issue.getRStatus match {
              case ReviewWarning =>
                "Scan review comments"
                val latestReviewWarning = issue.commentList.filter(comment => CoordBotUtil.getReviewStatus(comment.Body) == ReviewWarning).maxBy(_.getCreateTime)
                if (CoordBotUtil.checkExpiredReviewWarning(latestReviewWarning)) {
                  GithubAPI.addCommentOnIssue(issue.Number, latestReviewWarning.Body) // this would trigger an issue update to delete the expired warning
                  Logger.debug("Actor adds new review warning comment")
                }
              case _ => "Do nothing"
            }
          case None => "Do nothing"
        }
      }
    }
    try {
      refreshView
      checkView
    } catch {
      case e: MalFormedJSONPayloadException =>
        Logger.error(e.toString())
      case e: MissingDefaultLabelsException =>
        Logger.error(e.toString())
      case e: ParsingException =>
        Logger.error(e.getMessage())
      case e: MissingMilestoneMappingException =>
        Logger.error(e.getMessage())
      case _ =>
        Logger.error("Expecting JSON data")
    }
  }

  def checkIssueCommentView(issue: GithubAPIIssue): Issue = {
    var labelList = new ListBuffer[String]
    var buildCommentList = new ListBuffer[Comment]
    var testCommentList = new ListBuffer[Comment]
    var reviewCommentList = new ListBuffer[Comment]
    var bState: BuildState = BuildNone
    var tState: TestState = TestNone
    var rStatus: ReviewStatus = ReviewNone
    var commentList = new ListBuffer[Comment]
    var reviewList = new ListBuffer[Review]
    var rOpenList = new ListBuffer[Comment]
    var rWarnList = new ListBuffer[Comment]
    var bSuccList = new ListBuffer[Comment]
    var tSuccList = new ListBuffer[Comment]

    def checkPullRequest = {
      if (issue.pull_request.html_url != null) {
        Logger.debug("A Pull Request!")
        // check base label and set milestone
        val pullString = GithubAPI.getPullRequest(issue.number)
        val pull = com.codahale.jerkson.Json.parse[GithubAPIPullRequest](pullString)
        Config.pullRequestMileStone.get(pull.base.label) match {
          case Some(milestone) =>
            milestoneMap.get(milestone) match {
              case Some(milestoneNumber) =>
                GithubAPI.editIssueMilestone(issue.number, milestoneNumber)
              case None =>
                "Ooops, something is wrong."
                Logger.error("Cannot find milestone: " + milestone)
                throw new MissingMilestoneMappingException("Cannot find milestone: " + milestone)
            }
          case None =>
            "Ooops, something is wrong."
            Logger.error("Cannot find the milestone mapping of " + pull.base.label)
            throw new MissingMilestoneMappingException("Cannot find the milestone mapping of " + pull.base.label)
        }
        // add links to JIRA issue tracker

      }
    }

    def scanIssueTitle = {}

    def scanIssueBody = {
      val rstatus = CoordBotUtil.getReviewStatus(issue.body)
      rstatus match {
        case ReviewOpen =>
          "Delete the reviewed label, and save comment id for redundancy check"
          labelList.-=("reviewed")
          reviewList = rstatus.reviewers.map { reviewer => new Review(reviewer, ReviewOpen) }
          rStatus = rstatus
        case ReviewFault =>
          "Add illegal reviewer comment"
          rStatus = rstatus
          rStatus.msg = "Web Bot: unrecognized reviewers by @" + issue.user.login + " : " + rstatus.reviewers.filter { reviewer => !Config.reviewerList.contains(reviewer) }.map { _.drop(1) }.mkString(" ")
        case _ => "Do nothing"
      }
    }

    def scanComments = {
      commentList.++=(CoordBotUtil.getCommentsOnIssue(issue.number).sortWith((a, b) => a.getCreateTime < b.getCreateTime))
      commentList.map { comment =>
        CoordBotUtil.processComment(comment.Body) match {
          case (bstate, tstate, rstatus) =>
            // get comment type
            CoordBotUtil.getCommentType(bstate, tstate, rstatus) match {
              case BuildComment => buildCommentList.+=(comment)
              case TestComment => testCommentList.+=(comment)
              case ReviewComment => reviewCommentList.+=(comment)
              case UnknownComment => "Do nothing"
            }
            // update build state
            bstate match {
              case BuildSuccess =>
                "Check test success, and save comment id for redundancy check";
                tState match {
                  case TestSuccess =>
                    "Add the tested label"
                    labelList.+=("tested")
                  case _ => "Do nothing"
                }
                bSuccList.+=(comment)
                bState = bstate
                Logger.debug("Build success")
              case BuildFailure =>
                "Delete the tested label";
                labelList.-=("tested")
                bState = bstate
                Logger.debug("Build failure")
              case _ => "Do nothing"
            }
            // update test state
            tstate match {
              case TestSuccess =>
                "Check build success, and save comment id for redundancy check"
                bState match {
                  case BuildSuccess =>
                    "Add the tested label"
                    labelList.+=("tested")
                  case _ => "Do nothing"
                }
                tSuccList.+=(comment)
                tState = tstate
                Logger.debug("Test success")
              case TestFailure =>
                "Delete the tested label";
                labelList.-=("tested")
                tState = tstate
                Logger.debug("Test failure")
              case _ => "Do nothing"
            }
            // update review status
            rstatus match {
              case ReviewOpen =>
                "Delete the reviewed label, and save comment id for redundancy check"
                labelList.-=("reviewed")
                reviewList = rstatus.reviewers.map { reviewer => new Review(reviewer, ReviewOpen) }
                rOpenList.+=(comment)
                rStatus = rstatus
              case ReviewFault =>
                "Add illegal reviewer comment"
                rStatus = rstatus
                rStatus.msg = "Web Bot: unrecognized reviewers by @" + comment.User + " : " + rstatus.reviewers.filter { reviewer => !Config.reviewerList.contains(reviewer) }.map { _.drop(1) }.mkString(" ")
              case ReviewDone =>
                "Add the reviewed label"
                reviewList = reviewList.map { review => if (review.getReviewer == comment.User) { review.copy(rStatus = ReviewDone) } else review }
                if (reviewList.size > 0 &&
                  reviewList.forall { review => review.rStatus == ReviewDone }) {
                  Logger.debug("All reviewers LGTM")
                  // wait until all reviewers add LGTM comments 
                  labelList.+=("reviewed")
                  rStatus = rstatus
                }
              case ReviewWarning =>
                "Save comment id for redundancy check"
                Logger.debug("Review warning found")
                rWarnList.+=(comment)
                rStatus = rstatus
              case _ => "Do nothing"
            }
        }
      }
    }

    def addComments = {
      rStatus match {
        case ReviewFault =>
          "add review warning comment"
          GithubAPI.addCommentOnIssue(issue.number, rStatus.msg)
        case _ => "Do nothing"
      }
    }

    def deleteComments = {
      rStatus match {
        case ReviewOpen =>
          "Delete all previous review comments"
          val latestOpenReview = rOpenList.maxBy(_.getCreateTime)
          reviewCommentList.filter { _.Id != latestOpenReview.Id }.map { comment =>
            GithubAPI.deleteCommentOnIssue(comment.Id)
          }
        case ReviewWarning =>
          "Delete the expired warning"
          rWarnList.map { comment =>
            if (CoordBotUtil.checkExpiredReviewWarning(comment))
              GithubAPI.deleteCommentOnIssue(comment.Id)
          }
        case _ => "Do nothing"
      }
      (bState, tState) match {
        case (BuildSuccess, TestSuccess) =>
          "Delete old build/test messages, only keep the latest build/test messages"
          val latestBuildSucc = bSuccList.maxBy(_.getCreateTime)
          buildCommentList.filter { _.Id != latestBuildSucc.Id }.map { comment =>
            GithubAPI.deleteCommentOnIssue(comment.Id)
          }
          val latestTestSucc = tSuccList.maxBy(_.getCreateTime)
          testCommentList.filter { _.Id != latestTestSucc.Id }.map { comment =>
            GithubAPI.deleteCommentOnIssue(comment.Id)
          }
        case _ => "Do nothing"
      }
    }

    def updateIssue: Issue = {
      var newIssue = new Issue(issue)
      newIssue.updateBState(bState)
      newIssue.updateTState(tState)
      newIssue.updateRStatus(rStatus)
      newIssue.labels.--=(List("tested", "reviewed"))
      newIssue.labels.++=(labelList)
      CoordBotUtil.updateLabelsOnIssue(issue.number, labelList.toList)
      newIssue.reviewList.++=(reviewList)
      newIssue.commentList.++=(commentList)
      Logger.debug(newIssue.toString())
      newIssue
    }

    //checkPullRequest
    scanIssueTitle
    scanIssueBody
    scanComments
    addComments
    deleteComments
    updateIssue
  }

  def updatedIssueCommentView(issueNumber: Long): Issue = {
    var issueTitle, issueBody = ""
    val issueString = GithubAPI.getIssue(issueNumber)
    val issue = com.codahale.jerkson.Json.parse[GithubAPIIssue](issueString)
    // update comment view
    checkIssueCommentView(issue)
  }

  def initIssueCommentView = {
    val issueString = GithubAPI.getOpenIssues
    val issues = com.codahale.jerkson.Json.parse[List[GithubAPIIssue]](issueString)
    for (issue <- issues) {
      issueMap = issueMap.+((issue.number, checkIssueCommentView(editIssue(issue))))
    }
    CoordBotUtil.printIssueMap
  }

  def setupEnv = {
    GithubAPI.initParameters(Config.gitHubUser, Config.gitHubPassword, Config.gitHubRepo, Config.gitHubUrl, Config.hookUrl)
    Logger.info("Github Webhook parameters: " + "\nUser: " + Config.gitHubUser + "\nRepository: " + Config.gitHubRepo + "\nHook url: " + Config.hookUrl)
    CoordBotUtil.deleteAllHooks
    GithubAPI.setupAllRepoHooks
  }
  
  def setupTempEnv = {
    CoordBotUtil.getLabels
    Logger.debug("Repo labels: " + totalLabelList.mkString(" "))
    val missinglabels = CoordBotUtil.missingLabels
    if (missinglabels.size != 0) {
      throw new MissingDefaultLabelsException("Default label(s) missing: " + missinglabels.mkString(" "))
    }
    //CoordBotUtil.getMilestones
  }

  def setupGithubEnv = {
    setupEnv
    setupTempEnv
    coordBotInit = false
  }
}
