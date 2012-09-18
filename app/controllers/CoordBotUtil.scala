package controllers

import java.util.Calendar
import java.util.Date
import org.joda.time.format.ISODateTimeFormat
import controllers.github._
import play.api.Logger
import models._
import math._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

object CoordBotUtil {

  var totalLabelList = new ListBuffer[String]
  var milestoneMap: Map[String, Long] = new HashMap[String, Long]
  var branchList = new ListBuffer[String]

  val dateParser = ISODateTimeFormat.dateTimeNoMillis();
  def parseISO8601(date: String): Date = {
    dateParser.parseDateTime(date).toDate()
  }
  def milliSecToSec(milliSec: Long): Long = { milliSec / 1000 }
  def milliSecToMin(milliSec: Long): Long = { milliSec / (1000 * 60) }
  def milliSecToHour(milliSec: Long): Long = { milliSec / (1000 * 60 * 60) }
  def milliSecToDay(milliSec: Long): Long = { milliSec / (1000 * 60 * 60 * 24) }

  def editIssue(issue: GithubAPIIssue): GithubAPIIssue = {
    var newIssueBody = ""
    val links = missingJIRALinks(issue.body, JIRATickets(issue.title))
    if (links.size > 0) {
      Logger.debug("Add JIRA links to the body of issue " + issue.number)
      newIssueBody = links.mkString("", "\n", "\n") + issue.body
      GithubAPI.editIssueBody(issue.number, newIssueBody)
    }
    issue.copy(body = newIssueBody)
  }

  def getCommentsOnIssue(issueNumber: Long): List[Comment] = {
    val commentString = GithubAPI.getCommentsOnIssue(issueNumber)
    val comments = com.codahale.jerkson.Json.parse[List[GithubAPIComment]](commentString)
    comments.map { comment =>
      new Comment(comment)
    }
  }

  def processComment(comment: Comment): (BuildState, TestState, ReviewStatus) = {
    val msg = comment.Body
    val bstate = if (comment.User == Config.buildBot) getBuildState(msg) else BuildNone
    val tstate = if (comment.User == Config.buildBot) getTestState(msg) else TestNone
    val rstatus = getReviewStatus(msg)
    (bstate, tstate, rstatus)
  }

  def getCommentType(bState: BuildState, tState: TestState, rStatus: ReviewStatus): CommentType = {
    bState match {
      case BuildNone => "Not build comment"
      case _ => return BuildComment
    }
    tState match {
      case TestNone => "Not test comment"
      case _ => return TestComment
    }
    rStatus match {
      case ReviewNone => "Not review comment"
      case _ => return ReviewComment
    }
    UnknownComment
  }

  def getReviewStatus(msg: String): ReviewStatus = {
    if (coordBotMsg(msg)) {
      val reviewWarning = "unrecognized reviewers"
      if (msg.contains(reviewWarning)) {
        return ReviewWarning
      }
      return ReviewNone
    }
    if (msg.contains("@") && reviewMsg(msg)) {
      var tokens = new ListBuffer[String]
      var rstatus: ReviewStatus = ReviewOpen
      tokens.++=(msg.split(" "))
      val reviewers = tokens.filter(token => token.contains("@")).map(_.drop(1)) // assume all reviewers are specified in one comment
      Logger.debug("Specified reviewers: " + reviewers.mkString(" "))
      for (token <- reviewers; if !Config.reviewerList.contains(token)) {
        rstatus = ReviewFault
        rstatus.reviewers = reviewers
        return rstatus
      }
      rstatus.reviewers = reviewers
      rstatus
    } else if (reviewedMsg(msg))
      ReviewDone
    else
      ReviewNone
  }

  def reviewMsg(msg: String): Boolean = {
    for (token <- Config.reviewMsgList; if (msg.contains(token)))
      return true
    false
  }

  def reviewedMsg(msg: String): Boolean = {
    for (token <- Config.reviewedMsgList; if (msg.contains(token)))
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

  def coordBotMsg(msg: String): Boolean = {
    return msg.contains("Web Bot:")
  }

  def getBuildState(msg: String): BuildState = {
    if (coordBotMsg(msg)) {
      Logger.debug("Ignore web bot message")
      return BuildNone
    }
    val botMsg = "jenkins job"
    val buildMsg = "pr-rangepos"
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
      } else
        BuildNone
    } else
      BuildNone
  }

  def getTestState(msg: String): TestState = {
    if (coordBotMsg(msg)) {
      Logger.debug("Ignore web bot message")
      return TestNone
    }
    val botMsg = "jenkins job"
    val testMsg = "pr-scala-testsuite-linux-opt"
    val startMsg = "Started"
    val successMsg = "Success"
    if (msg.contains(botMsg)) {
      if (msg.contains(testMsg)) {
        if (msg.contains(startMsg))
          TestStart
        else if (msg.contains(successMsg))
          TestSuccess
        else
          TestFailure
      } else
        TestNone
    } else
      TestNone
  }

  def updateLabelsOnIssue(issueNumber: Long, labelList: List[String]) = {
    val existedLabelsSet = getLabelsOnIssue(issueNumber).toSet
    val newLabelsSet = labelList.toSet
    val labelsToAdd = newLabelsSet.diff(existedLabelsSet)
    val labelsToDelete = existedLabelsSet.diff(newLabelsSet)
    deleteLabelsOnIssue(issueNumber, labelsToDelete.toList)
    addLabelsOnIssue(issueNumber, labelsToAdd.toList)
  }

  def getLabels = {
    val labelString = GithubAPI.getLabels
    val labels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    for (label <- labels) {
      totalLabelList.+=(label.name)
    }
  }

  def missingLabels: List[String] = {
    val missingLabels = for (label <- Config.defaultLabelList; if (!totalLabelList.contains(label)))
      yield label
    missingLabels
  }

  def addLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    // in case adding an already existed issue would produce a HTTP error
    val labels = githublabels.map { githublabel => githublabel.name }
    if (!labels.contains(label))
      GithubAPI.addLabelOnIssue(issueNumber, label)
  }

  def addLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels)
      addLabelOnIssue(issueNumber, label)
  }

  def getLabelsOnIssue(issueNumber: Long): List[String] = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    val labels = githublabels.map { githublabel => githublabel.name }
    labels
  }

  def deleteLabelOnIssue(issueNumber: Long, label: String) = {
    val labelString = GithubAPI.getLabelsOnIssue(issueNumber)
    val githublabels = com.codahale.jerkson.Json.parse[List[GithubAPILabel]](labelString)
    // in case adding an already existed label would produce a HTTP error
    val labels = githublabels.map { githublabel => githublabel.name }
    if (labels.contains(label)) {
      GithubAPI.deleteLabelOnIssue(issueNumber, label)
    }
  }

  def deleteLabelsOnIssue(issueNumber: Long, labels: List[String]) = {
    for (label <- labels) {
      Logger.debug("Delete label " + label)
      deleteLabelOnIssue(issueNumber, label)
    }
  }

  def getAllHooks: List[GithubAPIHook] = {
    val hookString = GithubAPI.getHooks
    val hooks = com.codahale.jerkson.Json.parse[List[GithubAPIHook]](hookString).sortWith((a, b) => a.id < b.id)
    hooks
  }

  def deleteAllHooks = {
    getAllHooks.map { hook => GithubAPI.deleteHook(hook.id) }
  }

  def checkExpiredReviewWarning(reviewWarning: Comment): Boolean = {
    def checkDay: Boolean = { CoordBotUtil.milliSecToDay(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > Config.waitDayInterval }
    def checkHour: Boolean = { CoordBotUtil.milliSecToHour(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > Config.waitHourInterval }
    def checkMin: Boolean = { CoordBotUtil.milliSecToMin(abs(reviewWarning.getCreateTime - Calendar.getInstance.getTime().getTime())) > Config.waitMinInterval }
    def check: Boolean = checkHour
    check
  }

  def getMilestones = {
    val milestoneString = GithubAPI.getMilestones
    val milestones = com.codahale.jerkson.Json.parse[List[GithubAPIMilestone]](milestoneString)
    for (milestone <- milestones) {
      milestoneMap = milestoneMap.+=((milestone.title, milestone.number))
    }
    Logger.debug("Get milestones done: " + milestoneMap.toString())
  }
  
  def getBranches = {
    val branchString = GithubAPI.getBranches
    val branches = com.codahale.jerkson.Json.parse[List[GithubAPIBranch]](branchString)
    for (branch <- branches) {
      branchList.+=(Config.gitHubUser+":"+branch.name)
    }
    Logger.debug("Get branches done:" + branchList.toString())
  }

  def setupEnv = {
    GithubAPI.initParameters(Config.gitHubUser, Config.gitHubPassword, Config.gitHubRepo, Config.gitHubUrl, Config.hookUrl)
    Logger.info("Github Webhook parameters: " + "\nUser: " + Config.gitHubUser + "\nRepository: " + Config.gitHubRepo + "\nHook url: " + Config.hookUrl)
    CoordBotUtil.deleteAllHooks
    GithubAPI.setupAllRepoHooks
  }

  def setupTempEnv = {
    totalLabelList.clear
    getLabels
    Logger.debug("Repo labels: " + totalLabelList.mkString(" "))
    val missinglabels = missingLabels
    if (missinglabels.size != 0) {
      throw new MissingDefaultLabelsException("Default label(s) missing: " + missinglabels.mkString(" "))
    }
    getMilestones
    getBranches
    if (!validMilestoneMapping)
      throw new MissingMilestoneMappingException("Invalid milestone mapping")
  }
  
  def validMilestoneMapping: Boolean = {
    for (branch <- Config.pullRequestMilestoneMap.keySet) {
      if (!branchList.contains(branch)) return false
      Config.pullRequestMilestoneMap.get(branch) match {
        case None => return false;
        case _ => "OK"
      }
    }
    return true
  }

  def editIssueMilestone(issueNumber: Long) = {
    val pullString = GithubAPI.getPullRequest(issueNumber)
    val pull = com.codahale.jerkson.Json.parse[GithubAPIPullRequest](pullString)
    Config.pullRequestMilestoneMap.get(pull.base.label) match {
      case Some(milestone) =>
        milestoneMap.get(milestone) match {
          case Some(milestoneNumber) =>
            GithubAPI.editIssueMilestone(issueNumber, milestoneNumber)
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
  }
}