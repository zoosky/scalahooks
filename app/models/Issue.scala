package models
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.util.Date
import controllers.github._
import controllers.CoordBotUtil

case class MalFormedJSONPayloadException(msg: String) extends RuntimeException {
  override def toString: String = msg
}

case class MissingDefaultLabelsException(msg: String) extends RuntimeException {
  override def toString: String = msg
}

case class Comment (comment: GithubAPIComment) {
  def Id: Long = {comment.id}
  def Body: String = {comment.body}
  def CreateTime: String = {comment.created_at.toString()}
  def getCreateTime: Long = {CoordBotUtil.parseISO8601(comment.created_at).getTime}
  def UpdateTime: String = {comment.updated_at.toString}
  def getUpdateTime: Long = {CoordBotUtil.parseISO8601(comment.updated_at).getTime}
  def User: String = {comment.user.login}
  override def toString(): String = {comment.body}
}

case class Review(reviewer: String, rStatus: ReviewStatus) {
  def getReviewer = reviewer.drop(1)
}

case class Issue (issue: GithubAPIIssue) {
  private var rStatus: ReviewStatus = ReviewNone
  private var bState: BuildState = BuildNone
  private var tState: TestState = TestNone
  var labels = new ListBuffer[String]
  var commentList = new ListBuffer[Comment]
  var reviewList = new ListBuffer[Review]
  def Number: Long = {issue.number}
  def Title: String = {issue.title}
  def Body: String = {issue.body}
  def getBState: BuildState = {bState}
  def updateBState(state: BuildState) = {bState = state}
  def getTState: TestState = {tState}
  def updateTState(state: TestState) = {tState = state}
  def getRStatus: ReviewStatus = {rStatus}
  def updateRStatus(status: ReviewStatus) = {rStatus = status}
  override def toString(): String = {
    val issueString = "Issue number: " + issue.number.toString   + "\n" +
                      "Issue title: "  + issue.title             + "\n" +
                      "Issue body: "   + issue.body              + "\n"
    val commentString = "Comment body: \n" + (if (commentList.size == 0) "No comment" else commentList.mkString("\n"))
    issueString + commentString
  }
  def describe: String = {
    "<< " + issue.title + " >> "
  }
  def isTested: Boolean = {
    return labels.contains("tested")
  }
  def isReviewed: Boolean = {
    return labels.contains("reviewed")
  }
}

sealed trait BuildState {
  override def toString = BuildState.mapToString(this)
}

object BuildState {
  val mapToString: Map[BuildState, String] = Map(
    BuildNone              -> "build none",
    BuildStart             -> "build started",
    BuildSuccess           -> "build successful",
    BuildFailure           -> "build failed"
  )

  def apply(s: String): BuildState = mapToString.map(_.swap).apply(s)
}

case object BuildNone             extends BuildState
case object BuildStart            extends BuildState 
case object BuildSuccess          extends BuildState
case object BuildFailure          extends BuildState

sealed trait TestState {
  override def toString = TestState.mapToString(this)
}

object TestState {
  val mapToString: Map[TestState, String] = Map(
    TestNone               -> "test none",
    TestStart              -> "test started",
    TestSuccess            -> "test successful",
    TestFailure            -> "test failed"
  )

  def apply(s: String): TestState = mapToString.map(_.swap).apply(s)
}

case object TestNone              extends TestState
case object TestStart             extends TestState
case object TestSuccess           extends TestState
case object TestFailure           extends TestState

sealed trait ReviewStatus {
  var reviewers = new ListBuffer[String]
  var msg = ""
}
object ReviewStatus

case object ReviewNone            extends ReviewStatus
case object ReviewOpen            extends ReviewStatus 
case object ReviewFault           extends ReviewStatus 
case object ReviewDone            extends ReviewStatus
case object ReviewWarning         extends ReviewStatus

sealed trait CommentAction {}
object CommentAction {}

case object CommentCreated        extends CommentAction
case object CommentDeleted        extends CommentAction

sealed trait CommentType {}
object CommentType {}

case object UnknownComment             extends CommentType           
case object BuildComment          extends CommentType
case object TestComment           extends CommentType
case object ReviewComment         extends CommentType


