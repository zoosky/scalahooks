package models
import scala.collection.mutable.LinkedList

case class MalFormedJSONPayloadException(ex: String) extends RuntimeException {
  override def toString: String = ex
}

class Comment (action: CommentAction, id: Long, body: String, createTime: String, updateTime: String) {
  def Id: Long = {id}
  def Body: String = {body}
  def CreateTime: String = {createTime}
  def UpdateTime: String = {updateTime}
}

class Issue (number: Long, title: String, body: String) {
  val minInterval = 60
  val waitInterval = minInterval * 48
  var rStatus: ReviewStatus = ReviewNone
  var rCounter: Long = -1
  var bState: BuildBotState = UnknownBuildBotState
  var labels = LinkedList[String]()
  var commentList = new LinkedList[Comment]()
  def insertComment(comment: Comment) = {commentList = commentList.:+(comment)}
  def insertComments(comments: List[Comment]) = {commentList = commentList.++:(comments)}
  def clearComments = {commentList = commentList.drop(commentList.size)}
  def Number: Long = {number}
  def Title: String = {title}
  def Body: String = {body}
  def getBState: BuildBotState = {bState}
  def updateBState(state: BuildBotState) = {bState = state}
  def getRStatus: ReviewStatus = {rStatus}
  def updateRStatus(status: ReviewStatus) = {rStatus = status}
  def setRCounter(counter: Long) = {rCounter = counter}
  def checkRStatus: ReviewStatus = {
    rStatus match {
      case ReviewNone => "Do nothing";        rStatus
      case ReviewFault => "Do nothing";       rStatus
      case ReviewOpen => "Wait until tested"; rStatus
      case ReviewWait => rCounter -= minInterval 
        if (rCounter < 0) {
          rStatus = ReviewExpired; rStatus
        } 
        else  
          rStatus
      case ReviewExpired => rStatus 
      case ReviewDone => rStatus
    }
  }
  override def toString(): String = {
    val issueString = "Issue title: " + this.title + "\n" +
                      "Issue body: "  + this.body  + "\n"
    var commentString = ""
    commentList.map(comment => commentString += comment.Body)
    if (commentString != "") commentString = "Comment body: " + commentString
    issueString + commentString
  }
}

sealed trait BuildBotState {
  override def toString() = BuildBotState.mapToString(this)

  def describe =
    this match {
      case BuildStart             => "build started"
      case BuildSuccess           => "build successful"
      case BuildFailure           => "build failed"
      case TestStart              => "test started"
      case TestSuccess            => "test successful"
      case TestFailure            => "test failed"
      case SameBuildBotState      => "same state"
      case UnknownBuildBotState   => "unknown state"
    }
}

object BuildBotState {
  val mapToString: Map[BuildBotState, String] = Map(
    BuildStart             -> "build started",
    BuildSuccess           -> "build successful",
    BuildFailure           -> "build failed",
    TestStart              -> "test started",
    TestSuccess            -> "test successful",
    TestFailure            -> "test failed",
    UnknownBuildBotState   -> "unknown state"
  )

  def apply(s: String): BuildBotState = mapToString.map(_.swap).apply(s)
}

case object BuildStart            extends BuildBotState 
case object BuildSuccess          extends BuildBotState
case object BuildFailure          extends BuildBotState
case object TestStart             extends BuildBotState
case object TestSuccess           extends BuildBotState
case object TestFailure           extends BuildBotState
case object SameBuildBotState     extends BuildBotState
case object UnknownBuildBotState  extends BuildBotState

sealed trait ReviewStatus {}

object ReviewStatus {}

case object ReviewNone            extends ReviewStatus
case object ReviewFault           extends ReviewStatus
case object ReviewOpen            extends ReviewStatus
case object ReviewWait            extends ReviewStatus
case object ReviewExpired         extends ReviewStatus
case object ReviewDone            extends ReviewStatus

sealed trait CommentAction {}

object CommentAction {}

case object CommentCreated        extends CommentAction
case object CommentDeleted        extends CommentAction

