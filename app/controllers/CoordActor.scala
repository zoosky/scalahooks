package controllers
import akka.actor.Actor
import play.api.Logger


class CoordActor extends Actor {
  def receive = {
    case "refresh" =>
      Logger.info("Actor checks issue-comment view")
      CoordBot.actorCheckIssueCommentView
  }
}