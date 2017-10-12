import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.util.ByteString
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
import scala.io.Source
import scala.util.parsing.json._

/**
  * xkcd Telegram bot
  */
class XkcdBot(tokenArg: String) extends TelegramBot
  with Polling
  with Commands
  with ChatActions {

  onCommand("/give") { implicit msg =>
    withArgs { args =>
      val url = "https://xkcd.com/" + (args mkString "/") + "/info.0.json"
      val rawJSON = Source.fromURL(url).mkString
      val actJSON = JSON.parseFull(rawJSON)
      val imgURL = actJSON.get.asInstanceOf[Map[String, Any]]("img").asInstanceOf[String]
      for {
        res <- Http().singleRequest(HttpRequest(uri = Uri(imgURL)))
        if res.status.isSuccess()
        bytes <- Unmarshal(res).to[ByteString]
      } /* do */ {
        val photo = InputFile("img.png", bytes)
        uploadingPhoto // Hint the user
        request(SendPhoto(msg.source, photo))
      }
    }
  }

  override def token: String = tokenArg
}

object MainBot extends App {
  new XkcdBot(args(0)).run()
}
