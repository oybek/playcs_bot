package io.github.oybek

import cats.instances.option._
import cats.syntax.all._
import cats.effect._
import io.github.oybek.domain.{CmdStartCSDS, Server}
import io.github.oybek.service.{Octopus, ServerPool}

import scala.concurrent.duration._
import java.io.File
import java.util.concurrent.TimeUnit

import cats.effect.concurrent.Ref
import io.github.oybek.config.Config
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.middleware.Logger
import org.slf4j.LoggerFactory
import telegramium.bots.{Chat, Message}
import telegramium.bots.client.{Api, ApiHttp4sImp}

import scala.concurrent.ExecutionContext.Implicits.global

object PlayCS extends IOApp {

  type F[+T] = IO[T]

  private val log = LoggerFactory.getLogger("Main")

  def run(args: List[String]): IO[ExitCode] =
    for {
      configFile <- Sync[F].delay(Option(System.getProperty("application.conf")))
      config <- Config.load[F](configFile)

      poolRef <- Ref[F].of(List.empty[Server[F]])
      serverPool = new ServerPool[F](poolRef, config)
      _ <- serverPool.init()

      _ <- Sync[F].delay { log.info(s"loaded config: $config") }
      _ <- Sync[F].delay { log.info(s"starting service...") }
      _ <- resources
        .use { httpClient =>
          implicit val client   : Client[F] = Logger(logHeaders = false, logBody = false)(httpClient)
          implicit val tgBotApi : Api[F]    = new ApiHttp4sImp[F](client, s"https://api.telegram.org/bot${config.tgBotApiToken}")
          val tgBot = new TgBot[F](serverPool)
          tgBot.start
        }
    } yield ExitCode.Success

  def run(tgBot: TgBot[F]): F[Unit] = {
    for {
      _ <- Sync[F].delay(print("Enter command: "))
      s <- Sync[F].delay(scala.io.StdIn.readLine())
      _ <- tgBot.onMessage(
        Message(
          messageId = 0,
          date = 0,
          chat = Chat(0, ""),
          text = Some(s)
        )
      )
      _ <- run(tgBot)
    } yield ()
  }

  private def resources: Resource[F, Client[F]] =
    BlazeClientBuilder[F](global)
      .withResponseHeaderTimeout(FiniteDuration(60, TimeUnit.SECONDS))
      .resource
}

