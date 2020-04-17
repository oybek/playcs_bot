package io.github.oybek

import java.sql.Timestamp

import cats.effect.concurrent.Ref
import cats.effect.{Async, Concurrent, Sync, Timer}
import cats.syntax.all._
import cats.instances.option._
import io.github.oybek.config.Config
import io.github.oybek.domain.CmdStartCSDS
import io.github.oybek.service.{Octopus, ServerPool}
import org.slf4j.{Logger, LoggerFactory}
import telegramium.bots.client.Api
import telegramium.bots.high.LongPollBot

import scala.concurrent.duration._

class TgBot[F[_]: Async: Timer: Concurrent](serverPool: ServerPool[F])(implicit bot: Api[F])
  extends LongPollBot[F](bot) with TgExtractors {

  val log: Logger = LoggerFactory.getLogger("TgGate")

  import telegramium.bots._
  import telegramium.bots.client._

  private def nowAnd(minutes: FiniteDuration) =
    new Timestamp(System.currentTimeMillis() + minutes.toMillis)

  override def onMessage(message: Message): F[Unit] =
    Sync[F].delay { log.info(s"got message: $message") } *> (message match {
      case Text(x) if x.startsWith("/start") =>
        sendMessage(message.chat.id,
          """
            |Привет!
            |Я умею создавать выделенные серверы для Counter Strike 1.6.
            |Напиши /new de_dust2 30m - чтобы создать сервер на карте de_dust2 на 30 минут.
            |Так же меня можно добавить в беседу
            |""".stripMargin)

      case Text(`/new`(map, time)) =>
        for {
          serverO <- serverPool.poll(
            map,
            message.chat.id,
            nowAnd(time.filter(_.isDigit).toInt minutes)
          )
          reply = serverO match {
            case Some(server) =>
              s"""
                 |Сервер создан на ${server.theMap}, удачной игры! 😎
                 |Скопируйте последнюю строку и вставьте в консоль игры:
                 |connect ${server.ip}:${server.port}; password ${server.password}
                 |""".stripMargin
            case None =>
              s"""
                 |Слишком много обращений и поэтому кончилась оперативка на серваке,
                 |Не могу создать сервер - напишите @wolfodav пусть добавить еще RAM
                 |""".stripMargin
          }
          _ <- sendMessage(message.chat.id, reply)
        } yield ()

      case Text(x) if x.startsWith("/new") =>
        sendMessage(message.chat.id,
          """
            |Напишите /new <карта> <минуты>m - чтобы создать сервер
            |Например: /new de_dust 30m
            |""".stripMargin)

      case _ =>
        sendMessage(message.chat.id, "Не понял Вас")
    })

  private def sendMessage(chatId: Int, text: String): F[Unit] = {
    val sendMessageReq = SendMessageReq(chatId = ChatIntId(chatId), text = text)
    bot.sendMessage(sendMessageReq).void *>
      Sync[F].delay { log.info(s"send message: $sendMessageReq") }
  }

}
