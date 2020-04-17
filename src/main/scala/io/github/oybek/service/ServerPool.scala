package io.github.oybek.service

import java.io.File
import java.sql.Timestamp

import cats.syntax.all._
import cats.effect.{Async, Concurrent, Sync, Timer}
import cats.effect.syntax.all._
import cats.instances.list._
import cats.instances.option._
import cats.effect.concurrent.Ref
import io.github.oybek.config.Config
import io.github.oybek.domain.{CmdStartCSDS, Server}

import scala.util.Random
import scala.concurrent.duration._
import io.github.oybek.util.TimeTools._
import org.slf4j.LoggerFactory

class ServerPool[F[_]: Async: Timer: Concurrent](poolRef: Ref[F, List[Server[F]]], config: Config) {

  private val log = LoggerFactory.getLogger("ServerPool")

  def init(): F[Unit] =
    for {
      pool <- createPool
      _ <- poolRef.update(_ => pool)
      _ <- check.every(1 minute).start.void
      _ <- Sync[F].delay { log.info(s"created ${pool.length} servers")}
    } yield ()

  def poll(map: String, chatId: Long, rentUntil: Timestamp): F[Option[Server[F]]] =
    poolRef.get.flatMap { pool =>
      split(
        pool,
        (srv: Server[F]) => srv.rentedBy.contains(chatId),
        (srv: Server[F]) => srv.rentedBy.isEmpty,
      ) match {
        case None =>
          Sync[F].delay(log.info(s"no server left in pool")) *>
            Sync[F].pure(Option.empty[Server[F]])

        case Some((x, xs)) =>
          refresh(x, map, Some(chatId), Some(rentUntil)).flatMap { srv =>
            Sync[F].delay(log.info(s"polled $srv")) *>
              poolRef.update(_ => srv::xs) *>
              Sync[F].pure(Option(srv))
          }
      }
    }

  def split[T](l: List[T], ps: (T => Boolean)*): Option[(T, List[T])] =
    ps
      .map(p => l.span(!p(_)))
      .collectFirst {
        case (xs, y::ys) => (y, xs ++ ys)
      }

  private def check: F[Unit] = {
    for {
      now <- Sync[F].delay(new Timestamp(System.currentTimeMillis()))
      pool <- poolRef.get
      _ <- Sync[F].delay(log.info("checking expired servers..."))
      poolC <- pool.foldLeftM(List.empty[Server[F]]) {
        case (acc, cur) =>
          Sync[F].delay(log.info(s"time $now, checking $cur...")) *> (
            if (cur.rentedUntil.exists(_.before(now))) refresh(cur).map(_ :: acc)
            else Sync[F].pure(cur :: acc)
          )
      }
      _ <- poolRef.update(_ => poolC)
    } yield ()
  }

  private def refresh(srv: Server[F],
                      map: String = "de_dust2",
                      rentedBy: Option[Long] = None,
                      rentUntil: Option[Timestamp] = None,
                      password: String = randomPassword) =
    for {
      srv2 <- Sync[F].delay(srv.copy(
        theMap = map,
        rentedBy = rentedBy,
        rentedUntil = rentUntil,
        password = password
      ))
      _ <- Sync[F].delay(log.info(s"$srv -> $srv2"))
      _ <- srv2.octopus.push(s"sv_password ${srv2.password}")
      _ <- Timer[F].sleep(200 millis)
      _ <- srv2.octopus.push(s"map ${srv2.theMap}")
    } yield srv2

  private def createPool: F[List[Server[F]]] =
    (0 until config.serverPoolSize).toList.traverse { i =>
      val ip = config.serverIp
      val port = 27015 + i
      Octopus
        .run(CmdStartCSDS(new File(config.hldsDir), port))
        .map(Server(ip, port, "", "", None, None, _))
        .flatMap(refresh(_))
    }

  private def randomPassword: String = (Random.nextInt(90000) + 10000).toString
}
