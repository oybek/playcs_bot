package io.github.oybek.domain

import java.sql.Timestamp

import io.github.oybek.service.Octopus

sealed trait Status
case object BeingUsed extends Status
case object Reserved extends Status

case class Server[F[_]](ip: String,
                        port: Int,
                        theMap: String,
                        password: String,
                        rentedBy: Option[Long],
                        rentedUntil: Option[Timestamp],
                        octopus: Octopus[F])
