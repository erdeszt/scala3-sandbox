package sandbox

import java.net.DatagramSocket
import scala.annotation.tailrec
import java.net.DatagramPacket
import java.net.InetAddress
import fs2._
import cats.effect.{IO, IOApp}
import cats.effect.std.Queue
import cats.effect.kernel.Resource

val port = 12345

final case class Sender(
    address: InetAddress,
    port:    Int
)

final case class Packet(
    message: String,
    sender:  Sender
)

object StreamingServer extends IOApp.Simple:
  def receiveLoop(socket: DatagramSocket, receiveBuffer: Array[Byte], queue: Queue[IO, Packet]): IO[Unit] =
    def go(): IO[Unit] =
      for
        receivePacket <- IO(new DatagramPacket(receiveBuffer, receiveBuffer.length))
        _ <- IO(socket.receive(receivePacket))
        message <- IO(new String(receivePacket.getData, 0, receivePacket.getLength, "utf-8"))
        _ <- queue.offer(Packet(message, Sender(receivePacket.getAddress, receivePacket.getPort)))
        _ <- go()
      yield ()
    go()

  val buffer = new Array[Byte](1024)

  val run =
    for
      queue <- Queue.unbounded[IO, Packet]
      stream        = Stream.fromQueueUnterminated(queue)
      managedSocket = Resource.fromAutoCloseable(IO(new DatagramSocket(port)))
      socketThread <- managedSocket.use(receiveLoop(_, buffer, queue)).start
      _ <- stream.evalMap(packet => IO(println(s"Got: ${packet}"))).compile.drain
    yield ()

object StreamingClient extends IOApp.Simple:

  def udp(socket: DatagramSocket, host: InetAddress, port: Int): Pipe[IO, String, String] =
    _.evalTap { (message: String) =>
      val buffer = message.getBytes("utf-8")
      for
        packet <- IO(new DatagramPacket(buffer, buffer.length, host, port))
        _ <- IO(socket.send(packet))
      yield ()
    }

  val run =
    val managedSocket = Resource.fromAutoCloseable(IO(new DatagramSocket()))
    val stream        = Stream.emits[IO, String](List("hi 1", "yo 2", "foo 3"))
    for
      host <- IO(InetAddress.getByName("localhost"))
      udpThread <- managedSocket.use(socket => stream.through(udp(socket, host, port)).compile.drain)
      _ <- IO(println("Done"))
    yield ()

@main
def listenerExample(): Unit =
  val socket        = new DatagramSocket(port)
  val receiveBuffer = new Array[Byte](1024)
  listenerLoop(socket, receiveBuffer)

@main
def senderExample(): Unit =
  val host   = InetAddress.getByName("localhost")
  val socket = new DatagramSocket()
  val buffer = "Hi there!".getBytes("utf-8")
  val packet = new DatagramPacket(buffer, buffer.length, host, port)
  socket.send(packet)

@tailrec
def listenerLoop(socket: DatagramSocket, receiveBuffer: Array[Byte]): Unit =
  val receivePacket = new DatagramPacket(receiveBuffer, receiveBuffer.length)
  socket.receive(receivePacket)
  val receiveData   = new String(receivePacket.getData, 0, receivePacket.getLength, "utf-8")
  val senderAddress = receivePacket.getAddress
  val senderPort    = receivePacket.getPort
  println(s"Got message from: ${senderAddress}:${senderPort}")
  println(s"Message: ${receiveData}")
  listenerLoop(socket, receiveBuffer)
