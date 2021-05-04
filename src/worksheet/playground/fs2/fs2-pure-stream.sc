import fs2.{Chunk, Stream}
import fs2.io.net.Network
import cats.effect.MonadCancelThrow
import cats.effect.std.Console
import cats.syntax.all._
import com.comcast.ip4s._

def client[F[_]: MonadCancelThrow: Console: Network]: F[Unit] =
  implicitly[Network[F]].client(SocketAddress(host"localhost", port"5555")).use { socket =>
    socket.write(Chunk.array("Hello, world!".getBytes)) >>
      socket.read(8192).flatMap { response =>
        Console[F].println(s"Response: $response")
      }
  }