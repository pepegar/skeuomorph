package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
final case class LogicalTypes(dec: _root_.scala.math.BigDecimal, maybeDec: _root_.scala.Option[_root_.scala.math.BigDecimal], ts: _root_.java.time.Instant, dt: _root_.java.time.LocalDate)
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait LogicalTypes[F[_]] { def identity(req: _root_.foo.bar.LogicalTypes): F[_root_.foo.bar.LogicalTypes] }