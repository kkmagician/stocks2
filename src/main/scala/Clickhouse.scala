import org.http4s.Uri

case class Clickhouse(host: Uri, user: String, password: String, table: String)
