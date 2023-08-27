// To be replaced by regular logging library

trait Logger:
	def log(info: String): Unit

object NoOpLogger extends Logger:
  def log(info: String): Unit = ()

object SimpleLogger extends Logger:
  def log(info: String): Unit = println(info)

object Logger:
  def log(using logger: Logger) = logger.log
  given Logger = NoOpLogger

