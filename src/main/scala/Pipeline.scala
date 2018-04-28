object Pipeline {
  implicit class PipelineContainer[F](val value: F) extends AnyVal {
    def |>[G] (f: F => G) = f(value)
  }
}
