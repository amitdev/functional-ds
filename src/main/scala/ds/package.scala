package object ds {
  type Orderable[T] = T => Ordered[T]
}