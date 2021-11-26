package util

/**
 * Created by Setsuna on 2021/11/25 9:10
 */
trait Stream[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: Stream[T]

    def reduce[A](f: (T, => A) => A, zero: A): A =
        if (this.isEmpty) zero
        else f(head, tail.reduce(f, zero))

    def map[A](f: T => A): Stream[A] = {
        def tmp(hd: T, tl: => Stream[A]): Stream[A] = Stream.cons[A](f(hd), tl)

        reduce(tmp, Stream.empty)
    }

    def filter(p: T => Boolean): Stream[T] = {
        if (isEmpty) this
        else if (p(head)) Stream.cons(head, tail.filter(p))
        else tail.filter(p)
    }

    def take(n: Int): ListOf[T] = {
        if (isEmpty || n == 0) Empty
        else Cons(head, tail.take(n - 1))
    }

    def printElement(n: Int): String =
        if (isEmpty) ""
        else if (n == 0) "..."
        else head.toString + " " + tail.printElement(n - 1)

    override def toString: String = "[" + printElement(3) + "]"
}

object Stream {
    def apply[T](hd: T, tl: => Stream[T]): Stream[T] = cons(hd, tl)
    def cons[T](hd: T, tl: => Stream[T]): Stream[T] = new Stream[T] {
        def isEmpty = false
        def head: T = hd
        def tail: Stream[T] = tl
    }
    val empty: Stream[Nothing] = new Stream[Nothing] {
        def isEmpty = true
        def head = throw new NoSuchElementException("empty.head")
        def tail = throw new NoSuchElementException("empty.tail")
    }
    def take[T](f: Int => T, n: Int, now: Int = 0): Stream[T] =
        if (n == now) Stream.empty
        else Stream(f(now), take(f, n, now + 1))
}
