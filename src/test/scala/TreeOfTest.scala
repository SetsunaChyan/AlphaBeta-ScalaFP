import util.{Cons, Stream, ListOf, Node}

/**
 * Created by Setsuna on 2021/11/24 12:56
 */
object TreeOfTest extends App {
    val tree = Node(1, Stream(
        Node(2, Stream(
            Node(4, Stream.empty), Stream(
                Node(5, Stream.empty), Stream.empty))), Stream(
            Node(3, Stream.empty), Stream(
                Node(6, Stream.empty), Stream.empty))
    ))
    println(tree) // 1 [2 [4 5] 3 6]

    def sum(x: Int, y: => Int): Int = x + y
    println(tree.reduce(sum, sum, 0)) // 21

    def append[T](l1: Stream[T], l2: => Stream[T]) =
        l1.reduce(Stream.cons[T], l2)
    println(tree.reduce(Stream.cons[Int], append[Int], Stream.empty)) // [1 2 4 5 3 6]

    println(tree.map((x:Int)=>(x*x).toString)) // 1 [4 [16 25] 9 36]
}
