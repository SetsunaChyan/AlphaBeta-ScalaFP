import util.{Cons, Empty, ListOf, combine}

/**
 * Created by Setsuna on 2021/11/24 11:04
 */
object ListOfTest extends App {
    val list = Cons(1, Cons(5, Cons(9, Empty)))
    println(list) // [1 5 9]
    println(list.reduce((x: Int, y: Int) => x + y, 0)) // 15
    println(list.reduce((x: Int, y: Int) => x * y, 1)) // 45
    println(list.map((x: Int) => x * x).reduce((x: Int, y: Int) => x + y, 0)) // 107
    val list2 = Cons(7, Cons(2, Empty))
    println(list.reduce(Cons.cons[Int], list2)) // [1 5 9 7 2]
}
