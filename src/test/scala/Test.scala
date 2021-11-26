import engine.Game.lazyMax
import engine.{Game, TicTacToeGame}

/**
 * Created by Setsuna on 2021/11/25 21:43
 */
object Test extends App {
    def playerTurn = 1
    var p=TicTacToeGame.InitPos(playerTurn == 1)

    val l=Game.maximise_(Game.prune(TicTacToeGame.MaxDep)(TicTacToeGame.gameTree(p)).map(TicTacToeGame.static))
    println(l)
    //println(l.reduce(lazyMax, l.sons.head.label))
    //p=p.move(1,1)
    //p=p.move(0,1)
    //println(p)
    //p=p.move(0,0)
    //p=p.move(2,2)
    //println(TicTacToeGame.bestMove(p))
}
