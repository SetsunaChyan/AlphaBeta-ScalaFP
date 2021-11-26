import engine.{Game, Position, TicTacToeGame}

import scala.io.StdIn

/**
 * Created by Setsuna on 2021/11/25 11:28
 */
object Main extends App {
    def InputLoop(p: Int => Boolean, s: String): Int = {
        print(s)
        val ret = StdIn.readInt()
        if (p(ret)) ret
        else InputLoop(p, s)
    }

    println("TicTacToe-AlphaBeta")
    println("0: Offensive Position\n1: Defensive Position")
    val playerTurn = InputLoop((x: Int) => x == 0 || x == 1, "Your choice: ")
    val pos = TicTacToeGame.InitPos(playerTurn == 1)

    def mainLoop(turn: Int, pos: Position): Unit = {
        println(pos)
        val v = TicTacToeGame.static(pos)
        if (pos.isWin()) {
            printf("%s Won!\n", if (pos.isAITurn) "You" else "AI")
            return
        } else if (pos.countBlank() == 0) {
            println("Tie.")
            return
        }
        if (turn == playerTurn) {
            def getNextMove(p: (Int, Int) => Boolean, s: String): Position = {
                print(s)
                val Array(x,y):Array[Int] = StdIn.readLine.split(" ").map(_.toInt-1)
                if (p(x, y)) pos.move(x, y, false)
                else getNextMove(p, s)
            }

            mainLoop(turn ^ 1, getNextMove(pos.canMove, "Your turn: (x[1,3]: line, y[1,3]: column)\n"))
        } else {
            println("AI's turn:")
            mainLoop(turn ^ 1, TicTacToeGame.bestMove(pos))
        }
    }

    mainLoop(0, pos)
}

