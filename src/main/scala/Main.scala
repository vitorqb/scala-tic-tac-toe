/** The main class */
object TicTacToe {

  /** Welcomes the user and calls the play loop */
  def main(args: Array[String]): Unit = {
    println("WELCOME to the Tic-Tac-Toe!\n")
    println("Let's do it!")
    play_loop()
  }

  /** Keeps playing and asking the user if he want to repeat after
    * each game.
    */
  def play_loop(): Unit = {

    def ask(print_question: Boolean): Boolean = {
      if (print_question) {
        print("Would you like to play again?[y|n] ")
      }
      scala.io.StdIn.readLine() match {
        case "y" | "Y" | "yes" => true
        case "n" | "N" | "no"  => false
        case _ => {
          print("\nSorry, I don't understand. Yes or no? ")
          ask(false)
        }
      }
    }

    Game.play(Game.new_game)
    ask(true) match {
      case true => play_loop()
      case false => println("Bye bye")
    }
  }
}


//Represents the players
sealed abstract class Player
case class Computer() extends Player
case class Person() extends Player


//Represents a cell
sealed abstract class Cell
case class EmptyCell() extends Cell
case class MarkedCell(val player: Player) extends Cell


//represents a move
sealed abstract class Move
case class ValidMove(new_cells: List[Cell]) extends Move
case class InvalidMove(err_msg: String) extends Move

//Knows how to make a new move
object MoveMaker {

  /** Makes a move for game g trying to mark cell i */
  def makeMove(i: Int, g: GameStatus): Move = {
    if (i > 9 || i < 1) {
      InvalidMove("Please choose a number between 1 and 9, inclusive")
    } else {
      g.cells(i-1) match {
        case EmptyCell() => ValidMove(g.cells.updated(i-1, MarkedCell(g.player)))
        case MarkedCell(_) => InvalidMove("This cell is already marked!")
      }
    }
  }
}


/** Hosts the current board and the players that is currently playing. */
case class GameStatus(cells: List[Cell], player: Player) {

  //The board is represented by a list of 9 cells

  /** Constructs from a valid move and a previous game */
  def this(m: ValidMove, g: GameStatus) {
    this(m.new_cells, g.next_player)
  }
 
  /** The next game! */
  def nextGame(newCells: List[Cell]): GameStatus =
    GameStatus(newCells, next_player)

  /** Get the next player to play */
  def next_player = player match {
    case Person() => Computer()
    case Computer() => Person()
  }

  /** Returns a list with the rows, each row a list of 3 cells */
  def rows: List[List[Cell]] = 
    List.tabulate(3)(i => List.tabulate(3)(j => cells(3 * i + j)))

  /** Returns a list with the columns, each column a list of 3 cells */
  def cols: List[List[Cell]] =
    List.tabulate(3)(i => List.tabulate(3)(j => cells(i + 3 * j)))

  /** Returns the diagonals */
  def crosses: List[List[Cell]] =
    List(
      List.tabulate(3)(i => cells(i * 4)),
      List.tabulate(3)(i => cells(2 + i * 2))
    )

  /** Returns the winner, if any */
  def winner: Option[Player] = {
    def reduceWinner(l: List[Option[Player]]): Option[Player] = l match {
      case Nil => None
      case None :: xs => reduceWinner(xs)
      case Some(p) :: _ => Some(p)
    }
    reduceWinner((rows ::: cols ::: crosses).map(threeCellsWinner))
  }

  /** Does this sequence of 3 cells belong to the same player? */
  def threeCellsWinner(cells: List[Cell]): Option[Player] =
    if ( cells.length != 3) {
      throw new Exception("Cells should have length 3")
    } else {
      cells match {
        case MarkedCell(p1) :: MarkedCell(p2) :: MarkedCell(p3) :: Nil
            if p1 == p2 && p1 == p3 => Some(p1)
        case _ => None
      }
    }
}


//Represents a game
object Game {

  /** How much span (spaces) should a cell have? */
  val CellHorizontalSpan: Int = 2
  val CellVerticalSpan: Int = 1

  /** The columns separator */
  val ColumnSeparator: String = "|"

  /** The total width of a line 
    * 
    *  initial separator + 3 cells
    *  each cell has (span + marker + span + separator)
    */
  def totalWidth : Int = {
    val nspan = CellHorizontalSpan
    val nmarker = 1
    val nseparator = ColumnSeparator.length
    nseparator + 3 * (nspan + nmarker + nspan + nseparator)
  }

  /** Returns a new clean game */
  def new_game : GameStatus = {
    println("A new game has started!")
    new GameStatus(List.fill(9)(new EmptyCell), new Person)
  }

  /** Plays the game */
  def play (g: GameStatus): Unit = {
    GamePrinter.print_game(g)
    g.winner match {
      case Some(Person())   => println("Errr.. You won? Fuck you won.")
      case Some(Computer()) => println("Yep. You LOST. I WON.")
      case None => {
        if ( ! g.cells.exists(_.isInstanceOf[EmptyCell]) ) {
          println("Game over. Nobody wins.")
        } else {
          g.player match {
            case Person() => personPlay(g)
            case Computer() => computerPlay(g)
          }
        }
      }
    }
  }

  /** When a person is playing */
  def personPlay (g: GameStatus): Unit = {
    def askChoice(): Int = {
      print("Choose an option [1-9]! ")
      try {
        scala.io.StdIn.readInt()
      } catch {
        case e: Exception => {
          println("I didn't understand! Try again:")
          askChoice()
        }
      }
    }
    val choice: Int = askChoice()
    MoveMaker.makeMove(choice, g) match {
      case ValidMove(new_cells) => play(g.nextGame(new_cells))
      case InvalidMove(e_msg) => {
        println(e_msg)
        play(g)
      }
    }
  }

  /** When a computer is playing */
  def computerPlay (g: GameStatus): Unit = {

    val random = new util.Random(System.currentTimeMillis)
    val allMoves = random.shuffle((1 to 9).map(MoveMaker.makeMove(_, g))).toList

    def choose(next: List[Move], valid: Option[GameStatus]): GameStatus =
      (next, valid) match {
        case (Nil, Some(x)) => x
        case (Nil, None) =>
          throw new Exception("Computer had no possible move?")
        case (InvalidMove(_) :: xs, _) =>
          choose(xs, valid)
        case (ValidMove(new_cells) :: xs, _) => {
          val newGame = g.nextGame(new_cells)
          newGame.winner match {
            case Some(Computer()) => newGame
            case _ => choose(xs, Some(newGame))
          }
        }
    }
    play(choose(allMoves, None))
  }
}


/** Responsible for printing games! */
object GamePrinter {

  /** Prints a game! */
  def print_game(g: GameStatus): Unit = {
    println("-" * Game.totalWidth)
    for ( r <- g.rows ) {
      print_row(r)
    }
    print("\n")
  }

  /** Prints an entire row! 
    * Assumes r is a list of three cells.
    * -------------
    * |   |   |   | <-
    * | C | P |   | <-
    * |   |   |   | <-
    * ------------- <-
    * |   |   |   |
    * (...)
    */
  def print_row(r: List[Cell]): Unit = {
    for ( i <- 1 to Game.CellVerticalSpan ) {
      print_clean_line()
    }
    print_markers_line(r)
    for ( i <- 1 to Game.CellVerticalSpan ) {
      print_clean_line()
    }
    print("-" * Game.totalWidth)
    print("\n")
  }

  /** Prints a clean line with no markers, only
    * column separators.
    * 
    * -------------
    * |   |   |   | <- 
    * | C |   | P | 
    * |   |   |   | 
    * -------------
    * |   |   |   |
    * (...)
    */
  def print_clean_line(): Unit = {
    val nspaces = 2 * Game.CellHorizontalSpan + 1
    print(Game.ColumnSeparator)
    for ( i <- 1 to 3) {
      print(" " * nspaces)
      print(Game.ColumnSeparator)
    }
    print("\n")
  }

  /** Prints the entire line that has a marker. Assumes 
    * that markers is a list with 3 markers.
    * -------------
    * |   |   |   |
    * | C | P |   | <-
    * |   |   |   |
    * -------------
    * |   |   |   |
    * (...)
    */
  def print_markers_line(markers: List[Cell]): Unit = {
    print(Game.ColumnSeparator)
    for ( i <- markers ){
      print(" " * Game.CellHorizontalSpan)
      print_marker(i)
      print(" " * Game.CellHorizontalSpan)
      print(Game.ColumnSeparator)
    }
    print("\n")
  }

  /** Prints a single marker! */
  def print_marker(cell: Cell): Unit = cell match {
    case EmptyCell() => print(" ")
    case MarkedCell(Computer()) => print("C")
    case MarkedCell(Person()) => print("P")
  }
}
