import cats.effect._
import cats.effect.std.Console
import cats.effect.std.Random
import cats.syntax.all._
import simulator.Simulator
import simulator.Simulator.{EvaluationResult, GameCommand, PreciseState, SimulationStepInput}
import webapp.marslander.{Coord, Level}
import typings.node.NodeJS.{ReadableStream, WritableStream}
import typings.node.processMod.global.process
import typings.node.{pathMod, readlineMod}

import scala.util.Try

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    //  val allLevels =
    //    asJson[List[Level]]
    //

    for {
      random    <- Random.scalaUtilRandom[IO]
      allLevels <- readLevels
      randomLevel <- Random[IO](random).shuffleList(allLevels).map(_.head) // let it crash

      result <- repl(randomLevel)

    } yield result

  def printLevelInfo(level: Level): IO[Unit] =
    Console[IO].println(level.initialState.surfaceN) *> level.initialState.surfaceCoords.traverse_ { case Coord(x, y) =>
      Console[IO].println(s"$x $y")
    }

  def repl(level: Level): IO[ExitCode] = {
    val initialState = PreciseState(
      x = level.landerInitialState.x,
      y = level.landerInitialState.y,
      rotate = level.landerInitialState.rotate,
      hSpeed = level.landerInitialState.hSpeed,
      vSpeed = level.landerInitialState.vSpeed,
      power = level.landerInitialState.power,
      fuel = level.landerInitialState.fuel,
    )
    printLevelInfo(level) *> loop(level, initialState)
  }

  def readLevels: IO[List[Level]] = {
    import webapp.marslander.Codecs._
    // don't know how to handle resources in node js. So, I'm cheating and have a compile time json string ;-)
    IO.fromEither(io.circe.parser.decode[List[Level]](Levels.json))
  }

  def loop(level: Level, preciseState: PreciseState): IO[ExitCode] =
    step(level, preciseState).flatMap {
      case Left((EvaluationResult.Landed, state))    => printState(state) *> IO(ExitCode.Success)
      case Left((EvaluationResult.Crashed, state))   =>
        printState(state) *> Console[IO].errorln("Crashed") *> IO(ExitCode.Error)
      case Left((EvaluationResult.OffLimits, state)) =>
        printState(state) *> Console[IO].errorln("OffLimits") *> IO(ExitCode.Error)
      case Right(state)                              => loop(level, state)
    }

  def step(level: Level, preciseState: PreciseState): IO[Either[(EvaluationResult, PreciseState), PreciseState]] =
    for {
      _       <- printState(preciseState)
      cmdStr  <- askUser
      cmd     <- IO.fromTry(Try(GameCommand.parse(cmdStr)))
      newState = Simulator.simulate(SimulationStepInput(level, preciseState, cmd))
      result   = newState.evaluate(level, Some(preciseState))
    } yield result match {
      case EvaluationResult.AllClear => Right(newState)
      case res                       => Left((res, newState))
    }

  def printState(preciseState: PreciseState): IO[Unit] =
    Console[IO].println(s"$preciseState")

  val readline =
    readlineMod.createInterface(process.stdin.asInstanceOf[ReadableStream], process.stdout.asInstanceOf[WritableStream])

  val askUser: IO[String] =
    // TODO promise overload for readline?
    IO.async_[String](cb => readline.question("", data => cb(Right(data))))

}