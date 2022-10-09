import cats.effect._
import cats.effect.std.{Console, Random}
import cats.syntax.all._
import io.circe.syntax.EncoderOps
import simulator.Simulator
import simulator.Simulator.{EvaluationResult, GameCommand, PreciseState, SimulationStepInput}
import typings.node.NodeJS.{ReadableStream, WritableStream}
import typings.node.processMod.global.process
import typings.node.readlineMod
import webapp.marslander.{Coord, Level}

import scala.util.Try

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    repl.foreverM

  def printLevelInfo(level: Level): IO[Unit] =
    Console[IO].println(s"Level: ${level.name}") *>
      Console[IO].println(level.initialState.surfaceN) *>
      level.initialState.surfaceCoords.traverse_ { case Coord(x, y) =>
        Console[IO].println(s"$x $y")
      }

  def askQuestion(question: String): IO[String] =
    for {
      _      <- Console[IO].println(question)
      answer <- askUser
    } yield answer

  def askSeed: IO[Int] =
    askQuestion("Enter seed: ").map(_.toIntOption).iterateUntil(_.isDefined).map(_.get)

  def getRandomLevel(seed: Int): IO[Level] = for {
    random    <- Random.scalaUtilRandomSeedInt[IO](seed)
    allLevels <- readLevels
    randomLevel <- Random[IO](random).shuffleList(allLevels).map(_.head) // let it crash
  } yield randomLevel

  def repl: IO[EvaluationResult] = askSeed.flatMap(getRandomLevel).flatMap(levelRepl)

  def levelRepl(level: Level): IO[EvaluationResult] = {
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

  def loop(level: Level, preciseState: PreciseState): IO[EvaluationResult] =
    step(level, preciseState).flatMap { either =>
      either match {
        case Right(state)         => loop(level, state)
        case Left((evRes, state)) =>
          printState(level, state, evRes) *> IO.pure(evRes) // never pollute stdout with infos
      }
    }

  def step(level: Level, oldState: PreciseState): IO[Either[(EvaluationResult, PreciseState), PreciseState]] =
    for {
      currentResult <- IO.pure(oldState.evaluate(level, None))
      _             <- printState(level, oldState, currentResult)
      cmdStr        <- askUser
      cmd           <- IO.fromTry(Try(GameCommand.parse(cmdStr)))
      newState       = Simulator.simulate(SimulationStepInput(level, oldState, cmd))
      result         = newState.evaluate(level, Some(oldState))
    } yield result match {
      case _: EvaluationResult.AllClear => Right(newState)
      case res                          => Left((res, newState))
    }

  def printState(level: Level, preciseState: PreciseState, evaluationResult: EvaluationResult): IO[Unit] =
    Console[IO].println(s"${evaluationResult.enrichedState.flattened.asJson.noSpacesSortKeys}")

  val readline =
    readlineMod.createInterface(process.stdin.asInstanceOf[ReadableStream], process.stdout.asInstanceOf[WritableStream])

  val askUser: IO[String] =
    // TODO promise overload for readline?
    IO.async_[String](cb => readline.question("", data => cb(Right(data))))

}
