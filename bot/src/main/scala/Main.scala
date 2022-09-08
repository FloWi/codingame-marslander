import scala.util.{Failure, Success, Try}

object Main extends App {

  val p = os.proc("node", "cli/target/scala-2.13/scalajs-bundler/main/marslander.js")

  // important, otherwise the error is captured in some other process' stderr
  val sub = p.spawn(stderr = os.Pipe)

  val start = System.currentTimeMillis
  println("starting interaction")

  val numPoints = sub.stdout.readLine().toInt
  println(s"Level has $numPoints coordinates")

  val coords = 1.to(numPoints).map(_ => sub.stdout.readLine())
  coords.foreach(println)

  val c = "0 4"

  var round = 0

  while (sub.isAlive()) {
    // val error = Option(sub.stderr.readLine())

    round += 1
    println(s"round #$round")
//    println("   error? " + Option(sub.stderr.readLine()))

    val state = sub.stdout.readLine()
    println(s"  State: $state")
//    println("   error? " + Option(sub.stderr.readLine()))
    sub.stdin.writeLine(c)
//    println("   error? " + Option(sub.stderr.readLine()))
    Try(sub.stdin.flush()) match {
      case Failure(err) =>
        println("   error? " + Option(sub.stderr.readLine()))
        Console.err.println(s"Error: '$err'")
      case Success(_)   =>
    }
  }

  sub.close()

  val end = System.currentTimeMillis
  println(s"finished in ${end - start}ms ")
}
