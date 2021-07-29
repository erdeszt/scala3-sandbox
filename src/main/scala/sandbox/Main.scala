package sandbox

val maxTries = 10000

@main
def main(): Unit =
//  writetomap.Macro.demo
//  writetomap.Generic.demo // TODO: Fix

  Records.demo

  val pe44Solution = ProjectEuler44.solve()

  println(s"The solution to Project Euler problem 44 is: ${pe44Solution}")
