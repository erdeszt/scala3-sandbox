package sandbox

val maxTries = 10000

@main
def main(): Unit =
  writetomap.Macro.demo
//  writetomap.Generic.demo // TODO: Fix
  Records.demo
