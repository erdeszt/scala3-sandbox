package sandbox.writetomap

import scala.collection.{MapView, mutable}
import scala.compiletime._
import scala.deriving._
import scala.quoted._
import sandbox.MacroUtils._

object Generic:

  trait Writer[T]:
    def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit

  extension [T](value: T)(using writer: Writer[T])
    def writeToMap: MapView[String, Any] =
      val map = mutable.Map.empty[String, Any]
      writer.writeToMap(map)("", value)
      map.view
      
  object Writer:

    given Writer[Int] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: Int): Unit =
        map.put(key, value)

    given Writer[String] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: String): Unit =
        map.put(key, value)
    
    given Writer[EmptyTuple] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: EmptyTuple): Unit =
        ()

    given [H: Writer, T <: Tuple: Writer]: Writer[H *: T] with
      def writeToMap(map: mutable.Map[String, Any])(key: String, value: H *: T): Unit =
        value match
          case (h *: t) =>
            map.put(key, h)
            summon[Writer[T]].writeToMap(map)(key, t)

    inline def derived[T](using mir: Mirror.Of[T]) =
      PrintMacPass(
        new Writer[T]:
          def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =  
            inline mir match
              case proMir: Mirror.ProductOf[T] =>
                summonInline[Writer[T]].writeToMap(map)(key, value) // Doesn't work
      )

  case class Address(city: String, street: String) derives Writer

  case class Person(name: String, age: Int, address: Address) derives Writer
    
  def demo: Unit =

    val joe = Person("jo", 27, Address("Cph", "X"))
    println(s"Joe: ${joe.writeToMap.toMap}")
