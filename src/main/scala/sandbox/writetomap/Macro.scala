package sandbox.writetomap

import scala.collection.{MapView, mutable}
import scala.compiletime._
import scala.deriving._
import scala.quoted._
import sandbox.MacroUtils._

object Macro:

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

    inline def recurse[Names, Types](element: Product, map: mutable.Map[String, Any])(index: Int): Unit =
      inline erasedValue[(Names, Types)] match
        case (_: (name *: names), _: (tpe *: types)) =>
          val key = constValue[name].toString
          val value = element.productElement(index).asInstanceOf[tpe]
          summonInline[Writer[tpe]].writeToMap(map)(key, value)
          recurse[names, types](element, map)(index + 1)
        case _ => ()

    inline def derived[T](using mir: Mirror.Of[T]) =
      PrintMacPass(
        new Writer[T]:
          override def writeToMap(map: mutable.Map[String, Any])(key: String, value: T): Unit =
            inline mir match
              case proMir: Mirror.ProductOf[T] =>
                recurse[proMir.MirroredElemLabels, proMir.MirroredElemTypes](value.asInstanceOf[Product], map)(0)
      )

  case class Address(city: String, street: String) derives Writer

  case class Person(name: String, age: Int, address: Address) derives Writer
  
  def demo: Unit =
    val joe = Person("jo", 27, Address("Cph", "X"))
    println(s"Joe: ${joe.writeToMap.toMap}")
  
