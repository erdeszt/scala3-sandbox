package sandbox

import scala.annotation.implicitNotFound
import scala.compiletime._
import scala.deriving._

sealed trait HList

// Why variance?
final case class ::[+H, +T <: HList](head: H, tail: T) extends HList:
  def ::[HH](hh: HH): HH :: H :: T = new ::(hh, this)

class HNil extends HList:
  def ::[H](h: H): H :: HNil = new ::(h, this)
object HNil extends HNil

class Record[Rec <: HList](storage: Map[String, Any]):
  def apply[Key <: String](using ev: HasField[Rec, Key], keyString: ValueOf[Key]): ev.Out =
    storage.get(keyString.value).get.asInstanceOf[ev.Out]
    
  def add[Key <: String, Value](field: Field[Key, Value]): Record[Field[Key, Value] :: Rec] =
    new Record(storage + (field.key.toString -> field.value))
    
class EmptyRecord extends Record[HNil](Map.empty)
object EmptyRecord extends EmptyRecord

extension [Key <: String, Value](field: Field[Key, Value])
  def +:[Rec <: HList](record: Record[Rec]): Record[Field[Key, Value] :: Rec] =
    record.add(field)

final case class Field[Key, Value](key: Key, value: Value)

type ->>[Key, Value] = Field[Key, Value]

extension [Key](key: Key)
  def ->>[Value](value: Value): Field[Key, Value] =
    Field(key, value)

@implicitNotFound("Record: ${Rec} doesn't have field: ${Key} or the requested output type is incorrect")
trait HasField[Rec, Key <: String]:
  type Out
  def getField(value: Rec): Out

object HasField:
  type Aux[Rec, Key <: String, Output] = HasField[Rec, Key] { type Out = Output }
  given here[Key <: String, T,  Tail <: HList]: HasField[Key ->> T :: Tail, Key] with
    type Out = T
    def getField(rec: Key ->> T :: Tail): T =
      rec.head.value
    
  given there[H, Field <: String, Tail <: HList](using tailHasField: HasField[Tail, Field]): HasField[H :: Tail, Field] with
    type Out = tailHasField.Out
    def getField(rec: H :: Tail): tailHasField.Out =
      tailHasField.getField(rec.tail)

  given fromRecord[Key <: String, T, Rec <: HList](using keyString: ValueOf[Key], ev: HasField.Aux[Rec, Key, T]): HasField[Record[Rec], Key] with
    type Out = T
    def getField(rec: Record[Rec]): T =
      rec[Key]

def field[Key <: String]: FieldBuilder[Key] = new FieldBuilder[Key]

class FieldBuilder[Key <: String]:
  inline def apply[Value](value: Value): Field[Key, Value] =
    Field(constValue[Key], value)
  
//inline def productToRec[T](using mir: Mirror.Of[T]) =
//  inline mir match
//    case prodMir: Mirror.ProductOf[T] =>
//      prodMir

type TestRec = "x" ->> Int :: "y" ->> Boolean :: HNil

case class Test(x: Int, y: Boolean)

inline def test[T](x: T)(using mir: Mirror.Of[T]): Unit =
  println(s"X: ${x}")

inline def powX[Rec](rec: Rec)(using getX: HasField.Aux[Rec, "x", Int]): Int =
  getX.getField(rec) * getX.getField(rec)

@main
def main(): Unit =
  val onlyY = field["y"](true) :: HNil
  val onlyStringX = field["x"]("alma") :: HNil
  val test: TestRec = field["x"](2) :: field["y"](false) :: HNil
  val testX  = summon[HasField[TestRec, "x"]].getField(test)
  val testXX = powX(test)
  val test2: Record[TestRec] = field["x"](2) +: field["y"](false) +: EmptyRecord
  val test2XX = powX(test2)
//  val wrong1: Int = powX(onlyStringX) // Compile time error
//  val wrong2: Int = powX(onlyY) // Compile time error
  println(s"X: ${testX}, XX: ${testXX}, 2XX: ${test2XX}")
//  test[{ val x: Int }](Test(1, false))
  writetomap.Macro.demo
//  writetomap.Generic.demo