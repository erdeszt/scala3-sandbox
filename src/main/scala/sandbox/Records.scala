package sandbox

import scala.annotation.implicitNotFound
import scala.compiletime._
import scala.deriving._
import sandbox.MacroUtils._

// NOTE: could use mutable Map or var for more performance perhaps
class Record[Rec <: Tuple](val storage: Map[String, Any]):
  def apply[Key <: String](using ev: HasField[Rec, Key], keyString: ValueOf[Key]): ev.Out =
    storage.get(keyString.value).get.asInstanceOf[ev.Out]

  def add[Key, Value](field: Field[Key, Value]): Record[Field[Key, Value] *: Rec] =
    new Record(storage + (field.key.toString -> field.value))

class EmptyRecord extends Record[EmptyTuple](Map.empty)
object EmptyRecord extends EmptyRecord

extension [Key, Value](field:  Field[Key, Value])
  def +:[Rec <: Tuple](record: Record[Rec]): Record[Field[Key, Value] *: Rec] =
    record.add(field)

final case class Field[Key, Value](key: Key, value: Value)

type ->>[Key, Value] = Field[Key, Value]

def field[Key]: FieldBuilder[Key] = new FieldBuilder[Key]

class FieldBuilder[Key]:
  inline def apply[Value](value: Value): Field[Key, Value] =
    Field(constValue[Key], value)

@implicitNotFound("Record: ${Rec} doesn't have field: ${Key} or the requested output type is incorrect")
trait HasField[Rec, Key]:
  type Out
  def apply(value: Rec): Out

object HasField:
  type Aux[Rec, Key, Output] = HasField[Rec, Key] { type Out = Output }
  given here[Key <: String, T, Tail <: Tuple]: HasField[(Key ->> T) *: Tail, Key] with
    type Out = T
    def apply(rec: (Key ->> T) *: Tail): T =
      rec.head.value

  given there[H, Field <: String, Tail <: Tuple](using tailHasField: HasField[Tail, Field]): HasField[H *: Tail, Field]
    with
    type Out = tailHasField.Out
    def apply(rec: H *: Tail): tailHasField.Out =
      tailHasField.apply(rec.tail)

  // NOTE: doesn't work without unifying T with the rest of the types(so using ev.Out instead of T)
  given fromRecord[Key <: String, T, Rec <: Tuple](using
      keyString: ValueOf[Key],
      ev:        HasField.Aux[Rec, Key, T]
  ): HasField[Record[Rec], Key] with
    type Out = T
    def apply(rec: Record[Rec]): T =
      rec[Key]

type RecordType[Names, Types] <: Tuple = (Names, Types) match
  case (name *: names, tpe *: types) => Field[name, tpe] *: RecordType[names, types]
  case _                             => EmptyTuple

type RecordTypeAcc[SoFar <: Tuple, Names, Types] <: Tuple = (Names, Types) match
  case (name *: names, tpe *: types) => RecordTypeAcc[Field[name, tpe] *: SoFar, names, types]
  case _                             => EmptyTuple

// NOTE: How to get rid of the .asInstanceOf calls?
inline def productToRecMapper[Names, Types](element: Product, index: Int): RecordType[Names, Types] =
  inline erasedValue[(Names, Types)] match
    case (_: (name *: names), _: (tpe *: types)) =>
      val key   = constValue[name].toString
      val value = element.productElement(index).asInstanceOf[tpe]
      (field[name](value) *: productToRecMapper[names, types](element, index + 1))
        .asInstanceOf[RecordType[Names, Types]]
    case _ => EmptyTuple.asInstanceOf[RecordType[Names, Types]]

transparent inline def productToRec[T](value: T)(using mir: Mirror.Of[T]): Any =
  inline mir match
    case prodMir: Mirror.ProductOf[T] =>
      productToRecMapper[prodMir.MirroredElemLabels, prodMir.MirroredElemTypes](value.asInstanceOf[Product], 0)

// TODO: Unsafe, should check that all fields exists at compile time
inline def recToProdMapper[Names, Types, Rec <: Tuple](rec: Record[Rec]): Tuple = // TODO: Any
  inline erasedValue[(Names, Types)] match
    case (_: (name *: names), _: (tpe *: types)) =>
      val key   = constValue[name].toString
      val ev    = summonInline[HasField.Aux[Rec, name, tpe]] // TODO: Not working
      val value = rec.storage.get(key).getOrElse(throw new RuntimeException(s"Field not found: ${key}"))
      value *: recToProdMapper[names, types, Rec](rec)
    case _ => EmptyTuple

inline def recToProd[T <: Product]: RecBuilder[T] = new RecBuilder[T]

class RecBuilder[T <: Product]:
  inline def apply[Rec <: Tuple](rec: Record[Rec])(using mir: Mirror.Of[T]): T =
    PrintMacPass(
      inline mir match
        case prodMir: Mirror.ProductOf[T] =>
          val tuple = recToProdMapper[prodMir.MirroredElemLabels, prodMir.MirroredElemTypes, Rec](rec)
          prodMir.fromProduct(tuple)
    )

inline def productToRecordMapper[Names, Types, R <: Tuple](
    element: Product,
    index:   Int,
    rec:     Record[R]
): Record[RecordTypeAcc[R, Names, Types]] =
  inline erasedValue[(Names, Types)] match
    case (_: (name *: names), _: (tpe *: types)) =>
      val key   = constValue[name].toString
      val value = element.productElement(index).asInstanceOf[tpe]
      productToRecordMapper[names, types, Field[name, tpe] *: R](
        element,
        index + 1,
        rec.add[name, tpe](field[name](value))
      ).asInstanceOf[Record[RecordTypeAcc[R, Names, Types]]]
    case _ => rec.asInstanceOf[Record[RecordTypeAcc[R, Names, Types]]]

transparent inline def productToRecord[T](value: T)(using mir: Mirror.Of[T]): Any =
  inline mir match
    case prodMir: Mirror.ProductOf[T] =>
      productToRecMapper[prodMir.MirroredElemLabels, prodMir.MirroredElemTypes](value.asInstanceOf[Product], 0)

type TestRec  = ("x" ->> Int) *: ("y" ->> Boolean) *: EmptyTuple
type TestRec_ = Field["x", Int] *: Field["y", Boolean] *: EmptyTuple

case class Test(x: Int, y: Boolean)

inline def test[T](x: T)(using mir: Mirror.Of[T]): Unit =
  println(s"X: ${x}")

type %[Rec, Key <: String] = (Rec, Key)

type :::[F, T] = F match
  case (r, k) => HasField.Aux[r, k, T]

inline def powX[Rec](rec: Rec)(using getX: (Rec % "x") ::: Int): Int =
  getX(rec) * getX(rec)

object Records:
  def demo: Unit =
    val onlyY       = field["y"](true) *: EmptyTuple
    val onlyStringX = field["x"]("alma") *: EmptyTuple
    val testc       = Test(5, false)
    val test: TestRec = field["x"](2) *: field["y"](false) *: EmptyTuple
    val testX  = summon[HasField[TestRec, "x"]](test)
    val testXX = powX(test)
    val test2: Record[TestRec] = field["x"](2) +: field["y"](false) +: EmptyRecord
    val test2XX = powX(test2)
    val testcr: TestRec = productToRec(testc)
    val testcR: TestRec = productToRecord(testc)
    val testcr2 = powX(testcr)
    val testcR2 = powX(testcR)
//  val back = recToProd[Test](test2)
//    val x = summon[HasField.Aux[Field["x", Int] *: Field["y", Boolean] *: EmptyTuple, "y", Boolean]]
//  val backWrong = recToProd[Test](field["y"](true) +: EmptyRecord)
//  val wrong1: Int = powX(onlyStringX) // Compile time error
//  val wrong2: Int = powX(onlyY) // Compile time error
    println(s"X: ${testX} XX: ${testXX}, 2XX: ${test2XX}")
    println(s"CR: ${testcr}, CR2: ${testcr2}, CRR: ${testcR}, CRR2: ${testcR2}")
//  println(s"Back: ${back}")
//  test[{ val x: Int }](Test(1, false))
