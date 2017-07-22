import scalanative.native._

@extern
@link("girepository-1.0")
object gir {

  type Repository = Ptr[Byte]
  type Typelib = Ptr[Byte]
  type BaseInfo = Ptr[Byte]

  @name("g_irepository_get_default")
  def default: Repository = extern

  @name("g_irepository_get_n_infos")
  def nInfos(repository: Repository, namespace: CString): CInt = extern

  @name("g_irepository_require")
  def require(repository: Repository, namespace: CString, version: CString, flags: CInt, error: Ptr[Ptr[Byte]]): Typelib = extern

  @name("g_typelib_get_namespace")
  def namespace(typelib: Typelib): CString = extern

  @name("g_irepository_get_info")
  def getInfo(repository: Repository, namespace: CString, index: CInt): BaseInfo  = extern

  @name("g_base_info_get_name")
  def infoGetName(info: BaseInfo): CString = extern

  @name("g_base_info_get_type")
  def infoGetType(info: BaseInfo): CInt = extern
}

object ScalaGir extends App {
  Zone { z =>
    implicit def convertStringToNative(str: String): CString = toCString(str)(z)

    // enum
    sealed class InfoType(val i: Int)
    case object Invalid extends InfoType(0)
    case object Function extends InfoType(1)
    case object Callback extends InfoType(2)
    case object Struct extends InfoType(3)
    case object Boxed extends InfoType(4)
    case class Unknown(override val i: Int) extends InfoType(i) {
      override def toString = s"$i ???"
    }
    object InfoType {
      val all = Seq(Invalid, Function, Callback, Struct, Boxed)
      val map = all.map(x => x.i -> x).toMap
      def apply(x: Int) = map.getOrElse(x, Unknown(x))
    }

    if (args.isEmpty) {
      println("Usage: ./scala-gir <typelib>...")
    }

    for (arg <- args) {
      println(s"Loading typelib ‘$arg’")
      val gtk = gir.require(null, arg, null, 0, null)
      if (gtk == null) {
        println("\terror: no such typelib")
      } else {
        println("\tloaded")
        val n = gir.nInfos(null, arg)
        for (i <- 0 until n) {
          val info = gir.getInfo(null, arg, i)
          val name = fromCString(gir.infoGetName(info))
          val typ = InfoType(gir.infoGetType(info))
          println(s"\titem #$i: $name, type $typ")
        }
      }
    }


  }
}
