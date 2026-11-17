//> using dep com.lihaoyi::cask:0.9.4

case class Produk(id: Int, nama: String, kategori: String, stok: Int)

def filterRec[A](list: List[A], pred: A => Boolean): List[A] =
  list match
    case Nil => Nil
    case head :: tail =>
      if pred(head) then head :: filterRec(tail, pred)
      else filterRec(tail, pred)

def namaMengandung(q: String): Produk => Boolean =
  p => p.nama.toLowerCase.contains(q.toLowerCase)

def kategoriSama(k: String): Produk => Boolean =
  p => p.kategori.equalsIgnoreCase(k)

val stokTersedia: Produk => Boolean =
  p => p.stok > 0

def gabung(preds: List[Produk => Boolean]): Produk => Boolean =
  preds match
    case Nil          => _ => true
    case head :: tail => p => head(p) && gabung(tail)(p)

object TokoApp extends cask.MainRoutes:

  override def port: Int = 8080

  val daftarProduk = List(
    Produk(1, "Pupuk Urea 50kg", "Pupuk", 20),
    Produk(2, "Pupuk NPK 25kg", "Pupuk", 0),
    Produk(3, "Pupuk Organik Kompos", "Pupuk", 35),
    Produk(4, "Cangkul Baja", "Alat", 8),
    Produk(5, "Sabit Panen", "Alat", 0),
    Produk(6, "Sprayer Punggung 16L", "Alat", 5),
    Produk(7, "Benih Jagung Hibrida", "Benih", 0),
    Produk(8, "Benih Padi Inpari 32", "Benih", 40),
    Produk(9, "Benih Cabai Rawit", "Benih", 12),
    Produk(10, "Insektisida Organik", "Pestisida", 15),
    Produk(11, "Fungisida Daun", "Pestisida", 0)
  )

  @cask.get("/")
  def index() =
    cask.Response(
      java.nio.file.Files.readString(java.nio.file.Path.of("index.html")),
      headers = Seq("Content-Type" -> "text/html; charset=utf-8")
    )

  @cask.get("/api/produk")
  def cariProduk(nama: String = "", kategori: String = "", tersedia: String = "") =
    val preds: List[Produk => Boolean] =
      List(
        if nama.trim.nonEmpty then Some(namaMengandung(nama.trim)) else None,
        if kategori.trim.nonEmpty then Some(kategoriSama(kategori.trim)) else None,
        if tersedia == "true" then Some(stokTersedia) else None
      ).flatten

    val hasil = filterRec(daftarProduk, gabung(preds))

    ujson.Arr.from(
      hasil.map(p =>
        ujson.Obj(
          "id" -> p.id,
          "nama" -> p.nama,
          "kategori" -> p.kategori,
          "stok" -> p.stok
        )
      )
    )

  initialize()