infix || &&
val op|| = Tigr.||
val op&& = Tigr.&&

(* Quote a string for use as a single word in a POSIX shell command. *)
fun shellQuote (s: string) : string =
  "'" ^ String.translate (fn #"'" => "'\\''" | c => String.str c) s ^ "'"

(* Rasterise every page of the PDF file `pdf` at `dpi` dots per inch and load
   the pages as bitmaps.  The pages are rendered with pdftoppm (from
   poppler-utils) into a fresh temporary directory, which is removed again
   once the images have been loaded.  pdftoppm names the files
   slide-1.png, slide-2.png, ..., zero-padding the numbers so that
   lexicographic order is page order. *)
fun loadSlides (pdf: string, dpi: int) : Tigr.tigr array =
  let
    fun fail msg =
      (print (msg ^ "\n"); OS.Process.exit OS.Process.failure)
    val () =
      if OS.FileSys.access (pdf, [OS.FileSys.A_READ]) then ()
      else fail ("Cannot read PDF file: " ^ pdf)
    (* As per the Basis spec, tmpName creates an empty file with a unique
       name, which we keep while the directory next to it exists, so the name
       stays reserved.  MLKit before 4.7.23 only returns an unused name, so
       mkDir may still fail if the name was taken in the meantime; retry. *)
    fun mkTmpDir tries =
      let val f = OS.FileSys.tmpName ()
      in (OS.FileSys.mkDir (f ^ ".d"); f)
         handle OS.SysErr _ =>
           ( OS.FileSys.remove f handle OS.SysErr _ => ()
           ; if tries > 0 then mkTmpDir (tries - 1)
             else fail "Cannot create temporary directory"
           )
      end
    val tmpFile = mkTmpDir 10
    val dir = tmpFile ^ ".d"
    fun cleanup () =
      let
        val stream = OS.FileSys.openDir dir
        fun loop () =
          case OS.FileSys.readDir stream of
            NONE => ()
          | SOME entry =>
              ( OS.FileSys.remove
                  (OS.Path.joinDirFile {dir = dir, file = entry})
              ; loop ()
              )
      in
        loop ();
        OS.FileSys.closeDir stream;
        OS.FileSys.rmDir dir;
        OS.FileSys.remove tmpFile handle OS.SysErr _ => ()
      end
    val () = print ("Converting " ^ pdf ^ " to images...\n")
    val cmd =
      "pdftoppm -r " ^ Int.toString dpi ^ " -png " ^ shellQuote pdf ^ " "
      ^ shellQuote (OS.Path.joinDirFile {dir = dir, file = "slide"})
    val () =
      if OS.Process.isSuccess (OS.Process.system cmd) then ()
      else (cleanup (); fail ("pdftoppm failed on " ^ pdf))
    val stream = OS.FileSys.openDir dir
    fun collect acc =
      case OS.FileSys.readDir stream of
        NONE => acc
      | SOME entry =>
          let
            val acc =
              if String.isSuffix ".png" entry then
                OS.Path.joinDirFile {dir = dir, file = entry} :: acc
              else
                acc
          in
            collect acc
          end
    val paths = collect []
    val () = OS.FileSys.closeDir stream
    fun insert x [] = [x]
      | insert x (h :: t) =
          if x <= h then x :: h :: t else h :: insert x t
    val sorted = foldl (fn (x, acc) => insert x acc) [] paths
    val slides = Array.fromList (map Tigr.loadImage sorted)
  in
    cleanup ();
    if Array.length slides = 0 then fail ("No pages found in " ^ pdf)
    else slides
  end

(* Clamp i to [0, n-1]. *)
fun clamp (i, n) =
  Int.max (0, Int.min (i, n - 1))

(* Futhark-side state for the current slide, plus which effect is running. *)
datatype effect =
  Still
| NBody (* Pixels fly around as gravitating bodies. *)
| Sand of int (* Pixels fall like sand; the int is the step counter. *)

(* Convert a tigr bitmap to a Futhark viewer state. *)
fun loadState ctx (bmp: Tigr.tigr) : Viewer.Opaque.state.t =
  let
    val w = Tigr.width bmp
    val h = Tigr.height bmp
    val pixels = Word8Array.array (h * w * 4, 0w0)
    val () = Tigr.toArray (bmp, pixels)
    val arr = Viewer.Word8Array3.new ctx (Word8ArraySlice.full pixels) (h, w, 4)
    val state = Viewer.Entry.load_image ctx arr
  in
    Viewer.Word8Array3.free arr;
    state
  end

fun main () =
  let
    fun usage () =
      ( print
          ("Usage: " ^ CommandLine.name () ^ " <slides.pdf> [dpi]\n" ^ "  e.g. "
           ^ CommandLine.name () ^ " presentation.pdf 150\n")
      ; OS.Process.exit OS.Process.failure
      )
    val (pdf, dpi) =
      case CommandLine.arguments () of
        [p] => (p, 150)
      | [p, d] =>
          (case Int.fromString d of
             SOME d => (p, d)
           | NONE => usage ())
      | _ => usage ()

    val slides = loadSlides (pdf, dpi)
    val n = Array.length slides
    val () = print ("Loaded " ^ Int.toString n ^ " slides.\n")
    val () = print
      ("Keys: arrows/space/PgUp/PgDn/Home/End navigate, P explode, R shuffle,\n\
       \      O revert explosion, D sand, Q/Esc quit.\n")

    (* Open the window at the first slide's native resolution.
       With TIGR_AUTO, the window bitmap is resized to match the OS window,
       and the slides are scaled to fit it in `render`. *)
    val first = Array.sub (slides, 0)
    val win = Tigr.window
      { w = Tigr.width first
      , h = Tigr.height first
      , title = "Slide Viewer"
      , flags = Tigr.TIGR_AUTO
      }

    fun keyd k = Tigr.keyDown (win, k)

    val ctx = Viewer.Context.new Viewer.Config.default

    val state = ref (loadState ctx first)
    val effect = ref Still
    (* Whether the state has changed since it was last rendered. *)
    val dirty = ref true

    fun setState s =
      (Viewer.Opaque.state.free (!state); state := s; dirty := true)

    fun showSlide idx =
      (setState (loadState ctx (Array.sub (slides, idx))); effect := Still)

    fun startNBody () =
      (setState (Viewer.Entry.start_nbody ctx (!state)); effect := NBody)

    (* Advance the running effect by one step. *)
    fun step () =
      case !effect of
        Still => ()
      | NBody => setState (Viewer.Entry.advance ctx (!state))
      | Sand i =>
          ( setState (Viewer.Entry.drop_sand ctx
              (Int32.fromInt (i mod 2), !state))
          ; effect := Sand (i + 1)
          )

    fun handleEffectKeys () =
      if keyd (Tigr.ascii #"P") then
        startNBody ()
      else if keyd (Tigr.ascii #"R") then
        ( if !effect = NBody then () else startNBody ()
        ; setState (Viewer.Entry.shuffle ctx
            ( Int32.fromLarge (Time.toMilliseconds (Time.now ()) mod 1000000)
            , !state
            ))
        )
      else if keyd (Tigr.ascii #"O") andalso !effect = NBody then
        setState (Viewer.Entry.revert ctx (!state))
      else if keyd (Tigr.ascii #"D") then
        (setState (Viewer.Entry.freeze ctx (!state)); effect := Sand 0)
      else
        ()

    (* Buffer for the rendered frame, reallocated when the window size
       changes. *)
    val frame = ref (Word8Array.array (0, 0w0))

    (* Render slide `idx` scaled to the window, then draw the overlay.  Does
       nothing if neither the state nor the window size has changed. *)
    fun render (idx: int) : unit =
      let
        val ww = Tigr.width win
        val wh = Tigr.height win
        val font = Tigr.defaultFont ()
        val label = Int.toString (idx + 1) ^ " / " ^ Int.toString n
        val tw = Tigr.textWidth (font, label)
        val th = Tigr.textHeight (font, label)
        val px = ww - tw - 6
        val py = wh - th - 4
      in
        if
          ww <= 0 orelse wh <= 0
          orelse (not (!dirty) andalso Word8Array.length (!frame) = ww * wh * 4)
        then
          ()
        else
          let
            val arr =
              Viewer.Entry.render ctx
                (Int64.fromInt wh, Int64.fromInt ww, !state)
          in
            if Word8Array.length (!frame) = ww * wh * 4 then ()
            else frame := Word8Array.array (ww * wh * 4, 0w0);
            Viewer.Word8Array3.values_into arr (Word8ArraySlice.full (!frame));
            Viewer.Word8Array3.free arr;
            dirty := false;
            Tigr.fromArray (win, !frame);
            (* Slide-number counter in the bottom-right corner with a shadow. *)
            Tigr.print
              { dest = win
              , font = font
              , x = px + 1
              , y = py + 1
              , color = Tigr.black
              , text = label
              };
            Tigr.print
              { dest = win
              , font = font
              , x = px
              , y = py
              , color = Tigr.white
              , text = label
              }
          end
      end

    fun loop (idx: int) : unit =
      if
        Tigr.closed win orelse keyd Tigr.TK_ESCAPE orelse keyd (Tigr.ascii #"Q")
      then
        ()
      else
        let
          val idx' =
            if
              keyd Tigr.TK_RIGHT orelse keyd Tigr.TK_SPACE
              orelse keyd Tigr.TK_PAGEDN orelse keyd Tigr.TK_RETURN
            then clamp (idx + 1, n)
            else if
              keyd Tigr.TK_LEFT orelse keyd Tigr.TK_BACKSPACE
              orelse keyd Tigr.TK_PAGEUP
            then clamp (idx - 1, n)
            else if
              keyd Tigr.TK_HOME
            then 0
            else if
              keyd Tigr.TK_END
            then n - 1
            else idx
        in
          if idx' <> idx then showSlide idx' else handleEffectKeys ();
          step ();
          render idx';
          Tigr.update win;
          loop idx'
        end
  in
    render 0;
    Tigr.update win;
    loop 0;
    Viewer.Opaque.state.free (!state);
    Viewer.Context.free ctx;
    Array.app Tigr.free slides;
    Tigr.free win
  end

val () = main ()
