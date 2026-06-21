(* entry.sml -- runs every suite, prints the summary, exits with status. *)

fun runAllSuites () =
  ( Harness.reset ()
  ; ObjTests.run ()
  ; MtlTests.run ()
  ; PlyTests.run ()
  ; BufferTests.run ()
  ; EdgeTests.run ()
  ; Harness.run () )

fun main () =
  OS.Process.exit
    (if runAllSuites () then OS.Process.success else OS.Process.failure)
