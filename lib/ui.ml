open Lwt.Infix
open GMain
open GdkKeysyms

let random_char () =
  let a = int_of_char 'a' in
  let z = int_of_char 'z' in
  char_of_int (a + Random.int (z - a + 1))

let array =
  Array.init 9 (fun _ -> Array.init 9 (fun _ -> String.make 1 (random_char ())))

let main () =
  let _ = GtkMain.Main.init () in
  let _ = Lwt_glib.install () in

  let window = GWindow.window ~width:200 ~height:200 ~title:"Array Boxes" () in
  let table = GPack.table ~rows:9 ~columns:9 ~packing:window#add () in

  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j item ->
          let button =
            GButton.button ~label:item
              ~packing:(table#attach ~left:j ~top:i ~expand:`BOTH)
              ()
          in
          button#misc#modify_font (Pango.Font.from_string "Sans Bold 60");
          (* Set the font size to 20 *)
          button#misc#modify_fg [ (`NORMAL, `NAME "black") ];
          button#connect#clicked ~callback:(fun () ->
              print_endline ("You clicked on " ^ item))
          |> ignore)
        row)
    array;

  let _ = window#connect#destroy ~callback:Main.quit in
  window#show ();

  Lwt_main.run (fst (Lwt.wait ()))

let _ = main ()
