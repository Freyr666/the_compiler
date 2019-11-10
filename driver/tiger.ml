let () =
  let open Result in
  let _ = map succ (Ok 42) in
  Printf.printf "Compiled\n"
