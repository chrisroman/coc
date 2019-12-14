open Core

let config_debug = ref false

let rec repeat_str str n =
  assert (n > 0);
  if n = 1 then str else str ^ (repeat_str str (n - 1))

let dbg_print ?(depth = 0) msg =
  if !config_debug then print_string ((repeat_str "\t" depth) ^ msg) else ()

let dbg_failwith ?(depth = 0) msg =
  failwith ((repeat_str "\t" depth) ^ msg)

let m_assert ?(msg = "") ?(depth = 0) cond =
   assert (if not cond then print_endline ((repeat_str "\t" depth) ^ msg); cond)
