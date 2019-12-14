open Core

let m_assert ?(msg = "") cond =
   assert (if not cond then print_endline msg; cond)
