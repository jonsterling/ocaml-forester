include Asai.Range

let pp_located pp_arg fmt (x : 'a located) =
  pp_arg fmt x.value
