open Prims
type regs = Bytecode_Instructions.reg -> FStar_UInt64.t
type 'a st = regs -> ('a * regs)
type program = Bytecode_Instructions.inst Prims.list
let (read_reg : Bytecode_Instructions.reg -> FStar_UInt64.t st) =
  fun r -> fun s -> ((s r), s)
let (write_reg : Bytecode_Instructions.reg -> FStar_UInt64.t -> unit st) =
  fun r -> fun v -> fun s -> ((), (fun r' -> if r' = r then v else s r'))
let bind : 'a 'b . 'a st -> ('a -> 'b st) -> 'b st =
  fun f ->
    fun g ->
      fun s0 -> let uu___ = f s0 in match uu___ with | (x, s1) -> g x s1
let return : 'a . 'a -> 'a st = fun x -> fun s -> (x, s)
let op_let_Bang :
  'uuuuu 'uuuuu1 . unit -> 'uuuuu st -> ('uuuuu -> 'uuuuu1 st) -> 'uuuuu1 st
  = fun uu___ -> bind
let (mask32 : FStar_UInt64.t -> FStar_UInt64.t) =
  fun x -> FStar_UInt64.logand x (Stdint.Uint64.of_string "0xFFFFFFFF")
let (sign_extend_imm32 : FStar_UInt64.t -> FStar_UInt64.t) =
  fun imm ->
    if
      (FStar_UInt64.logand imm (Stdint.Uint64.of_string "0x80000000")) <>
        Stdint.Uint64.zero
    then
      FStar_UInt64.logor imm (Stdint.Uint64.of_string "0xFFFFFFFF00000000")
    else imm
let (eval_operand : Bytecode_Instructions.operand -> FStar_UInt64.t st) =
  fun op ->
    match op with
    | Bytecode_Instructions.RegOp r -> read_reg r
    | Bytecode_Instructions.ImmOp i -> return i
let (eval_operand_sx : Bytecode_Instructions.operand -> FStar_UInt64.t st) =
  fun op ->
    match op with
    | Bytecode_Instructions.RegOp r -> read_reg r
    | Bytecode_Instructions.ImmOp i -> return (sign_extend_imm32 i)
let (sign_extend_width :
  FStar_UInt64.t -> Bytecode_Instructions.sx_width -> FStar_UInt64.t) =
  fun x ->
    fun w ->
      match w with
      | Bytecode_Instructions.W8 ->
          let masked = FStar_UInt64.logand x (Stdint.Uint64.of_int (0xFF)) in
          if
            (FStar_UInt64.logand masked (Stdint.Uint64.of_int (0x80))) <>
              Stdint.Uint64.zero
          then
            FStar_UInt64.logor masked
              (Stdint.Uint64.of_string "0xFFFFFFFFFFFFFF00")
          else masked
      | Bytecode_Instructions.W16 ->
          let masked =
            FStar_UInt64.logand x (Stdint.Uint64.of_string "0xFFFF") in
          if
            (FStar_UInt64.logand masked (Stdint.Uint64.of_int (0x8000))) <>
              Stdint.Uint64.zero
          then
            FStar_UInt64.logor masked
              (Stdint.Uint64.of_string "0xFFFFFFFFFFFF0000")
          else masked
      | Bytecode_Instructions.W32 ->
          let masked =
            FStar_UInt64.logand x (Stdint.Uint64.of_string "0xFFFFFFFF") in
          if
            (FStar_UInt64.logand masked
               (Stdint.Uint64.of_string "0x80000000"))
              <> Stdint.Uint64.zero
          then
            FStar_UInt64.logor masked
              (Stdint.Uint64.of_string "0xFFFFFFFF00000000")
          else masked
let (host_is_little_endian : Prims.bool) = true
let (should_swap : Bytecode_Instructions.endianness -> Prims.bool) =
  fun target ->
    match target with
    | Bytecode_Instructions.LE -> Prims.op_Negation host_is_little_endian
    | Bytecode_Instructions.BE -> host_is_little_endian
let (bswap :
  FStar_UInt64.t -> Bytecode_Instructions.bswap_width -> FStar_UInt64.t) =
  fun x ->
    fun w ->
      match w with
      | Bytecode_Instructions.B16 ->
          let x1 = FStar_UInt64.logand x (Stdint.Uint64.of_string "0xFFFF") in
          FStar_UInt64.logor
            (FStar_UInt64.shift_left
               (FStar_UInt64.logand x1 (Stdint.Uint64.of_int (0x00FF)))
               (Stdint.Uint32.of_int (8)))
            (FStar_UInt64.shift_right
               (FStar_UInt64.logand x1 (Stdint.Uint64.of_int (0xFF00)))
               (Stdint.Uint32.of_int (8)))
      | Bytecode_Instructions.B32 ->
          let x1 =
            FStar_UInt64.logand x (Stdint.Uint64.of_string "0xFFFFFFFF") in
          FStar_UInt64.logor
            (FStar_UInt64.shift_left
               (FStar_UInt64.logand x1 (Stdint.Uint64.of_int (0x000000FF)))
               (Stdint.Uint32.of_int (24)))
            (FStar_UInt64.logor
               (FStar_UInt64.shift_left
                  (FStar_UInt64.logand x1 (Stdint.Uint64.of_int (0x0000FF00)))
                  (Stdint.Uint32.of_int (8)))
               (FStar_UInt64.logor
                  (FStar_UInt64.shift_right
                     (FStar_UInt64.logand x1
                        (Stdint.Uint64.of_string "0x00FF0000"))
                     (Stdint.Uint32.of_int (8)))
                  (FStar_UInt64.shift_right
                     (FStar_UInt64.logand x1
                        (Stdint.Uint64.of_string "0xFF000000"))
                     (Stdint.Uint32.of_int (24)))))
      | Bytecode_Instructions.B64 ->
          FStar_UInt64.logor
            (FStar_UInt64.shift_left
               (FStar_UInt64.logand x
                  (Stdint.Uint64.of_int (0x00000000000000FF)))
               (Stdint.Uint32.of_int (56)))
            (FStar_UInt64.logor
               (FStar_UInt64.shift_left
                  (FStar_UInt64.logand x
                     (Stdint.Uint64.of_int (0x000000000000FF00)))
                  (Stdint.Uint32.of_int (40)))
               (FStar_UInt64.logor
                  (FStar_UInt64.shift_left
                     (FStar_UInt64.logand x
                        (Stdint.Uint64.of_string "0x0000000000FF0000"))
                     (Stdint.Uint32.of_int (24)))
                  (FStar_UInt64.logor
                     (FStar_UInt64.shift_left
                        (FStar_UInt64.logand x
                           (Stdint.Uint64.of_string "0x00000000FF000000"))
                        (Stdint.Uint32.of_int (8)))
                     (FStar_UInt64.logor
                        (FStar_UInt64.shift_right
                           (FStar_UInt64.logand x
                              (Stdint.Uint64.of_string "0x000000FF00000000"))
                           (Stdint.Uint32.of_int (8)))
                        (FStar_UInt64.logor
                           (FStar_UInt64.shift_right
                              (FStar_UInt64.logand x
                                 (Stdint.Uint64.of_string "0x0000FF0000000000"))
                              (Stdint.Uint32.of_int (24)))
                           (FStar_UInt64.logor
                              (FStar_UInt64.shift_right
                                 (FStar_UInt64.logand x
                                    (Stdint.Uint64.of_string "0x00FF000000000000"))
                                 (Stdint.Uint32.of_int (40)))
                              (FStar_UInt64.shift_right
                                 (FStar_UInt64.logand x
                                    (Stdint.Uint64.of_string "0xFF00000000000000"))
                                 (Stdint.Uint32.of_int (56)))))))))
let (eval_inst : Bytecode_Instructions.inst -> unit st) =
  fun i ->
    match i with
    | Bytecode_Instructions.ADD (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.add_mod x y))))
    | Bytecode_Instructions.ADD64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.add_mod x y)))
    | Bytecode_Instructions.SUB (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.sub_mod x y))))
    | Bytecode_Instructions.SUB64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.sub_mod x y)))
    | Bytecode_Instructions.MUL (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.mul_mod x y))))
    | Bytecode_Instructions.MUL64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.mul_mod x y)))
    | Bytecode_Instructions.DIV (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y ->
                  write_reg dst
                    (mask32
                       (if y = Stdint.Uint64.zero
                        then Stdint.Uint64.zero
                        else FStar_UInt64.div x y))))
    | Bytecode_Instructions.DIV64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  write_reg dst
                    (if y = Stdint.Uint64.zero
                     then Stdint.Uint64.zero
                     else FStar_UInt64.div x y)))
    | Bytecode_Instructions.MOD (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y ->
                  write_reg dst
                    (mask32
                       (if y = Stdint.Uint64.zero
                        then x
                        else FStar_UInt64.rem x y))))
    | Bytecode_Instructions.MOD64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  write_reg dst
                    (if y = Stdint.Uint64.zero
                     then x
                     else FStar_UInt64.rem x y)))
    | Bytecode_Instructions.OR (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.logor x y))))
    | Bytecode_Instructions.OR64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.logor x y)))
    | Bytecode_Instructions.AND (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.logand x y))))
    | Bytecode_Instructions.AND64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.logand x y)))
    | Bytecode_Instructions.XOR (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y -> write_reg dst (mask32 (FStar_UInt64.logxor x y))))
    | Bytecode_Instructions.XOR64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y -> write_reg dst (FStar_UInt64.logxor x y)))
    | Bytecode_Instructions.LSH (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y ->
                  write_reg dst
                    (mask32
                       (FStar_UInt64.shift_left x
                          (FStar_UInt32.uint_to_t
                             ((FStar_UInt64.v y) mod (Prims.of_int (32))))))))
    | Bytecode_Instructions.LSH64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  write_reg dst
                    (FStar_UInt64.shift_left x
                       (FStar_UInt32.uint_to_t
                          ((FStar_UInt64.v y) mod (Prims.of_int (64)))))))
    | Bytecode_Instructions.RSH (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y ->
                  write_reg dst
                    (mask32
                       (FStar_UInt64.shift_right x
                          (FStar_UInt32.uint_to_t
                             ((FStar_UInt64.v y) mod (Prims.of_int (32))))))))
    | Bytecode_Instructions.RSH64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  write_reg dst
                    (FStar_UInt64.shift_right x
                       (FStar_UInt32.uint_to_t
                          ((FStar_UInt64.v y) mod (Prims.of_int (64)))))))
    | Bytecode_Instructions.MOV (op, dst) ->
        (op_let_Bang ()) (eval_operand op)
          (fun v -> write_reg dst (mask32 v))
    | Bytecode_Instructions.MOV64 (op, dst) ->
        (op_let_Bang ()) (eval_operand_sx op) (fun v -> write_reg dst v)
    | Bytecode_Instructions.ARSH (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let result =
                    FStar_Int64.shift_arithmetic_right sx
                      (FStar_UInt32.uint_to_t
                         ((FStar_UInt64.v y) mod (Prims.of_int (32)))) in
                  write_reg dst
                    (mask32 (FStar_Int_Cast.int64_to_uint64 result))))
    | Bytecode_Instructions.ARSH64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let result =
                    FStar_Int64.shift_arithmetic_right sx
                      (FStar_UInt32.uint_to_t
                         ((FStar_UInt64.v y) mod (Prims.of_int (64)))) in
                  write_reg dst (FStar_Int_Cast.int64_to_uint64 result)))
    | Bytecode_Instructions.SDIV (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let sy = FStar_Int_Cast.uint64_to_int64 y in
                  let result =
                    if (FStar_Int64.v sy) = Prims.int_zero
                    then FStar_Int64.zero
                    else
                      if
                        ((FStar_Int64.v sx) =
                           (FStar_Int.min_int (Prims.of_int (64))))
                          && ((FStar_Int64.v sy) = (Prims.of_int (-1)))
                      then FStar_Int64.zero
                      else FStar_Int64.div sx sy in
                  write_reg dst
                    (mask32 (FStar_Int_Cast.int64_to_uint64 result))))
    | Bytecode_Instructions.SDIV64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let sy = FStar_Int_Cast.uint64_to_int64 y in
                  let result =
                    if (FStar_Int64.v sy) = Prims.int_zero
                    then FStar_Int64.zero
                    else
                      if
                        ((FStar_Int64.v sx) =
                           (FStar_Int.min_int (Prims.of_int (64))))
                          && ((FStar_Int64.v sy) = (Prims.of_int (-1)))
                      then FStar_Int64.zero
                      else FStar_Int64.div sx sy in
                  write_reg dst (FStar_Int_Cast.int64_to_uint64 result)))
    | Bytecode_Instructions.SMOD (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let sy = FStar_Int_Cast.uint64_to_int64 y in
                  let result =
                    if (FStar_Int64.v sy) = Prims.int_zero
                    then sx
                    else
                      if
                        ((FStar_Int64.v sx) =
                           (FStar_Int.min_int (Prims.of_int (64))))
                          && ((FStar_Int64.v sy) = (Prims.of_int (-1)))
                      then FStar_Int64.zero
                      else FStar_Int64.rem sx sy in
                  write_reg dst
                    (mask32 (FStar_Int_Cast.int64_to_uint64 result))))
    | Bytecode_Instructions.SMOD64 (op, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             (op_let_Bang ()) (eval_operand_sx op)
               (fun y ->
                  let sx = FStar_Int_Cast.uint64_to_int64 x in
                  let sy = FStar_Int_Cast.uint64_to_int64 y in
                  let result =
                    if (FStar_Int64.v sy) = Prims.int_zero
                    then sx
                    else
                      if
                        ((FStar_Int64.v sx) =
                           (FStar_Int.min_int (Prims.of_int (64))))
                          && ((FStar_Int64.v sy) = (Prims.of_int (-1)))
                      then FStar_Int64.zero
                      else FStar_Int64.rem sx sy in
                  write_reg dst (FStar_Int_Cast.int64_to_uint64 result)))
    | Bytecode_Instructions.NEG dst ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             write_reg dst
               (mask32 (FStar_UInt64.sub_mod Stdint.Uint64.zero x)))
    | Bytecode_Instructions.NEG64 dst ->
        (op_let_Bang ()) (read_reg dst)
          (fun x -> write_reg dst (FStar_UInt64.sub_mod Stdint.Uint64.zero x))
    | Bytecode_Instructions.MOVSX (src, width, dst) ->
        (op_let_Bang ()) (read_reg src)
          (fun v ->
             let sx = sign_extend_width v width in write_reg dst (mask32 sx))
    | Bytecode_Instructions.MOVSX64 (src, width, dst) ->
        (op_let_Bang ()) (read_reg src)
          (fun v -> let sx = sign_extend_width v width in write_reg dst sx)
    | Bytecode_Instructions.END (endianness, width, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x ->
             let swapped =
               if should_swap endianness then bswap x width else x in
             write_reg dst swapped)
    | Bytecode_Instructions.END64 (width, dst) ->
        (op_let_Bang ()) (read_reg dst)
          (fun x -> write_reg dst (bswap x width))
    | uu___ -> return ()
let rec (run : program -> unit st) =
  fun p ->
    match p with
    | [] -> return ()
    | hd::tl -> (op_let_Bang ()) (eval_inst hd) (fun uu___ -> run tl)
