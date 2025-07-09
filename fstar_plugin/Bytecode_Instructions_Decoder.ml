open Prims
let (uint_to_reg :
  FStar_UInt64.t -> Bytecode_Instructions.reg FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R0
    | uu___ when uu___ = Prims.int_one ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R1
    | uu___ when uu___ = (Prims.of_int (2)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R2
    | uu___ when uu___ = (Prims.of_int (3)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R3
    | uu___ when uu___ = (Prims.of_int (4)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R4
    | uu___ when uu___ = (Prims.of_int (5)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R5
    | uu___ when uu___ = (Prims.of_int (6)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R6
    | uu___ when uu___ = (Prims.of_int (7)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R7
    | uu___ when uu___ = (Prims.of_int (8)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R8
    | uu___ when uu___ = (Prims.of_int (9)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R9
    | uu___ when uu___ = (Prims.of_int (10)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.R10
    | uu___ -> FStar_Pervasives_Native.None
let (get_mov_width :
  FStar_UInt64.t ->
    Bytecode_Instructions.sx_width FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = (Prims.of_int (8)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.W8
    | uu___ when uu___ = (Prims.of_int (16)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.W16
    | uu___ when uu___ = (Prims.of_int (32)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.W32
    | uu___ -> FStar_Pervasives_Native.None
let (get_bswap_width :
  FStar_UInt64.t ->
    Bytecode_Instructions.bswap_width FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = (Prims.of_int (16)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.B16
    | uu___ when uu___ = (Prims.of_int (32)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.B32
    | uu___ when uu___ = (Prims.of_int (64)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.B64
    | uu___ -> FStar_Pervasives_Native.None
let (get_mem_size :
  FStar_UInt64.t -> Bytecode_Instructions.size FStar_Pervasives_Native.option)
  =
  fun sz ->
    match FStar_UInt64.v sz with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.W
    | uu___ when uu___ = Prims.int_one ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.H
    | uu___ when uu___ = (Prims.of_int (2)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.B
    | uu___ when uu___ = (Prims.of_int (3)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.DW
    | uu___ -> FStar_Pervasives_Native.None
let (get_memx_size :
  FStar_UInt64.t ->
    Bytecode_Instructions.sx_size FStar_Pervasives_Native.option)
  =
  fun sz ->
    match FStar_UInt64.v sz with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.SxW
    | uu___ when uu___ = Prims.int_one ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.SxH
    | uu___ when uu___ = (Prims.of_int (2)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.SxB
    | uu___ -> FStar_Pervasives_Native.None
let (get_atomic_size :
  FStar_UInt64.t ->
    Bytecode_Instructions.atomic_size FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.AtomicW
    | uu___ when uu___ = (Prims.of_int (3)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.AtomicDW
    | uu___ -> FStar_Pervasives_Native.None
let (get_atomic_op :
  FStar_UInt64.t ->
    Bytecode_Instructions.atomic_op FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.Add
    | uu___ when uu___ = (Prims.of_int (0x40)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.Or
    | uu___ when uu___ = (Prims.of_int (0x50)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.And
    | uu___ when uu___ = (Prims.of_int (0xa0)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.Xor
    | uu___ when uu___ = Prims.int_one ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.FetchAdd
    | uu___ when uu___ = (Prims.of_int (0x41)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.FetchOr
    | uu___ when uu___ = (Prims.of_int (0x51)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.FetchAnd
    | uu___ when uu___ = (Prims.of_int (0xa1)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.FetchXor
    | uu___ when uu___ = (Prims.of_int (0xe1)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.Xchg
    | uu___ when uu___ = (Prims.of_int (0xf1)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.CmpXchg
    | uu___ -> FStar_Pervasives_Native.None
let (get_imm_ld_kind :
  FStar_UInt64.t ->
    Bytecode_Instructions.imm_ld_kind FStar_Pervasives_Native.option)
  =
  fun n ->
    match FStar_UInt64.v n with
    | uu___ when uu___ = Prims.int_zero ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.LDI64
    | uu___ when uu___ = Prims.int_one ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.MapFd
    | uu___ when uu___ = (Prims.of_int (0x2)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.MapValFd
    | uu___ when uu___ = (Prims.of_int (0x3)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.VarAddr
    | uu___ when uu___ = (Prims.of_int (0x4)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.CodeAddr
    | uu___ when uu___ = (Prims.of_int (0x5)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.MapIdx
    | uu___ when uu___ = (Prims.of_int (0x6)) ->
        FStar_Pervasives_Native.Some Bytecode_Instructions.MapValIdx
    | uu___ -> FStar_Pervasives_Native.None
let (decode_imm_le : FStar_UInt64.t -> FStar_UInt64.t) =
  fun encoded ->
    let b0 =
      FStar_UInt64.logand
        (FStar_UInt64.shift_right encoded (Stdint.Uint32.of_int (24)))
        (Stdint.Uint64.of_int (0xFF)) in
    let b1 =
      FStar_UInt64.logand
        (FStar_UInt64.shift_right encoded (Stdint.Uint32.of_int (16)))
        (Stdint.Uint64.of_int (0xFF)) in
    let b2 =
      FStar_UInt64.logand
        (FStar_UInt64.shift_right encoded (Stdint.Uint32.of_int (8)))
        (Stdint.Uint64.of_int (0xFF)) in
    let b3 = FStar_UInt64.logand encoded (Stdint.Uint64.of_int (0xFF)) in
    FStar_UInt64.logor b0
      (FStar_UInt64.logor
         (FStar_UInt64.shift_left b1 (Stdint.Uint32.of_int (8)))
         (FStar_UInt64.logor
            (FStar_UInt64.shift_left b2 (Stdint.Uint32.of_int (16)))
            (FStar_UInt64.shift_left b3 (Stdint.Uint32.of_int (24)))))
let (decode_offset_le : FStar_UInt64.t -> FStar_UInt64.t) =
  fun encoded ->
    let b0 =
      FStar_UInt64.logand
        (FStar_UInt64.shift_right encoded (Stdint.Uint32.of_int (8)))
        (Stdint.Uint64.of_int (0xFF)) in
    let b1 = FStar_UInt64.logand encoded (Stdint.Uint64.of_int (0xFF)) in
    FStar_UInt64.logor b0
      (FStar_UInt64.shift_left b1 (Stdint.Uint32.of_int (8)))
let (decode_alu_jump :
  FStar_UInt64.t ->
    FStar_UInt64.t ->
      FStar_UInt64.t ->
        FStar_UInt64.t ->
          FStar_UInt64.t ->
            FStar_UInt64.t ->
              FStar_UInt64.t ->
                Bytecode_Instructions.inst FStar_Pervasives_Native.option)
  =
  fun code ->
    fun src ->
      fun cls ->
        fun src_reg_val ->
          fun dst_reg ->
            fun offset ->
              fun imm ->
                FStar_Option.op_let_Question (uint_to_reg dst_reg)
                  (fun dst ->
                     FStar_Option.op_let_Question (uint_to_reg src_reg_val)
                       (fun src_reg ->
                          let op =
                            if src = Stdint.Uint64.one
                            then Bytecode_Instructions.RegOp src_reg
                            else Bytecode_Instructions.ImmOp imm in
                          match ((FStar_UInt64.v cls), (FStar_UInt64.v code))
                          with
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = Prims.int_zero)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.ADD (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = Prims.int_zero)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.ADD64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = Prims.int_one)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.SUB (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = Prims.int_one)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.SUB64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x2)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.MUL (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x2)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.MUL64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x3)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.DIV (op, dst))
                              else
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.SDIV (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x3)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.DIV64 (op, dst))
                              else
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.SDIV64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x4)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.OR (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x4)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.OR64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x5)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.AND (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x5)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.AND64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x6)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.LSH (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x6)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.LSH64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x7)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.RSH (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x7)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.RSH64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x8)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.NEG dst)
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x8)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.NEG64 dst)
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0x9)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.MOD (op, dst))
                              else
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.SMOD (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0x9)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.MOD64 (op, dst))
                              else
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.SMOD64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0xa)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.XOR (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0xa)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.XOR64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0xb)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.MOV (op, dst))
                              else
                                FStar_Option.op_let_Question
                                  (get_mov_width offset)
                                  (fun w ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.MOVSX
                                          (src_reg, w, dst)))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0xb)))
                              ->
                              if offset = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.MOV64 (op, dst))
                              else
                                FStar_Option.op_let_Question
                                  (get_mov_width offset)
                                  (fun w ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.MOVSX64
                                          (src_reg, w, dst)))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0xc)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.ARSH (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0xc)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.ARSH64 (op, dst))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x4))) &&
                                (uu___1 = (Prims.of_int (0xd)))
                              ->
                              FStar_Option.op_let_Question
                                (get_bswap_width imm)
                                (fun w ->
                                   let e =
                                     if src = Stdint.Uint64.one
                                     then Bytecode_Instructions.BE
                                     else Bytecode_Instructions.LE in
                                   FStar_Pervasives_Native.Some
                                     (Bytecode_Instructions.END (e, w, dst)))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x7))) &&
                                (uu___1 = (Prims.of_int (0xd)))
                              ->
                              if src = Stdint.Uint64.zero
                              then
                                FStar_Option.op_let_Question
                                  (get_bswap_width imm)
                                  (fun w ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.END64 (w, dst)))
                              else FStar_Pervasives_Native.None
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = Prims.int_zero)
                              ->
                              if src_reg_val = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.JA offset)
                              else FStar_Pervasives_Native.None
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = Prims.int_zero)
                              ->
                              if src_reg_val = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  (Bytecode_Instructions.JA32 imm)
                              else FStar_Pervasives_Native.None
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = Prims.int_one)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JEQ (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = Prims.int_one)
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JEQ32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x2)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JGT (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x2)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JGT32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x3)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JGE (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x3)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JGE32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x4)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSET (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x4)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSET32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x5)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JNE (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x5)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JNE32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x6)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSGT (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x6)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSGT32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x7)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSGE (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0x7)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSGE32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x8)))
                              ->
                              (match FStar_UInt64.v src_reg_val with
                               | uu___2 when uu___2 = Prims.int_zero ->
                                   FStar_Pervasives_Native.Some
                                     (Bytecode_Instructions.CALL
                                        (Bytecode_Instructions.StaticHelperCall,
                                          imm))
                               | uu___2 when uu___2 = Prims.int_one ->
                                   FStar_Pervasives_Native.Some
                                     (Bytecode_Instructions.CALL
                                        (Bytecode_Instructions.PcRelativeCall,
                                          imm))
                               | uu___2 when uu___2 = (Prims.of_int (2)) ->
                                   FStar_Pervasives_Native.Some
                                     (Bytecode_Instructions.CALL
                                        (Bytecode_Instructions.BtfHelperCall,
                                          imm))
                               | uu___2 -> FStar_Pervasives_Native.None)
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0x9)))
                              ->
                              if src_reg_val = Stdint.Uint64.zero
                              then
                                FStar_Pervasives_Native.Some
                                  Bytecode_Instructions.EXIT
                              else FStar_Pervasives_Native.None
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0xa)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JLT (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0xa)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JLT32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0xb)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JLE (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0xb)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JLE32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0xc)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSLT (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0xc)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSLT32
                                   (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x5))) &&
                                (uu___1 = (Prims.of_int (0xd)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSLE (dst, op, offset))
                          | (uu___, uu___1) when
                              (uu___ = (Prims.of_int (0x6))) &&
                                (uu___1 = (Prims.of_int (0xd)))
                              ->
                              FStar_Pervasives_Native.Some
                                (Bytecode_Instructions.JSLE32
                                   (dst, op, offset))
                          | uu___ -> FStar_Pervasives_Native.None))
let (decode_load_store :
  FStar_UInt64.t ->
    FStar_UInt64.t ->
      FStar_UInt64.t ->
        FStar_UInt64.t ->
          FStar_UInt64.t ->
            FStar_UInt64.t ->
              FStar_UInt64.t ->
                FStar_UInt64.t ->
                  Bytecode_Instructions.inst FStar_Pervasives_Native.option)
  =
  fun mode ->
    fun size ->
      fun cls ->
        fun src_reg ->
          fun dst_reg ->
            fun offset ->
              fun imm ->
                fun next_imm ->
                  FStar_Option.op_let_Question (uint_to_reg dst_reg)
                    (fun dst ->
                       FStar_Option.op_let_Question (uint_to_reg src_reg)
                         (fun src ->
                            match ((FStar_UInt64.v cls),
                                    (FStar_UInt64.v mode))
                            with
                            | (uu___, uu___1) when
                                (uu___ = Prims.int_one) &&
                                  (uu___1 = (Prims.of_int (0x3)))
                                ->
                                FStar_Option.op_let_Question
                                  (get_mem_size size)
                                  (fun sz ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.LDX
                                          (src, sz, offset, dst)))
                            | (uu___, uu___1) when
                                (uu___ = (Prims.of_int (0x2))) &&
                                  (uu___1 = (Prims.of_int (0x3)))
                                ->
                                FStar_Option.op_let_Question
                                  (get_mem_size size)
                                  (fun sz ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.ST
                                          (imm, sz, offset, dst)))
                            | (uu___, uu___1) when
                                (uu___ = (Prims.of_int (0x3))) &&
                                  (uu___1 = (Prims.of_int (0x3)))
                                ->
                                FStar_Option.op_let_Question
                                  (get_mem_size size)
                                  (fun sz ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.STX
                                          (src, sz, offset, dst)))
                            | (uu___, uu___1) when
                                (uu___ = Prims.int_one) &&
                                  (uu___1 = (Prims.of_int (0x4)))
                                ->
                                FStar_Option.op_let_Question
                                  (get_memx_size size)
                                  (fun s ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.LDXSX
                                          (src, s, offset, dst)))
                            | (uu___, uu___1) when
                                (uu___ = (Prims.of_int (0x3))) &&
                                  (uu___1 = (Prims.of_int (0x6)))
                                ->
                                FStar_Option.op_let_Question
                                  (get_atomic_size size)
                                  (fun sz ->
                                     FStar_Option.op_let_Question
                                       (get_atomic_op imm)
                                       (fun op ->
                                          FStar_Pervasives_Native.Some
                                            (Bytecode_Instructions.ATOMIC
                                               (src, sz, op, offset, dst))))
                            | (uu___, uu___1) when
                                (uu___ = Prims.int_zero) &&
                                  (uu___1 = Prims.int_zero)
                                ->
                                FStar_Option.op_let_Question
                                  (get_imm_ld_kind src_reg)
                                  (fun k ->
                                     FStar_Pervasives_Native.Some
                                       (Bytecode_Instructions.IMM
                                          (k, imm, next_imm, dst)))
                            | uu___ -> FStar_Pervasives_Native.None))
let (decode_inst :
  FStar_UInt64.t ->
    FStar_UInt64.t FStar_Pervasives_Native.option ->
      Bytecode_Instructions.inst FStar_Pervasives_Native.option)
  =
  fun w ->
    fun next ->
      let opcode = FStar_UInt64.shift_right w (Stdint.Uint32.of_int (56)) in
      let regs =
        FStar_UInt64.logand
          (FStar_UInt64.shift_right w (Stdint.Uint32.of_int (48)))
          (Stdint.Uint64.of_int (0xFF)) in
      let offset =
        decode_offset_le
          (FStar_UInt64.logand
             (FStar_UInt64.shift_right w (Stdint.Uint32.of_int (32)))
             (Stdint.Uint64.of_string "0xFFFF")) in
      let imm =
        decode_imm_le
          (FStar_UInt64.logand w (Stdint.Uint64.of_string "0xFFFFFFFF")) in
      let dst_reg = FStar_UInt64.logand regs (Stdint.Uint64.of_int (0xF)) in
      let src_reg = FStar_UInt64.shift_right regs (Stdint.Uint32.of_int (4)) in
      let cls = FStar_UInt64.logand opcode (Stdint.Uint64.of_int (0x7)) in
      match FStar_UInt64.v cls with
      | uu___ when uu___ = Prims.int_zero ->
          let mode =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (5)) in
          let size =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              (Stdint.Uint64.of_int (0x3)) in
          if (cls = Stdint.Uint64.zero) && (mode = Stdint.Uint64.zero)
          then
            FStar_Option.op_let_Question next
              (fun next_w ->
                 let reserved =
                   FStar_UInt64.shift_right next_w
                     (Stdint.Uint32.of_int (32)) in
                 if reserved <> Stdint.Uint64.zero
                 then FStar_Pervasives_Native.None
                 else
                   (let next_imm = decode_imm_le next_w in
                    decode_load_store mode size cls src_reg dst_reg offset
                      imm next_imm))
          else
            decode_load_store mode size cls src_reg dst_reg offset imm
              Stdint.Uint64.zero
      | uu___ when uu___ = Prims.int_one ->
          let mode =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (5)) in
          let size =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              (Stdint.Uint64.of_int (0x3)) in
          if (cls = Stdint.Uint64.zero) && (mode = Stdint.Uint64.zero)
          then
            FStar_Option.op_let_Question next
              (fun next_w ->
                 let reserved =
                   FStar_UInt64.shift_right next_w
                     (Stdint.Uint32.of_int (32)) in
                 if reserved <> Stdint.Uint64.zero
                 then FStar_Pervasives_Native.None
                 else
                   (let next_imm = decode_imm_le next_w in
                    decode_load_store mode size cls src_reg dst_reg offset
                      imm next_imm))
          else
            decode_load_store mode size cls src_reg dst_reg offset imm
              Stdint.Uint64.zero
      | uu___ when uu___ = (Prims.of_int (0x2)) ->
          let mode =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (5)) in
          let size =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              (Stdint.Uint64.of_int (0x3)) in
          if (cls = Stdint.Uint64.zero) && (mode = Stdint.Uint64.zero)
          then
            FStar_Option.op_let_Question next
              (fun next_w ->
                 let reserved =
                   FStar_UInt64.shift_right next_w
                     (Stdint.Uint32.of_int (32)) in
                 if reserved <> Stdint.Uint64.zero
                 then FStar_Pervasives_Native.None
                 else
                   (let next_imm = decode_imm_le next_w in
                    decode_load_store mode size cls src_reg dst_reg offset
                      imm next_imm))
          else
            decode_load_store mode size cls src_reg dst_reg offset imm
              Stdint.Uint64.zero
      | uu___ when uu___ = (Prims.of_int (0x3)) ->
          let mode =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (5)) in
          let size =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              (Stdint.Uint64.of_int (0x3)) in
          if (cls = Stdint.Uint64.zero) && (mode = Stdint.Uint64.zero)
          then
            FStar_Option.op_let_Question next
              (fun next_w ->
                 let reserved =
                   FStar_UInt64.shift_right next_w
                     (Stdint.Uint32.of_int (32)) in
                 if reserved <> Stdint.Uint64.zero
                 then FStar_Pervasives_Native.None
                 else
                   (let next_imm = decode_imm_le next_w in
                    decode_load_store mode size cls src_reg dst_reg offset
                      imm next_imm))
          else
            decode_load_store mode size cls src_reg dst_reg offset imm
              Stdint.Uint64.zero
      | uu___ when uu___ = (Prims.of_int (0x4)) ->
          let code =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (4)) in
          let src =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              Stdint.Uint64.one in
          decode_alu_jump code src cls src_reg dst_reg offset imm
      | uu___ when uu___ = (Prims.of_int (0x5)) ->
          let code =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (4)) in
          let src =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              Stdint.Uint64.one in
          decode_alu_jump code src cls src_reg dst_reg offset imm
      | uu___ when uu___ = (Prims.of_int (0x6)) ->
          let code =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (4)) in
          let src =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              Stdint.Uint64.one in
          decode_alu_jump code src cls src_reg dst_reg offset imm
      | uu___ when uu___ = (Prims.of_int (0x7)) ->
          let code =
            FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (4)) in
          let src =
            FStar_UInt64.logand
              (FStar_UInt64.shift_right opcode (Stdint.Uint32.of_int (3)))
              Stdint.Uint64.one in
          decode_alu_jump code src cls src_reg dst_reg offset imm
      | uu___ -> FStar_Pervasives_Native.None
