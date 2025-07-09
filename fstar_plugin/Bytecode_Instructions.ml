open Prims
type imm = FStar_UInt64.t
type next_imm = FStar_UInt64.t
type offset = FStar_UInt64.t
type reg =
  | R0 
  | R1 
  | R2 
  | R3 
  | R4 
  | R5 
  | R6 
  | R7 
  | R8 
  | R9 
  | R10 
let (uu___is_R0 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R0 -> true | uu___ -> false
let (uu___is_R1 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R1 -> true | uu___ -> false
let (uu___is_R2 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R2 -> true | uu___ -> false
let (uu___is_R3 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R3 -> true | uu___ -> false
let (uu___is_R4 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R4 -> true | uu___ -> false
let (uu___is_R5 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R5 -> true | uu___ -> false
let (uu___is_R6 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R6 -> true | uu___ -> false
let (uu___is_R7 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R7 -> true | uu___ -> false
let (uu___is_R8 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R8 -> true | uu___ -> false
let (uu___is_R9 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R9 -> true | uu___ -> false
let (uu___is_R10 : reg -> Prims.bool) =
  fun projectee -> match projectee with | R10 -> true | uu___ -> false
type dst = reg
type src = reg
type operand =
  | RegOp of src 
  | ImmOp of imm 
let (uu___is_RegOp : operand -> Prims.bool) =
  fun projectee -> match projectee with | RegOp _0 -> true | uu___ -> false
let (__proj__RegOp__item___0 : operand -> src) =
  fun projectee -> match projectee with | RegOp _0 -> _0
let (uu___is_ImmOp : operand -> Prims.bool) =
  fun projectee -> match projectee with | ImmOp _0 -> true | uu___ -> false
let (__proj__ImmOp__item___0 : operand -> imm) =
  fun projectee -> match projectee with | ImmOp _0 -> _0
type endianness =
  | LE 
  | BE 
let (uu___is_LE : endianness -> Prims.bool) =
  fun projectee -> match projectee with | LE -> true | uu___ -> false
let (uu___is_BE : endianness -> Prims.bool) =
  fun projectee -> match projectee with | BE -> true | uu___ -> false
type sx_width =
  | W8 
  | W16 
  | W32 
let (uu___is_W8 : sx_width -> Prims.bool) =
  fun projectee -> match projectee with | W8 -> true | uu___ -> false
let (uu___is_W16 : sx_width -> Prims.bool) =
  fun projectee -> match projectee with | W16 -> true | uu___ -> false
let (uu___is_W32 : sx_width -> Prims.bool) =
  fun projectee -> match projectee with | W32 -> true | uu___ -> false
type bswap_width =
  | B16 
  | B32 
  | B64 
let (uu___is_B16 : bswap_width -> Prims.bool) =
  fun projectee -> match projectee with | B16 -> true | uu___ -> false
let (uu___is_B32 : bswap_width -> Prims.bool) =
  fun projectee -> match projectee with | B32 -> true | uu___ -> false
let (uu___is_B64 : bswap_width -> Prims.bool) =
  fun projectee -> match projectee with | B64 -> true | uu___ -> false
type call_kind =
  | StaticHelperCall 
  | PcRelativeCall 
  | BtfHelperCall 
let (uu___is_StaticHelperCall : call_kind -> Prims.bool) =
  fun projectee ->
    match projectee with | StaticHelperCall -> true | uu___ -> false
let (uu___is_PcRelativeCall : call_kind -> Prims.bool) =
  fun projectee ->
    match projectee with | PcRelativeCall -> true | uu___ -> false
let (uu___is_BtfHelperCall : call_kind -> Prims.bool) =
  fun projectee ->
    match projectee with | BtfHelperCall -> true | uu___ -> false
type size =
  | W 
  | H 
  | B 
  | DW 
let (uu___is_W : size -> Prims.bool) =
  fun projectee -> match projectee with | W -> true | uu___ -> false
let (uu___is_H : size -> Prims.bool) =
  fun projectee -> match projectee with | H -> true | uu___ -> false
let (uu___is_B : size -> Prims.bool) =
  fun projectee -> match projectee with | B -> true | uu___ -> false
let (uu___is_DW : size -> Prims.bool) =
  fun projectee -> match projectee with | DW -> true | uu___ -> false
type atomic_size =
  | AtomicW 
  | AtomicDW 
let (uu___is_AtomicW : atomic_size -> Prims.bool) =
  fun projectee -> match projectee with | AtomicW -> true | uu___ -> false
let (uu___is_AtomicDW : atomic_size -> Prims.bool) =
  fun projectee -> match projectee with | AtomicDW -> true | uu___ -> false
type sx_size =
  | SxW 
  | SxH 
  | SxB 
let (uu___is_SxW : sx_size -> Prims.bool) =
  fun projectee -> match projectee with | SxW -> true | uu___ -> false
let (uu___is_SxH : sx_size -> Prims.bool) =
  fun projectee -> match projectee with | SxH -> true | uu___ -> false
let (uu___is_SxB : sx_size -> Prims.bool) =
  fun projectee -> match projectee with | SxB -> true | uu___ -> false
type atomic_op =
  | Add 
  | Or 
  | And 
  | Xor 
  | FetchAdd 
  | FetchOr 
  | FetchAnd 
  | FetchXor 
  | Xchg 
  | CmpXchg 
let (uu___is_Add : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | Add -> true | uu___ -> false
let (uu___is_Or : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | Or -> true | uu___ -> false
let (uu___is_And : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | And -> true | uu___ -> false
let (uu___is_Xor : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | Xor -> true | uu___ -> false
let (uu___is_FetchAdd : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | FetchAdd -> true | uu___ -> false
let (uu___is_FetchOr : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | FetchOr -> true | uu___ -> false
let (uu___is_FetchAnd : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | FetchAnd -> true | uu___ -> false
let (uu___is_FetchXor : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | FetchXor -> true | uu___ -> false
let (uu___is_Xchg : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | Xchg -> true | uu___ -> false
let (uu___is_CmpXchg : atomic_op -> Prims.bool) =
  fun projectee -> match projectee with | CmpXchg -> true | uu___ -> false
type imm_ld_kind =
  | LDI64 
  | MapFd 
  | MapValFd 
  | VarAddr 
  | CodeAddr 
  | MapIdx 
  | MapValIdx 
let (uu___is_LDI64 : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | LDI64 -> true | uu___ -> false
let (uu___is_MapFd : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | MapFd -> true | uu___ -> false
let (uu___is_MapValFd : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | MapValFd -> true | uu___ -> false
let (uu___is_VarAddr : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | VarAddr -> true | uu___ -> false
let (uu___is_CodeAddr : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | CodeAddr -> true | uu___ -> false
let (uu___is_MapIdx : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | MapIdx -> true | uu___ -> false
let (uu___is_MapValIdx : imm_ld_kind -> Prims.bool) =
  fun projectee -> match projectee with | MapValIdx -> true | uu___ -> false
type inst =
  | ADD of operand * dst 
  | ADD64 of operand * dst 
  | SUB of operand * dst 
  | SUB64 of operand * dst 
  | MUL of operand * dst 
  | MUL64 of operand * dst 
  | DIV of operand * dst 
  | DIV64 of operand * dst 
  | SDIV of operand * dst 
  | SDIV64 of operand * dst 
  | MOD of operand * dst 
  | MOD64 of operand * dst 
  | SMOD of operand * dst 
  | SMOD64 of operand * dst 
  | OR of operand * dst 
  | OR64 of operand * dst 
  | AND of operand * dst 
  | AND64 of operand * dst 
  | XOR of operand * dst 
  | XOR64 of operand * dst 
  | LSH of operand * dst 
  | LSH64 of operand * dst 
  | RSH of operand * dst 
  | RSH64 of operand * dst 
  | ARSH of operand * dst 
  | ARSH64 of operand * dst 
  | MOV of operand * dst 
  | MOV64 of operand * dst 
  | MOVSX of src * sx_width * dst 
  | MOVSX64 of src * sx_width * dst 
  | NEG of dst 
  | NEG64 of dst 
  | END of endianness * bswap_width * dst 
  | END64 of bswap_width * dst 
  | JA of offset 
  | JA32 of imm 
  | JEQ of dst * operand * offset 
  | JEQ32 of dst * operand * offset 
  | JGT of dst * operand * offset 
  | JGT32 of dst * operand * offset 
  | JGE of dst * operand * offset 
  | JGE32 of dst * operand * offset 
  | JSET of dst * operand * offset 
  | JSET32 of dst * operand * offset 
  | JNE of dst * operand * offset 
  | JNE32 of dst * operand * offset 
  | JSGT of dst * operand * offset 
  | JSGT32 of dst * operand * offset 
  | JSGE of dst * operand * offset 
  | JSGE32 of dst * operand * offset 
  | CALL of call_kind * imm 
  | EXIT 
  | JLT of dst * operand * offset 
  | JLT32 of dst * operand * offset 
  | JLE of dst * operand * offset 
  | JLE32 of dst * operand * offset 
  | JSLT of dst * operand * offset 
  | JSLT32 of dst * operand * offset 
  | JSLE of dst * operand * offset 
  | JSLE32 of dst * operand * offset 
  | ST of imm * size * offset * dst 
  | STX of src * size * offset * dst 
  | LDX of src * size * offset * dst 
  | LDXSX of src * sx_size * offset * dst 
  | ATOMIC of src * atomic_size * atomic_op * offset * dst 
  | IMM of imm_ld_kind * imm * next_imm * dst 
let (uu___is_ADD : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | ADD (_0, _1) -> true | uu___ -> false
let (__proj__ADD__item___0 : inst -> operand) =
  fun projectee -> match projectee with | ADD (_0, _1) -> _0
let (__proj__ADD__item___1 : inst -> dst) =
  fun projectee -> match projectee with | ADD (_0, _1) -> _1
let (uu___is_ADD64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | ADD64 (_0, _1) -> true | uu___ -> false
let (__proj__ADD64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | ADD64 (_0, _1) -> _0
let (__proj__ADD64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | ADD64 (_0, _1) -> _1
let (uu___is_SUB : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SUB (_0, _1) -> true | uu___ -> false
let (__proj__SUB__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SUB (_0, _1) -> _0
let (__proj__SUB__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SUB (_0, _1) -> _1
let (uu___is_SUB64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SUB64 (_0, _1) -> true | uu___ -> false
let (__proj__SUB64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SUB64 (_0, _1) -> _0
let (__proj__SUB64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SUB64 (_0, _1) -> _1
let (uu___is_MUL : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MUL (_0, _1) -> true | uu___ -> false
let (__proj__MUL__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MUL (_0, _1) -> _0
let (__proj__MUL__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MUL (_0, _1) -> _1
let (uu___is_MUL64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MUL64 (_0, _1) -> true | uu___ -> false
let (__proj__MUL64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MUL64 (_0, _1) -> _0
let (__proj__MUL64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MUL64 (_0, _1) -> _1
let (uu___is_DIV : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | DIV (_0, _1) -> true | uu___ -> false
let (__proj__DIV__item___0 : inst -> operand) =
  fun projectee -> match projectee with | DIV (_0, _1) -> _0
let (__proj__DIV__item___1 : inst -> dst) =
  fun projectee -> match projectee with | DIV (_0, _1) -> _1
let (uu___is_DIV64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | DIV64 (_0, _1) -> true | uu___ -> false
let (__proj__DIV64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | DIV64 (_0, _1) -> _0
let (__proj__DIV64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | DIV64 (_0, _1) -> _1
let (uu___is_SDIV : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SDIV (_0, _1) -> true | uu___ -> false
let (__proj__SDIV__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SDIV (_0, _1) -> _0
let (__proj__SDIV__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SDIV (_0, _1) -> _1
let (uu___is_SDIV64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SDIV64 (_0, _1) -> true | uu___ -> false
let (__proj__SDIV64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SDIV64 (_0, _1) -> _0
let (__proj__SDIV64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SDIV64 (_0, _1) -> _1
let (uu___is_MOD : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOD (_0, _1) -> true | uu___ -> false
let (__proj__MOD__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MOD (_0, _1) -> _0
let (__proj__MOD__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MOD (_0, _1) -> _1
let (uu___is_MOD64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOD64 (_0, _1) -> true | uu___ -> false
let (__proj__MOD64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MOD64 (_0, _1) -> _0
let (__proj__MOD64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MOD64 (_0, _1) -> _1
let (uu___is_SMOD : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SMOD (_0, _1) -> true | uu___ -> false
let (__proj__SMOD__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SMOD (_0, _1) -> _0
let (__proj__SMOD__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SMOD (_0, _1) -> _1
let (uu___is_SMOD64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | SMOD64 (_0, _1) -> true | uu___ -> false
let (__proj__SMOD64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | SMOD64 (_0, _1) -> _0
let (__proj__SMOD64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | SMOD64 (_0, _1) -> _1
let (uu___is_OR : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | OR (_0, _1) -> true | uu___ -> false
let (__proj__OR__item___0 : inst -> operand) =
  fun projectee -> match projectee with | OR (_0, _1) -> _0
let (__proj__OR__item___1 : inst -> dst) =
  fun projectee -> match projectee with | OR (_0, _1) -> _1
let (uu___is_OR64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | OR64 (_0, _1) -> true | uu___ -> false
let (__proj__OR64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | OR64 (_0, _1) -> _0
let (__proj__OR64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | OR64 (_0, _1) -> _1
let (uu___is_AND : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | AND (_0, _1) -> true | uu___ -> false
let (__proj__AND__item___0 : inst -> operand) =
  fun projectee -> match projectee with | AND (_0, _1) -> _0
let (__proj__AND__item___1 : inst -> dst) =
  fun projectee -> match projectee with | AND (_0, _1) -> _1
let (uu___is_AND64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | AND64 (_0, _1) -> true | uu___ -> false
let (__proj__AND64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | AND64 (_0, _1) -> _0
let (__proj__AND64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | AND64 (_0, _1) -> _1
let (uu___is_XOR : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | XOR (_0, _1) -> true | uu___ -> false
let (__proj__XOR__item___0 : inst -> operand) =
  fun projectee -> match projectee with | XOR (_0, _1) -> _0
let (__proj__XOR__item___1 : inst -> dst) =
  fun projectee -> match projectee with | XOR (_0, _1) -> _1
let (uu___is_XOR64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | XOR64 (_0, _1) -> true | uu___ -> false
let (__proj__XOR64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | XOR64 (_0, _1) -> _0
let (__proj__XOR64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | XOR64 (_0, _1) -> _1
let (uu___is_LSH : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | LSH (_0, _1) -> true | uu___ -> false
let (__proj__LSH__item___0 : inst -> operand) =
  fun projectee -> match projectee with | LSH (_0, _1) -> _0
let (__proj__LSH__item___1 : inst -> dst) =
  fun projectee -> match projectee with | LSH (_0, _1) -> _1
let (uu___is_LSH64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | LSH64 (_0, _1) -> true | uu___ -> false
let (__proj__LSH64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | LSH64 (_0, _1) -> _0
let (__proj__LSH64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | LSH64 (_0, _1) -> _1
let (uu___is_RSH : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | RSH (_0, _1) -> true | uu___ -> false
let (__proj__RSH__item___0 : inst -> operand) =
  fun projectee -> match projectee with | RSH (_0, _1) -> _0
let (__proj__RSH__item___1 : inst -> dst) =
  fun projectee -> match projectee with | RSH (_0, _1) -> _1
let (uu___is_RSH64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | RSH64 (_0, _1) -> true | uu___ -> false
let (__proj__RSH64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | RSH64 (_0, _1) -> _0
let (__proj__RSH64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | RSH64 (_0, _1) -> _1
let (uu___is_ARSH : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | ARSH (_0, _1) -> true | uu___ -> false
let (__proj__ARSH__item___0 : inst -> operand) =
  fun projectee -> match projectee with | ARSH (_0, _1) -> _0
let (__proj__ARSH__item___1 : inst -> dst) =
  fun projectee -> match projectee with | ARSH (_0, _1) -> _1
let (uu___is_ARSH64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | ARSH64 (_0, _1) -> true | uu___ -> false
let (__proj__ARSH64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | ARSH64 (_0, _1) -> _0
let (__proj__ARSH64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | ARSH64 (_0, _1) -> _1
let (uu___is_MOV : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOV (_0, _1) -> true | uu___ -> false
let (__proj__MOV__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MOV (_0, _1) -> _0
let (__proj__MOV__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MOV (_0, _1) -> _1
let (uu___is_MOV64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOV64 (_0, _1) -> true | uu___ -> false
let (__proj__MOV64__item___0 : inst -> operand) =
  fun projectee -> match projectee with | MOV64 (_0, _1) -> _0
let (__proj__MOV64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | MOV64 (_0, _1) -> _1
let (uu___is_MOVSX : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOVSX (_0, _1, _2) -> true | uu___ -> false
let (__proj__MOVSX__item___0 : inst -> src) =
  fun projectee -> match projectee with | MOVSX (_0, _1, _2) -> _0
let (__proj__MOVSX__item___1 : inst -> sx_width) =
  fun projectee -> match projectee with | MOVSX (_0, _1, _2) -> _1
let (__proj__MOVSX__item___2 : inst -> dst) =
  fun projectee -> match projectee with | MOVSX (_0, _1, _2) -> _2
let (uu___is_MOVSX64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | MOVSX64 (_0, _1, _2) -> true | uu___ -> false
let (__proj__MOVSX64__item___0 : inst -> src) =
  fun projectee -> match projectee with | MOVSX64 (_0, _1, _2) -> _0
let (__proj__MOVSX64__item___1 : inst -> sx_width) =
  fun projectee -> match projectee with | MOVSX64 (_0, _1, _2) -> _1
let (__proj__MOVSX64__item___2 : inst -> dst) =
  fun projectee -> match projectee with | MOVSX64 (_0, _1, _2) -> _2
let (uu___is_NEG : inst -> Prims.bool) =
  fun projectee -> match projectee with | NEG _0 -> true | uu___ -> false
let (__proj__NEG__item___0 : inst -> dst) =
  fun projectee -> match projectee with | NEG _0 -> _0
let (uu___is_NEG64 : inst -> Prims.bool) =
  fun projectee -> match projectee with | NEG64 _0 -> true | uu___ -> false
let (__proj__NEG64__item___0 : inst -> dst) =
  fun projectee -> match projectee with | NEG64 _0 -> _0
let (uu___is_END : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | END (_0, _1, _2) -> true | uu___ -> false
let (__proj__END__item___0 : inst -> endianness) =
  fun projectee -> match projectee with | END (_0, _1, _2) -> _0
let (__proj__END__item___1 : inst -> bswap_width) =
  fun projectee -> match projectee with | END (_0, _1, _2) -> _1
let (__proj__END__item___2 : inst -> dst) =
  fun projectee -> match projectee with | END (_0, _1, _2) -> _2
let (uu___is_END64 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | END64 (_0, _1) -> true | uu___ -> false
let (__proj__END64__item___0 : inst -> bswap_width) =
  fun projectee -> match projectee with | END64 (_0, _1) -> _0
let (__proj__END64__item___1 : inst -> dst) =
  fun projectee -> match projectee with | END64 (_0, _1) -> _1
let (uu___is_JA : inst -> Prims.bool) =
  fun projectee -> match projectee with | JA _0 -> true | uu___ -> false
let (__proj__JA__item___0 : inst -> offset) =
  fun projectee -> match projectee with | JA _0 -> _0
let (uu___is_JA32 : inst -> Prims.bool) =
  fun projectee -> match projectee with | JA32 _0 -> true | uu___ -> false
let (__proj__JA32__item___0 : inst -> imm) =
  fun projectee -> match projectee with | JA32 _0 -> _0
let (uu___is_JEQ : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JEQ (_0, _1, _2) -> true | uu___ -> false
let (__proj__JEQ__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JEQ (_0, _1, _2) -> _0
let (__proj__JEQ__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JEQ (_0, _1, _2) -> _1
let (__proj__JEQ__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JEQ (_0, _1, _2) -> _2
let (uu___is_JEQ32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JEQ32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JEQ32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JEQ32 (_0, _1, _2) -> _0
let (__proj__JEQ32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JEQ32 (_0, _1, _2) -> _1
let (__proj__JEQ32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JEQ32 (_0, _1, _2) -> _2
let (uu___is_JGT : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JGT (_0, _1, _2) -> true | uu___ -> false
let (__proj__JGT__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JGT (_0, _1, _2) -> _0
let (__proj__JGT__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JGT (_0, _1, _2) -> _1
let (__proj__JGT__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JGT (_0, _1, _2) -> _2
let (uu___is_JGT32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JGT32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JGT32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JGT32 (_0, _1, _2) -> _0
let (__proj__JGT32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JGT32 (_0, _1, _2) -> _1
let (__proj__JGT32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JGT32 (_0, _1, _2) -> _2
let (uu___is_JGE : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JGE (_0, _1, _2) -> true | uu___ -> false
let (__proj__JGE__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JGE (_0, _1, _2) -> _0
let (__proj__JGE__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JGE (_0, _1, _2) -> _1
let (__proj__JGE__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JGE (_0, _1, _2) -> _2
let (uu___is_JGE32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JGE32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JGE32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JGE32 (_0, _1, _2) -> _0
let (__proj__JGE32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JGE32 (_0, _1, _2) -> _1
let (__proj__JGE32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JGE32 (_0, _1, _2) -> _2
let (uu___is_JSET : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSET (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSET__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSET (_0, _1, _2) -> _0
let (__proj__JSET__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSET (_0, _1, _2) -> _1
let (__proj__JSET__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSET (_0, _1, _2) -> _2
let (uu___is_JSET32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSET32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSET32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSET32 (_0, _1, _2) -> _0
let (__proj__JSET32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSET32 (_0, _1, _2) -> _1
let (__proj__JSET32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSET32 (_0, _1, _2) -> _2
let (uu___is_JNE : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JNE (_0, _1, _2) -> true | uu___ -> false
let (__proj__JNE__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JNE (_0, _1, _2) -> _0
let (__proj__JNE__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JNE (_0, _1, _2) -> _1
let (__proj__JNE__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JNE (_0, _1, _2) -> _2
let (uu___is_JNE32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JNE32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JNE32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JNE32 (_0, _1, _2) -> _0
let (__proj__JNE32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JNE32 (_0, _1, _2) -> _1
let (__proj__JNE32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JNE32 (_0, _1, _2) -> _2
let (uu___is_JSGT : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSGT (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSGT__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSGT (_0, _1, _2) -> _0
let (__proj__JSGT__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSGT (_0, _1, _2) -> _1
let (__proj__JSGT__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSGT (_0, _1, _2) -> _2
let (uu___is_JSGT32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSGT32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSGT32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSGT32 (_0, _1, _2) -> _0
let (__proj__JSGT32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSGT32 (_0, _1, _2) -> _1
let (__proj__JSGT32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSGT32 (_0, _1, _2) -> _2
let (uu___is_JSGE : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSGE (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSGE__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSGE (_0, _1, _2) -> _0
let (__proj__JSGE__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSGE (_0, _1, _2) -> _1
let (__proj__JSGE__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSGE (_0, _1, _2) -> _2
let (uu___is_JSGE32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSGE32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSGE32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSGE32 (_0, _1, _2) -> _0
let (__proj__JSGE32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSGE32 (_0, _1, _2) -> _1
let (__proj__JSGE32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSGE32 (_0, _1, _2) -> _2
let (uu___is_CALL : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | CALL (_0, _1) -> true | uu___ -> false
let (__proj__CALL__item___0 : inst -> call_kind) =
  fun projectee -> match projectee with | CALL (_0, _1) -> _0
let (__proj__CALL__item___1 : inst -> imm) =
  fun projectee -> match projectee with | CALL (_0, _1) -> _1
let (uu___is_EXIT : inst -> Prims.bool) =
  fun projectee -> match projectee with | EXIT -> true | uu___ -> false
let (uu___is_JLT : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JLT (_0, _1, _2) -> true | uu___ -> false
let (__proj__JLT__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JLT (_0, _1, _2) -> _0
let (__proj__JLT__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JLT (_0, _1, _2) -> _1
let (__proj__JLT__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JLT (_0, _1, _2) -> _2
let (uu___is_JLT32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JLT32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JLT32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JLT32 (_0, _1, _2) -> _0
let (__proj__JLT32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JLT32 (_0, _1, _2) -> _1
let (__proj__JLT32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JLT32 (_0, _1, _2) -> _2
let (uu___is_JLE : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JLE (_0, _1, _2) -> true | uu___ -> false
let (__proj__JLE__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JLE (_0, _1, _2) -> _0
let (__proj__JLE__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JLE (_0, _1, _2) -> _1
let (__proj__JLE__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JLE (_0, _1, _2) -> _2
let (uu___is_JLE32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JLE32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JLE32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JLE32 (_0, _1, _2) -> _0
let (__proj__JLE32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JLE32 (_0, _1, _2) -> _1
let (__proj__JLE32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JLE32 (_0, _1, _2) -> _2
let (uu___is_JSLT : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSLT (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSLT__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSLT (_0, _1, _2) -> _0
let (__proj__JSLT__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSLT (_0, _1, _2) -> _1
let (__proj__JSLT__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSLT (_0, _1, _2) -> _2
let (uu___is_JSLT32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSLT32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSLT32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSLT32 (_0, _1, _2) -> _0
let (__proj__JSLT32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSLT32 (_0, _1, _2) -> _1
let (__proj__JSLT32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSLT32 (_0, _1, _2) -> _2
let (uu___is_JSLE : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSLE (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSLE__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSLE (_0, _1, _2) -> _0
let (__proj__JSLE__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSLE (_0, _1, _2) -> _1
let (__proj__JSLE__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSLE (_0, _1, _2) -> _2
let (uu___is_JSLE32 : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | JSLE32 (_0, _1, _2) -> true | uu___ -> false
let (__proj__JSLE32__item___0 : inst -> dst) =
  fun projectee -> match projectee with | JSLE32 (_0, _1, _2) -> _0
let (__proj__JSLE32__item___1 : inst -> operand) =
  fun projectee -> match projectee with | JSLE32 (_0, _1, _2) -> _1
let (__proj__JSLE32__item___2 : inst -> offset) =
  fun projectee -> match projectee with | JSLE32 (_0, _1, _2) -> _2
let (uu___is_ST : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | ST (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__ST__item___0 : inst -> imm) =
  fun projectee -> match projectee with | ST (_0, _1, _2, _3) -> _0
let (__proj__ST__item___1 : inst -> size) =
  fun projectee -> match projectee with | ST (_0, _1, _2, _3) -> _1
let (__proj__ST__item___2 : inst -> offset) =
  fun projectee -> match projectee with | ST (_0, _1, _2, _3) -> _2
let (__proj__ST__item___3 : inst -> dst) =
  fun projectee -> match projectee with | ST (_0, _1, _2, _3) -> _3
let (uu___is_STX : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | STX (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__STX__item___0 : inst -> src) =
  fun projectee -> match projectee with | STX (_0, _1, _2, _3) -> _0
let (__proj__STX__item___1 : inst -> size) =
  fun projectee -> match projectee with | STX (_0, _1, _2, _3) -> _1
let (__proj__STX__item___2 : inst -> offset) =
  fun projectee -> match projectee with | STX (_0, _1, _2, _3) -> _2
let (__proj__STX__item___3 : inst -> dst) =
  fun projectee -> match projectee with | STX (_0, _1, _2, _3) -> _3
let (uu___is_LDX : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | LDX (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__LDX__item___0 : inst -> src) =
  fun projectee -> match projectee with | LDX (_0, _1, _2, _3) -> _0
let (__proj__LDX__item___1 : inst -> size) =
  fun projectee -> match projectee with | LDX (_0, _1, _2, _3) -> _1
let (__proj__LDX__item___2 : inst -> offset) =
  fun projectee -> match projectee with | LDX (_0, _1, _2, _3) -> _2
let (__proj__LDX__item___3 : inst -> dst) =
  fun projectee -> match projectee with | LDX (_0, _1, _2, _3) -> _3
let (uu___is_LDXSX : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | LDXSX (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__LDXSX__item___0 : inst -> src) =
  fun projectee -> match projectee with | LDXSX (_0, _1, _2, _3) -> _0
let (__proj__LDXSX__item___1 : inst -> sx_size) =
  fun projectee -> match projectee with | LDXSX (_0, _1, _2, _3) -> _1
let (__proj__LDXSX__item___2 : inst -> offset) =
  fun projectee -> match projectee with | LDXSX (_0, _1, _2, _3) -> _2
let (__proj__LDXSX__item___3 : inst -> dst) =
  fun projectee -> match projectee with | LDXSX (_0, _1, _2, _3) -> _3
let (uu___is_ATOMIC : inst -> Prims.bool) =
  fun projectee ->
    match projectee with
    | ATOMIC (_0, _1, _2, _3, _4) -> true
    | uu___ -> false
let (__proj__ATOMIC__item___0 : inst -> src) =
  fun projectee -> match projectee with | ATOMIC (_0, _1, _2, _3, _4) -> _0
let (__proj__ATOMIC__item___1 : inst -> atomic_size) =
  fun projectee -> match projectee with | ATOMIC (_0, _1, _2, _3, _4) -> _1
let (__proj__ATOMIC__item___2 : inst -> atomic_op) =
  fun projectee -> match projectee with | ATOMIC (_0, _1, _2, _3, _4) -> _2
let (__proj__ATOMIC__item___3 : inst -> offset) =
  fun projectee -> match projectee with | ATOMIC (_0, _1, _2, _3, _4) -> _3
let (__proj__ATOMIC__item___4 : inst -> dst) =
  fun projectee -> match projectee with | ATOMIC (_0, _1, _2, _3, _4) -> _4
let (uu___is_IMM : inst -> Prims.bool) =
  fun projectee ->
    match projectee with | IMM (_0, _1, _2, _3) -> true | uu___ -> false
let (__proj__IMM__item___0 : inst -> imm_ld_kind) =
  fun projectee -> match projectee with | IMM (_0, _1, _2, _3) -> _0
let (__proj__IMM__item___1 : inst -> imm) =
  fun projectee -> match projectee with | IMM (_0, _1, _2, _3) -> _1
let (__proj__IMM__item___2 : inst -> next_imm) =
  fun projectee -> match projectee with | IMM (_0, _1, _2, _3) -> _2
let (__proj__IMM__item___3 : inst -> dst) =
  fun projectee -> match projectee with | IMM (_0, _1, _2, _3) -> _3
