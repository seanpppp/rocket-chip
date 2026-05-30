// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import chisel3.{DontCare, WireInit, withClock, withReset}
import chisel3.experimental.SourceInfo
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

object InstDecode {
  val FN_SZ = 33
  val LAT_SZ = 5

  val operand1:List[(Long, Int, Int, String)] = List( // 1 operand
    (0x000000800L,  8,  8, "acos"),
    (0x000800000L,  9,  9, "acosh"),
    (0x040000800L, 10, 10, "acospi"),
    (0x000000400L,  8,  8, "asin"),
    (0x001000000L,  9,  9, "asinh"),
    (0x040000400L, 10, 10, "asinpi"),
    (0x000000001L,  8,  8, "atan"),
    (0x002000000L, 10, 10, "atanh"),
    (0x040000001L, 11, 11, "atanpi"),
    (0x010000000L,  4,  4, "cbrt"),
    (0x000000004L,  2,  2, "ceil"),
    (0x000001000L, 10, 10, "cos"),
    (0x000100000L, 10, 10, "cosh"),
    (0x040001000L,  6,  6, "cospi"),
    (0x004000000L,  8,  8, "erf"),
    (0x044000000L,  8,  8, "erfc"),
    (0x000010000L, 10, 10, "exp"),
    (0x000080000L, 10, 10, "expm1"),
    (0x040008000L, 10, 10, "exp10"),
    (0x000008000L, 10, 10, "exp2"),
    (0x100000004L,  2,  2, "cfabs"),
    (0x040000004L,  2,  2, "floor"),
    (0x080000004L,  2,  2, "fract1"),
    (0x0c0000004L,  2,  2, "fract2"),
    (0x180000001L,  2,  2, "frexp1"),
    (0x1c0000001L,  2,  2, "frexp2"),
    (0x140000008L,  2,  2, "ilogb"),
    (0x048000000L, 14, 14, "lgamma"),
    (0x088000000L,  2,  2, "lgammar2"),
    (0x000000040L,  8,  8, "ln"),
    (0x040000020L,  8,  8, "log10"),
    (0x040000040L,  8,  8, "log1p"),
    (0x000000020L,  8,  8, "log2"),
    (0x1c0000002L,  2,  2, "logb"),
    (0x100000001L,  2,  2, "modf1"),
    (0x140000001L,  2,  2, "modf2"),
    (0x100000008L,  2,  2, "nan"),
    (0x000000200L,  4,  4, "rcp"),
    (0x100000002L,  2,  2, "rint"),
    (0x140000002L,  2,  2, "round"),
    (0x000000100L,  4,  4, "rsqrt"),
    (0x040080000L, 13, 13, "sigmoid"),
    (0x000002000L, 10, 10, "sin"),
    (0x000200000L, 10, 10, "sinh"),
    (0x040002000L,  6,  6, "sinpi"),
    (0x000004000L, 15, 15, "tan"),
    (0x000400000L, 15, 15, "tanh"),
    (0x040004000L, 12, 12, "tanpi"),
    (0x008000000L, 18, 18, "tgamma"),
    (0x180000002L,  2,  2, "trunc"))

  val operand2:List[(Long, Int, Int, String)] = List( // 2 operand
    (0x000000002L, 10, 10, "atan2"),
    (0x040000002L, 11, 11, "atan2pi"),
    (0x0e0000000L,  2,  2, "copysign"),
    (0x020000000L,  2,  2, "fdim"),
    (0x000020000L, 12, 12, "fmod"),
    (0x000040000L,  7,  7, "hypot"),
    (0x060000000L,  2,  2, "ldexp"),
    (0x000000008L,  2,  2, "maxmag"),
    (0x040000008L,  2,  2, "minmag"),
    (0x0a0000000L,  2,  2, "nextafter"),
    (0x000000010L, 14, 14, "pow"),
    (0x080000010L, 14, 14, "pown"),
    (0x040000010L, 14, 14, "powr"),
    (0x080020000L, 12, 12, "remainder"),
    (0x0c0020000L, 12, 12, "remquo"),
    (0x0c0000010L, 14, 14, "rootn"))
val maxLatency = (operand1 ++ operand2).map(_._3).max  //max latency

  implicit class LongB(val x:Long) extends AnyVal {def B = BitPat(x.U(FN_SZ.W))} 
  implicit class IntB(val x:Int) extends AnyVal {def B = BitPat(x.U(LAT_SZ.W))} 
  implicit class StrB(val x:String) extends AnyVal {def B = BitPat("b"+x)}
  implicit class IntZ(val x:Int) extends AnyVal {def Z = 0.U(x.W)} 

  // instruction patterns are generated here. 
  // 1, append it to opcodes and make, which copies the results to various ecoding.h
  // 2, manually copy SCM match, mask and declare_insn from ecoding.h to riscv-opc.h
  // 3, manually define DECLARE_INSN and copy SCM DECLARE_INSN to riscv-opc.c  
  // 4, manually change disasm.cc

  println(operand1.size + " Single precision 1 operand instructions")
  println(operand2.size + " Single precision 2 operand instructions")
  println("\n# Append below to rocket-chip/riscv-tools/riscv-opcodes/opcodes and make\n")
  // generate grey codes
  def grey(n: Int): List[String] = if (n == 1) List("0","1") else (grey(n-1).map("0"+_) ++ grey(n-1).reverse.map("1"+_))
  def pi(a:String) = Integer.parseInt(a.filter(x => "01".contains(x)),2)

  val fun = (// drop 2 to avoid overlap with s1
    (operand1 zip grey(6)         map{case (x,g) => (g,false,"0","0",x._1,x._2,x._4++".s")}) ++
    // (operand1 zip grey(6)         map{case (x,g) => (g,false,"1","0",x._1,x._2,x._4++".d.s")}) ++
    // (operand1 zip grey(6)         map{case (x,g) => (g,false,"0","1",x._1,x._3,x._4++".d")}) ++
    // (operand1 zip grey(6)         map{case (x,g) => (g,false,"1","1",x._1,x._3,x._4++".s.d")}) ++
    (operand2 zip grey(5).drop(2) map{case (x,g) => (g,true, "0","0",x._1,x._2,x._4++".s")}) // ++
    // (operand2 zip grey(5).drop(2) map{case (x,g) => (g,true, "1","0",x._1,x._2,x._4++".d.s")}) ++
    // (operand2 zip grey(5).drop(2) map{case (x,g) => (g,true, "0","1",x._1,x._3,x._4++".d")}) ++
    // (operand2 zip grey(5).drop(2) map{case (x,g) => (g,true, "1","1",x._1,x._3,x._4++".s.d")})    
  ) map {
    // g = bit pattern, op2 = two operand, c = float double convert, d = double precision 
      case (g,op2,c,dp,fn,lat,name) => 
      if (op2) {
        println(name + " rd rs1 rs2 31..27="+pi(g)+" 26..25=" + pi(c+dp) +" rm 6..0=11")     
        (g + c + dp + "?"*18 + "0001011",op2,c,dp,fn,lat,name)
      } else {
        println(name + " rd rs1 31..27="+g(0)+" 26..25="+ pi(c+dp) +" 24..20="+pi(g.substring(1,6))+" rm 6..0=11")
        ("0000" + g(0) + c + dp + g.substring(1,6) + "?"*13 + "0001011",op2,c,dp,fn,lat,name)
      }
    }

  val scmTable :List[(BitPat,List[BitPat])] = (fun map {case (bp,op2,c,dp,fn,lat,name) => bp.B -> 
    List(N,Y,Y,if (op2) Y else N, N,   N,      N,        S,       S,           N,      N,     N,      N,  N,  N,    Y,   N, fn.B, lat.B)})
  // ldst,wen,ren1, ren2,       ren3,swap12, swap23, typeTagIn, typeTagOut, fromint, toint, fastpipe,fma,div,sqrt,wflags,vec
/**
  fun.foreach{case (bp,op2,c,dp,fn,lat,name) => println(bp + " " + fn.toHexString + " " + lat + " " + name)}

  println("\n# Append below to before {0,0,0,0,0} in rocket-chip/riscv-tools/riscv-gnu-toolchain/riscv-binutils-gdb/opcodes/riscv-opc.c\n") 
  //{"fsqrt.d",   "D",   "D,S",  MATCH_FSQRT_D | MASK_RM, MASK_FSQRT_D | MASK_RM, match_opcode, 0 },
  fun.foreach{case (bp,op2,c,dp,fn,lat,name) =>
    val f = if (dp=="0") "F" else "D"
    val op = if (op2) "D,S,T" else "D,S"
    val uname = name.toUpperCase.replace('.','_')
    println(("{=" +name+ "=, =" +f+ "=, =" +op+ "=, MATCH_" +uname+ " | MASK_RM, MASK_" +uname+ " | MASK_RM, match_opcode, 0},").replace('=','"'))
    println(("{=" +name+ "=, =" +f+ "=, =" +op+ ",m=, MATCH_" +uname+ ", MASK_" +uname+ ", match_opcode, 0},").replace('=','"'))
  }

  // println("\n# Insert below into rocket-chip/riscv-tools/riscv-gnu-toolchain/riscv-binutils-gdb/include/opcode/riscv-opc.h\n")
  // fun.foreach {case (bp,op2,c,dp,fn,lat,name) =>
  //   val uname = name.toUpperCase.replace('.','_')
  //   val s = bp.drop(1).replace('?','0') //drop leading 'b'
  //   val mat = " 0x"+pi(s.take(4)).toHexString+pi(s.drop(4)).toHexString // split string as it can't handle that many
  //   val mask = if (op2) " 0xfe00007f" else " 0xfff0007f" 
  //   println("#define MATCH_"+uname+mat)
  //   println("#define MASK_"+uname+mask)
  // }
  // fun.foreach {case (bp,op2,c,dp,fn,lat,name) =>
  //   val uname = name.toUpperCase.replace('.','_')
  //   val s = bp.drop(1).replace('?','0') //drop leading 'b'
  //   val mat = " 0x"+pi(s.take(4)).toHexString+pi(s.drop(4)).toHexString // split string as it can't handle that many
  //   val mask = if (op2) " 0xfe00007f" else " 0xfff0007f" 
  //   // DECLARE_INSN(c_sd, MATCH_C_SD, MASK_C_SD)
  //   println("DECLARE_INSN(" + name.replace('.','_') + ", MATCH_" +uname+ ", MASK_" +uname+ ")")
  // }

  println("\n# Insert below into rocket-chip/riscv-tools/riscv-isa-sim/spike_main/spike-dasm.cc\n")
  fun.foreach {case (bp,op2,c,dp,fn,lat,name) => 
    println("DEFINE_FR"+ (if (!op2) "1" else "") + "TYPE(" + name.replace('.','_') + ");")}
**/
}

case class FPUParams(
  minFLen: Int = 32,
  fLen: Int = 64,
  divSqrt: Boolean = true,
  sfmaLatency: Int = 3,
  dfmaLatency: Int = 4,
  fpmuLatency: Int = 2,
  ifpuLatency: Int = 2
)

object FPConstants
{
  val RM_SZ = 3
  val FLAGS_SZ = 5
}

trait HasFPUCtrlSigs {
  val ldst = Bool()
  val wen = Bool()
  val ren1 = Bool()
  val ren2 = Bool()
  val ren3 = Bool()
  val swap12 = Bool()
  val swap23 = Bool()
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromint = Bool()
  val toint = Bool()
  val fastpipe = Bool()
  val fma = Bool()
  val div = Bool()
  val sqrt = Bool()
  val wflags = Bool()
  val vec = Bool()
  val fn = UInt(width = FN_SZ) /// add two fields
  val lat = UInt(width = LAT_SZ)
}
}

class FPUCtrlSigs extends Bundle with HasFPUCtrlSigs

class FPUDecoder(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val inst = Input(Bits(32.W))
    val sigs = Output(new FPUCtrlSigs())
  })

  private val X2 = BitPat.dontCare(2)

  val default =       List(X,X,X,X,X,X,X,X2,X2,X,X,X,X,X,X,X,N)
  /** delete 
  val h: Array[(BitPat, List[BitPat])] =
    Array(FLH      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N),
          FSH      -> List(Y,N,N,Y,N,Y,X, I, H,N,Y,N,N,N,N,N,N),
          FMV_H_X  -> List(N,Y,N,N,N,X,X, H, I,Y,N,N,N,N,N,N,N),
          FCVT_H_W -> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_WU-> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_L -> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FCVT_H_LU-> List(N,Y,N,N,N,X,X, H, H,Y,N,N,N,N,N,Y,N),
          FMV_X_H  -> List(N,N,Y,N,N,N,X, I, H,N,Y,N,N,N,N,N,N),
          FCLASS_H -> List(N,N,Y,N,N,N,X, H, H,N,Y,N,N,N,N,N,N),
          FCVT_W_H -> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_WU_H-> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_L_H -> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_LU_H-> List(N,N,Y,N,N,N,X, H,X2,N,Y,N,N,N,N,Y,N),
          FCVT_S_H -> List(N,Y,Y,N,N,N,X, H, S,N,N,Y,N,N,N,Y,N),
          FCVT_H_S -> List(N,Y,Y,N,N,N,X, S, H,N,N,Y,N,N,N,Y,N),
          FEQ_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FLT_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FLE_H    -> List(N,N,Y,Y,N,N,N, H, H,N,Y,N,N,N,N,Y,N),
          FSGNJ_H  -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FSGNJN_H -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FSGNJX_H -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,N,N),
          FMIN_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,Y,N),
          FMAX_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,Y,N,N,N,Y,N),
          FADD_H   -> List(N,Y,Y,Y,N,N,Y, H, H,N,N,N,Y,N,N,Y,N),
          FSUB_H   -> List(N,Y,Y,Y,N,N,Y, H, H,N,N,N,Y,N,N,Y,N),
          FMUL_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FMADD_H  -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FMSUB_H  -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FNMADD_H -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FNMSUB_H -> List(N,Y,Y,Y,Y,N,N, H, H,N,N,N,Y,N,N,Y,N),
          FDIV_H   -> List(N,Y,Y,Y,N,N,N, H, H,N,N,N,N,Y,N,Y,N),
          FSQRT_H  -> List(N,Y,Y,N,N,N,X, H, H,N,N,N,N,N,Y,Y,N)) **/
  val f: Array[(BitPat, List[BitPat])] =
    Array(FLW      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N, 0x000000000L, 3),
          FSW      -> List(Y,N,N,Y,N,Y,X, I, S,N,Y,N,N,N,N,N,N, 0x000000000L, 2),
          FMV_W_X  -> List(N,Y,N,N,N,X,X, S, I,Y,N,N,N,N,N,N,N, 0x000000000L, 2),
          FCVT_S_W -> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_S_WU-> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_S_L -> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_S_LU-> List(N,Y,N,N,N,X,X, S, S,Y,N,N,N,N,N,Y,N, 0x000000000L, 2),
          FMV_X_W  -> List(N,N,Y,N,N,N,X, I, S,N,Y,N,N,N,N,N,N, 0x000000000L, 2),
          FCLASS_S -> List(N,N,Y,N,N,N,X, S, S,N,Y,N,N,N,N,N,N, 0x000000000L, 2),
          FCVT_W_S -> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_WU_S-> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_L_S -> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FCVT_LU_S-> List(N,N,Y,N,N,N,X, S,X2,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FEQ_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FLT_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FLE_S    -> List(N,N,Y,Y,N,N,N, S, S,N,Y,N,N,N,N,Y,N, 0x000000000L, 2),
          FSGNJ_S  -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N, 0x000000000L, 2),
          FSGNJN_S -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N, 0x000000000L, 2),
          FSGNJX_S -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,N,N, 0x000000000L, 2),
          FMIN_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,Y,N, 0x000000000L, 2),
          FMAX_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,Y,N,N,N,Y,N, 0x000000000L, 2),

          FADD_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x100000010L, 2),
          FSUB_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x140000010L, 2),
          FMUL_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x1e0000020L, 2),
          FMADD_S  -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x100000020L, 3),
          FMSUB_S  -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x180000020L, 3),
          FNMADD_S -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x140000020L, 3),
          FNMSUB_S -> List(N,Y,Y,Y,Y,N,N, S, S,N,N,N,Y,N,N,Y,N, 0x1c0000020L, 3),
          FDIV_S   -> List(N,Y,Y,Y,N,N,N, S, S,N,N,N,N,Y,N,Y,N, 0x040020000L, 8),
          FSQRT_S  -> List(N,Y,Y,N,N,N,X, S, S,N,N,N,N,N,Y,Y,N, 0x000000080L, 4))
  /** delete
  val d: Array[(BitPat, List[BitPat])] =
    Array(FLD      -> List(Y,Y,N,N,N,X,X,X2,X2,N,N,N,N,N,N,N,N),
          FSD      -> List(Y,N,N,Y,N,Y,X, I, D,N,Y,N,N,N,N,N,N),
          FMV_D_X  -> List(N,Y,N,N,N,X,X, D, I,Y,N,N,N,N,N,N,N),
          FCVT_D_W -> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_WU-> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_L -> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FCVT_D_LU-> List(N,Y,N,N,N,X,X, D, D,Y,N,N,N,N,N,Y,N),
          FMV_X_D  -> List(N,N,Y,N,N,N,X, I, D,N,Y,N,N,N,N,N,N),
          FCLASS_D -> List(N,N,Y,N,N,N,X, D, D,N,Y,N,N,N,N,N,N),
          FCVT_W_D -> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_WU_D-> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_L_D -> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_LU_D-> List(N,N,Y,N,N,N,X, D,X2,N,Y,N,N,N,N,Y,N),
          FCVT_S_D -> List(N,Y,Y,N,N,N,X, D, S,N,N,Y,N,N,N,Y,N),
          FCVT_D_S -> List(N,Y,Y,N,N,N,X, S, D,N,N,Y,N,N,N,Y,N),
          FEQ_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FLT_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FLE_D    -> List(N,N,Y,Y,N,N,N, D, D,N,Y,N,N,N,N,Y,N),
          FSGNJ_D  -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FSGNJN_D -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FSGNJX_D -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,N,N),
          FMIN_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,Y,N),
          FMAX_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,Y,N,N,N,Y,N),
          FADD_D   -> List(N,Y,Y,Y,N,N,Y, D, D,N,N,N,Y,N,N,Y,N),
          FSUB_D   -> List(N,Y,Y,Y,N,N,Y, D, D,N,N,N,Y,N,N,Y,N),
          FMUL_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FMADD_D  -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FMSUB_D  -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FNMADD_D -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FNMSUB_D -> List(N,Y,Y,Y,Y,N,N, D, D,N,N,N,Y,N,N,Y,N),
          FDIV_D   -> List(N,Y,Y,Y,N,N,N, D, D,N,N,N,N,Y,N,Y,N),
          FSQRT_D  -> List(N,Y,Y,N,N,N,X, D, D,N,N,N,N,N,Y,Y,N))
  val fcvt_hd: Array[(BitPat, List[BitPat])] =
    Array(FCVT_H_D -> List(N,Y,Y,N,N,N,X, D, H,N,N,Y,N,N,N,Y,N),
          FCVT_D_H -> List(N,Y,Y,N,N,N,X, H, D,N,N,Y,N,N,N,Y,N))
  val vfmv_f_s: Array[(BitPat, List[BitPat])] =
    Array(VFMV_F_S -> List(N,Y,N,N,N,N,X,X2,X2,N,N,N,N,N,N,N,Y)) **/

  val insns = ((minFLen, fLen) match {
    case (32, 32) => f ++ scmTable /// add scm
/** delete
    case (16, 32) => h ++ f
    case (32, 64) => f ++ d
    case (16, 64) => h ++ f ++ d ++ fcvt_hd
**/
    case other => throw new Exception(s"minFLen = ${minFLen} & fLen = ${fLen} is an unsupported configuration")
  }) ++ (if (usingVector) vfmv_f_s else Array[(BitPat, List[BitPat])]())
  val decoder = DecodeLogic(io.inst, default, insns)
  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.typeTagIn, s.typeTagOut, s.fromint, s.toint,
                 s.fastpipe, s.fma, s.div, s.sqrt, s.wflags, s.vec)
  sigs zip decoder map {case(s,d) => s := d}
}

class FPUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val hartid = Input(UInt(hartIdLen.W))
  val time = Input(UInt(xLen.W))

  val inst = Input(Bits(32.W))
  val fromint_data = Input(Bits(xLen.W))

  val fcsr_rm = Input(Bits(FPConstants.RM_SZ.W))
  val fcsr_flags = Valid(Bits(FPConstants.FLAGS_SZ.W))

  val v_sew = Input(UInt(3.W))

  val store_data = Output(Bits(fLen.W))
  val toint_data = Output(Bits(xLen.W))

  val ll_resp_val = Input(Bool())
  val ll_resp_type = Input(Bits(3.W))
  val ll_resp_tag = Input(UInt(5.W))
  val ll_resp_data = Input(Bits(fLen.W))

  val valid = Input(Bool())
  val fcsr_rdy = Output(Bool())
  val nack_mem = Output(Bool())
  val illegal_rm = Output(Bool())
  val killx = Input(Bool())
  val killm = Input(Bool())
  val dec = Output(new FPUCtrlSigs())
  val sboard_set = Output(Bool())
  val sboard_clr = Output(Bool())
  val sboard_clra = Output(UInt(5.W))

  val keep_clock_enabled = Input(Bool())
}

class FPUIO(implicit p: Parameters) extends FPUCoreIO ()(p) {
  val cp_req = Flipped(Decoupled(new FPInput())) //cp doesn't pay attn to kill sigs
  val cp_resp = Decoupled(new FPResult())
}

class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits((fLen+1).W)
  val exc = Bits(FPConstants.FLAGS_SZ.W)
}

class IntToFPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(FPConstants.RM_SZ.W)
  val typ = Bits(2.W)
  val in1 = Bits(xLen.W)
}

class FPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(FPConstants.RM_SZ.W)
  val fmaCmd = Bits(2.W)
  val typ = Bits(2.W)
  val fmt = Bits(2.W)
  val in1 = Bits((fLen+1).W)
  val in2 = Bits((fLen+1).W)
  val in3 = Bits((fLen+1).W)

}

case class FType(exp: Int, sig: Int) {
  def ieeeWidth = exp + sig
  def recodedWidth = ieeeWidth + 1

  def ieeeQNaN = ((BigInt(1) << (ieeeWidth - 1)) - (BigInt(1) << (sig - 2))).U(ieeeWidth.W)
  def qNaN = ((BigInt(7) << (exp + sig - 3)) + (BigInt(1) << (sig - 2))).U(recodedWidth.W)
  def isNaN(x: UInt) = x(sig + exp - 1, sig + exp - 3).andR
  def isSNaN(x: UInt) = isNaN(x) && !x(sig - 2)

  def classify(x: UInt) = {
    val sign = x(sig + exp)
    val code = x(exp + sig - 1, exp + sig - 3)
    val codeHi = code(2, 1)
    val isSpecial = codeHi === 3.U

    val isHighSubnormalIn = x(exp + sig - 3, sig - 1) < 2.U
    val isSubnormal = code === 1.U || codeHi === 1.U && isHighSubnormalIn
    val isNormal = codeHi === 1.U && !isHighSubnormalIn || codeHi === 2.U
    val isZero = code === 0.U
    val isInf = isSpecial && !code(0)
    val isNaN = code.andR
    val isSNaN = isNaN && !x(sig-2)
    val isQNaN = isNaN && x(sig-2)

    Cat(isQNaN, isSNaN, isInf && !sign, isNormal && !sign,
        isSubnormal && !sign, isZero && !sign, isZero && sign,
        isSubnormal && sign, isNormal && sign, isInf && sign)
  }

  // convert between formats, ignoring rounding, range, NaN
  def unsafeConvert(x: UInt, to: FType) = if (this == to) x else {
    val sign = x(sig + exp)
    val fractIn = x(sig - 2, 0)
    val expIn = x(sig + exp - 1, sig - 1)
    val fractOut = fractIn << to.sig >> sig
    val expOut = {
      val expCode = expIn(exp, exp - 2)
      val commonCase = (expIn + (1 << to.exp).U) - (1 << exp).U
      Mux(expCode === 0.U || expCode >= 6.U, Cat(expCode, commonCase(to.exp - 3, 0)), commonCase(to.exp, 0))
    }
    Cat(sign, expOut, fractOut)
  }

  private def ieeeBundle = {
    val expWidth = exp
    class IEEEBundle extends Bundle {
      val sign = Bool()
      val exp = UInt(expWidth.W)
      val sig = UInt((ieeeWidth-expWidth-1).W)
    }
    new IEEEBundle
  }

  def unpackIEEE(x: UInt) = x.asTypeOf(ieeeBundle)

  def recode(x: UInt) = hardfloat.recFNFromFN(exp, sig, x)
  def ieee(x: UInt) = hardfloat.fNFromRecFN(exp, sig, x)
}

object FType {
  val H = new FType(5, 11)
  val S = new FType(8, 24)
  val D = new FType(11, 53)

  val all = List(H, S, D)
}

trait HasFPUParameters {
  require(fLen == 0 || FType.all.exists(_.ieeeWidth == fLen))
  val minFLen: Int
  val fLen: Int
  def xLen: Int
  val minXLen = 32
  val nIntTypes = log2Ceil(xLen/minXLen) + 1
  def floatTypes = FType.all.filter(t => minFLen <= t.ieeeWidth && t.ieeeWidth <= fLen)
  def minType = floatTypes.head
  def maxType = floatTypes.last
  def prevType(t: FType) = floatTypes(typeTag(t) - 1)
  def maxExpWidth = maxType.exp
  def maxSigWidth = maxType.sig
  def typeTag(t: FType) = floatTypes.indexOf(t)
  def typeTagWbOffset = (FType.all.indexOf(minType) + 1).U
  def typeTagGroup(t: FType) = (if (floatTypes.contains(t)) typeTag(t) else typeTag(maxType)).U
  // typeTag
  def H = typeTagGroup(FType.H)
  def S = typeTagGroup(FType.S)
  def D = typeTagGroup(FType.D)
  def I = typeTag(maxType).U

  private def isBox(x: UInt, t: FType): Bool = x(t.sig + t.exp, t.sig + t.exp - 4).andR

  private def box(x: UInt, xt: FType, y: UInt, yt: FType): UInt = {
    require(xt.ieeeWidth == 2 * yt.ieeeWidth)
    val swizzledNaN = Cat(
      x(xt.sig + xt.exp, xt.sig + xt.exp - 3),
      x(xt.sig - 2, yt.recodedWidth - 1).andR,
      x(xt.sig + xt.exp - 5, xt.sig),
      y(yt.recodedWidth - 2),
      x(xt.sig - 2, yt.recodedWidth - 1),
      y(yt.recodedWidth - 1),
      y(yt.recodedWidth - 3, 0))
    Mux(xt.isNaN(x), swizzledNaN, x)
  }

  // implement NaN unboxing for FU inputs
  def unbox(x: UInt, tag: UInt, exactType: Option[FType]): UInt = {
    val outType = exactType.getOrElse(maxType)
    def helper(x: UInt, t: FType): Seq[(Bool, UInt)] = {
      val prev =
        if (t == minType) {
          Seq()
        } else {
          val prevT = prevType(t)
          val unswizzled = Cat(
            x(prevT.sig + prevT.exp - 1),
            x(t.sig - 1),
            x(prevT.sig + prevT.exp - 2, 0))
          val prev = helper(unswizzled, prevT)
          val isbox = isBox(x, t)
          prev.map(p => (isbox && p._1, p._2))
        }
      prev :+ (true.B, t.unsafeConvert(x, outType))
    }

    val (oks, floats) = helper(x, maxType).unzip
    if (exactType.isEmpty || floatTypes.size == 1) {
      Mux(oks(tag), floats(tag), maxType.qNaN)
    } else {
      val t = exactType.get
      floats(typeTag(t)) | Mux(oks(typeTag(t)), 0.U, t.qNaN)
    }
  }

  // make sure that the redundant bits in the NaN-boxed encoding are consistent
  def consistent(x: UInt): Bool = {
    def helper(x: UInt, t: FType): Bool = if (typeTag(t) == 0) true.B else {
      val prevT = prevType(t)
      val unswizzled = Cat(
        x(prevT.sig + prevT.exp - 1),
        x(t.sig - 1),
        x(prevT.sig + prevT.exp - 2, 0))
      val prevOK = !isBox(x, t) || helper(unswizzled, prevT)
      val curOK = !t.isNaN(x) || x(t.sig + t.exp - 4) === x(t.sig - 2, prevT.recodedWidth - 1).andR
      prevOK && curOK
    }
    helper(x, maxType)
  }

  // generate a NaN box from an FU result
  def box(x: UInt, t: FType): UInt = {
    if (t == maxType) {
      x
    } else {
      val nt = floatTypes(typeTag(t) + 1)
      val bigger = box(((BigInt(1) << nt.recodedWidth)-1).U, nt, x, t)
      bigger | ((BigInt(1) << maxType.recodedWidth) - (BigInt(1) << nt.recodedWidth)).U
    }
  }

  // generate a NaN box from an FU result
  def box(x: UInt, tag: UInt): UInt = {
    val opts = floatTypes.map(t => box(x, t))
    opts(tag)
  }

  // zap bits that hardfloat thinks are don't-cares, but we do care about
  def sanitizeNaN(x: UInt, t: FType): UInt = {
    if (typeTag(t) == 0) {
      x
    } else {
      val maskedNaN = x & ~((BigInt(1) << (t.sig-1)) | (BigInt(1) << (t.sig+t.exp-4))).U(t.recodedWidth.W)
      Mux(t.isNaN(x), maskedNaN, x)
    }
  }

  // implement NaN boxing and recoding for FL*/fmv.*.x
  def recode(x: UInt, tag: UInt): UInt = {
    def helper(x: UInt, t: FType): UInt = {
      if (typeTag(t) == 0) {
        t.recode(x)
      } else {
        val prevT = prevType(t)
        box(t.recode(x), t, helper(x, prevT), prevT)
      }
    }

    // fill MSBs of subword loads to emulate a wider load of a NaN-boxed value
    val boxes = floatTypes.map(t => ((BigInt(1) << maxType.ieeeWidth) - (BigInt(1) << t.ieeeWidth)).U)
    helper(boxes(tag) | x, maxType)
  }

  // implement NaN unboxing and un-recoding for FS*/fmv.x.*
  def ieee(x: UInt, t: FType = maxType): UInt = {
    if (typeTag(t) == 0) {
      t.ieee(x)
    } else {
      val unrecoded = t.ieee(x)
      val prevT = prevType(t)
      val prevRecoded = Cat(
        x(prevT.recodedWidth-2),
        x(t.sig-1),
        x(prevT.recodedWidth-3, 0))
      val prevUnrecoded = ieee(prevRecoded, prevT)
      Cat(unrecoded >> prevT.ieeeWidth, Mux(t.isNaN(x), prevUnrecoded, unrecoded(prevT.ieeeWidth-1, 0)))
    }
  }
}

abstract class FPUModule(implicit val p: Parameters) extends Module with HasCoreParameters with HasFPUParameters

class FPToInt(implicit p: Parameters) extends FPUModule()(p) {
  class Output extends Bundle {
    val in = new FPInput
    val lt = Bool()
    val store = Bits(fLen.W)
    val toint = Bits(xLen.W)
    val exc = Bits(FPConstants.FLAGS_SZ.W)
  }
  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new Output)
  })

  val in = RegEnable(io.in.bits, io.in.valid)
  val valid = RegNext(io.in.valid)

  val dcmp = Module(new hardfloat.CompareRecFN(maxExpWidth, maxSigWidth))
  dcmp.io.a := in.in1
  dcmp.io.b := in.in2
  dcmp.io.signaling := !in.rm(1)

  val tag = in.typeTagOut
  val toint_ieee = (floatTypes.map(t => if (t == FType.H) Fill(maxType.ieeeWidth / minXLen,   ieee(in.in1)(15, 0).sextTo(minXLen))
                                        else              Fill(maxType.ieeeWidth / t.ieeeWidth, ieee(in.in1)(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)

  val toint = WireDefault(toint_ieee)
  val intType = WireDefault(in.fmt(0))
  io.out.bits.store := (floatTypes.map(t => Fill(fLen / t.ieeeWidth, ieee(in.in1)(t.ieeeWidth - 1, 0))): Seq[UInt])(tag)
  io.out.bits.toint := ((0 until nIntTypes).map(i => toint((minXLen << i) - 1, 0).sextTo(xLen)): Seq[UInt])(intType)
  io.out.bits.exc := 0.U

  when (in.rm(0)) {
    val classify_out = (floatTypes.map(t => t.classify(maxType.unsafeConvert(in.in1, t))): Seq[UInt])(tag)
    toint := classify_out | (toint_ieee >> minXLen << minXLen)
    intType := false.B
  }

  when (in.wflags) { // feq/flt/fle, fcvt
    toint := (~in.rm & Cat(dcmp.io.lt, dcmp.io.eq)).orR | (toint_ieee >> minXLen << minXLen)
    io.out.bits.exc := dcmp.io.exceptionFlags
    intType := false.B

    when (!in.ren2) { // fcvt
      val cvtType = in.typ.extract(log2Ceil(nIntTypes), 1)
      intType := cvtType
      val conv = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, xLen))
      conv.io.in := in.in1
      conv.io.roundingMode := in.rm
      conv.io.signedOut := ~in.typ(0)
      toint := conv.io.out
      io.out.bits.exc := Cat(conv.io.intExceptionFlags(2, 1).orR, 0.U(3.W), conv.io.intExceptionFlags(0))

      for (i <- 0 until nIntTypes-1) {
        val w = minXLen << i
        when (cvtType === i.U) {
          val narrow = Module(new hardfloat.RecFNToIN(maxExpWidth, maxSigWidth, w))
          narrow.io.in := in.in1
          narrow.io.roundingMode := in.rm
          narrow.io.signedOut := ~in.typ(0)

          val excSign = in.in1(maxExpWidth + maxSigWidth) && !maxType.isNaN(in.in1)
          val excOut = Cat(conv.io.signedOut === excSign, Fill(w-1, !excSign))
          val invalid = conv.io.intExceptionFlags(2) || narrow.io.intExceptionFlags(1)
          when (invalid) { toint := Cat(conv.io.out >> w, excOut) }
          io.out.bits.exc := Cat(invalid, 0.U(3.W), !invalid && conv.io.intExceptionFlags(0))
        }
      }
    }
  }

  io.out.valid := valid
  io.out.bits.lt := dcmp.io.lt || (dcmp.io.a.asSInt < 0.S && dcmp.io.b.asSInt >= 0.S)
  io.out.bits.in := in
}

class IntToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new IntToFPInput))
    val out = Valid(new FPResult)
  })

  val in = Pipe(io.in)
  val tag = in.bits.typeTagIn

  val mux = Wire(new FPResult)
  mux.exc := 0.U
  mux.data := recode(in.bits.in1, tag)

  val intValue = {
    val res = WireDefault(in.bits.in1.asSInt)
    for (i <- 0 until nIntTypes-1) {
      val smallInt = in.bits.in1((minXLen << i) - 1, 0)
      when (in.bits.typ.extract(log2Ceil(nIntTypes), 1) === i.U) {
        res := Mux(in.bits.typ(0), smallInt.zext, smallInt.asSInt)
      }
    }
    res.asUInt
  }

  when (in.bits.wflags) { // fcvt
    // could be improved for RVD/RVQ with a single variable-position rounding
    // unit, rather than N fixed-position ones
    val i2fResults = for (t <- floatTypes) yield {
      val i2f = Module(new hardfloat.INToRecFN(xLen, t.exp, t.sig))
      i2f.io.signedIn := ~in.bits.typ(0)
      i2f.io.in := intValue
      i2f.io.roundingMode := in.bits.rm
      i2f.io.detectTininess := hardfloat.consts.tininess_afterRounding
      (sanitizeNaN(i2f.io.out, t), i2f.io.exceptionFlags)
    }

    val (data, exc) = i2fResults.unzip
    val dataPadded = data.init.map(d => Cat(data.last >> d.getWidth, d)) :+ data.last
    mux.data := dataPadded(tag)
    mux.exc := exc(tag)
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class FPToFP(val latency: Int)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new FPResult)
    val lt = Input(Bool()) // from FPToInt
  })

  val in = Pipe(io.in)

  val signNum = Mux(in.bits.rm(1), in.bits.in1 ^ in.bits.in2, Mux(in.bits.rm(0), ~in.bits.in2, in.bits.in2))
  val fsgnj = Cat(signNum(fLen), in.bits.in1(fLen-1, 0))

  val fsgnjMux = Wire(new FPResult)
  fsgnjMux.exc := 0.U
  fsgnjMux.data := fsgnj

  when (in.bits.wflags) { // fmin/fmax
    val isnan1 = maxType.isNaN(in.bits.in1)
    val isnan2 = maxType.isNaN(in.bits.in2)
    val isInvalid = maxType.isSNaN(in.bits.in1) || maxType.isSNaN(in.bits.in2)
    val isNaNOut = isnan1 && isnan2
    val isLHS = isnan2 || in.bits.rm(0) =/= io.lt && !isnan1
    fsgnjMux.exc := isInvalid << 4
    fsgnjMux.data := Mux(isNaNOut, maxType.qNaN, Mux(isLHS, in.bits.in1, in.bits.in2))
  }

  val inTag = in.bits.typeTagIn
  val outTag = in.bits.typeTagOut
  val mux = WireDefault(fsgnjMux)
  for (t <- floatTypes.init) {
    when (outTag === typeTag(t).U) {
      mux.data := Cat(fsgnjMux.data >> t.recodedWidth, maxType.unsafeConvert(fsgnjMux.data, t))
    }
  }

  when (in.bits.wflags && !in.bits.ren2) { // fcvt
    if (floatTypes.size > 1) {
      // widening conversions simply canonicalize NaN operands
      val widened = Mux(maxType.isNaN(in.bits.in1), maxType.qNaN, in.bits.in1)
      fsgnjMux.data := widened
      fsgnjMux.exc := maxType.isSNaN(in.bits.in1) << 4

      // narrowing conversions require rounding (for RVQ, this could be
      // optimized to use a single variable-position rounding unit, rather
      // than two fixed-position ones)
      for (outType <- floatTypes.init) when (outTag === typeTag(outType).U && ((typeTag(outType) == 0).B || outTag < inTag)) {
        val narrower = Module(new hardfloat.RecFNToRecFN(maxType.exp, maxType.sig, outType.exp, outType.sig))
        narrower.io.in := in.bits.in1
        narrower.io.roundingMode := in.bits.rm
        narrower.io.detectTininess := hardfloat.consts.tininess_afterRounding
        val narrowed = sanitizeNaN(narrower.io.out, outType)
        mux.data := Cat(fsgnjMux.data >> narrowed.getWidth, narrowed)
        mux.exc := narrower.io.exceptionFlags
      }
    }
  }

  io.out <> Pipe(in.valid, mux, latency-1)
}

class MulAddRecFNPipe(latency: Int, expWidth: Int, sigWidth: Int) extends Module
{
    override def desiredName = s"MulAddRecFNPipe_l${latency}_e${expWidth}_s${sigWidth}"
    require(latency<=2)

    val io = IO(new Bundle {
        val validin = Input(Bool())
        val op = Input(Bits(2.W))
        val a = Input(Bits((expWidth + sigWidth + 1).W))
        val b = Input(Bits((expWidth + sigWidth + 1).W))
        val c = Input(Bits((expWidth + sigWidth + 1).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output(Bits((expWidth + sigWidth + 1).W))
        val exceptionFlags = Output(Bits(5.W))
        val validout = Output(Bool())
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val mulAddRecFNToRaw_preMul = Module(new hardfloat.MulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul = Module(new hardfloat.MulAddRecFNToRaw_postMul(expWidth, sigWidth))

    mulAddRecFNToRaw_preMul.io.op := io.op
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.b  := io.b
    mulAddRecFNToRaw_preMul.io.c  := io.c

    val mulAddResult =
        (mulAddRecFNToRaw_preMul.io.mulAddA *
             mulAddRecFNToRaw_preMul.io.mulAddB) +&
            mulAddRecFNToRaw_preMul.io.mulAddC

    val valid_stage0 = Wire(Bool())
    val roundingMode_stage0 = Wire(UInt(3.W))
    val detectTininess_stage0 = Wire(UInt(1.W))

    val postmul_regs = if(latency>0) 1 else 0
    mulAddRecFNToRaw_postMul.io.fromPreMul   := Pipe(io.validin, mulAddRecFNToRaw_preMul.io.toPostMul, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.mulAddResult := Pipe(io.validin, mulAddResult, postmul_regs).bits
    mulAddRecFNToRaw_postMul.io.roundingMode := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    roundingMode_stage0                      := Pipe(io.validin, io.roundingMode, postmul_regs).bits
    detectTininess_stage0                    := Pipe(io.validin, io.detectTininess, postmul_regs).bits
    valid_stage0                             := Pipe(io.validin, false.B, postmul_regs).valid

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val roundRawFNToRecFN = Module(new hardfloat.RoundRawFNToRecFN(expWidth, sigWidth, 0))

    val round_regs = if(latency==2) 1 else 0
    roundRawFNToRecFN.io.invalidExc         := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.invalidExc, round_regs).bits
    roundRawFNToRecFN.io.in                 := Pipe(valid_stage0, mulAddRecFNToRaw_postMul.io.rawOut, round_regs).bits
    roundRawFNToRecFN.io.roundingMode       := Pipe(valid_stage0, roundingMode_stage0, round_regs).bits
    roundRawFNToRecFN.io.detectTininess     := Pipe(valid_stage0, detectTininess_stage0, round_regs).bits
    io.validout                             := Pipe(valid_stage0, false.B, round_regs).valid

    roundRawFNToRecFN.io.infiniteExc := false.B

    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}

class FPUFMAPipe(val latency: Int, val t: FType)
                (implicit p: Parameters) extends FPUModule()(p) {
  override def desiredName = s"FPUFMAPipe_l${latency}_f${t.ieeeWidth}"
  require(latency>0)

  val io = IO(new Bundle {
    val in = Flipped(Valid(new FPInput))
    val out = Valid(new FPResult)
  })

  val valid = RegNext(io.in.valid)
  val in = Reg(new FPInput)
  when (io.in.valid) {
    val one = 1.U << (t.sig + t.exp - 1)
    val zero = (io.in.bits.in1 ^ io.in.bits.in2) & (1.U << (t.sig + t.exp))
    val cmd_fma = io.in.bits.ren3
    val cmd_addsub = io.in.bits.swap23
    in := io.in.bits
    when (cmd_addsub) { in.in2 := one }
    when (!(cmd_fma || cmd_addsub)) { in.in3 := zero }
  }

  val fma = Module(new MulAddRecFNPipe((latency-1) min 2, t.exp, t.sig))
  fma.io.validin := valid
  fma.io.op := in.fmaCmd
  fma.io.roundingMode := in.rm
  fma.io.detectTininess := hardfloat.consts.tininess_afterRounding
  fma.io.a := in.in1
  fma.io.b := in.in2
  fma.io.c := in.in3

  val res = Wire(new FPResult)
  res.data := sanitizeNaN(fma.io.out, t)
  res.exc := fma.io.exceptionFlags

  io.out := Pipe(fma.io.validout, res, (latency-3) max 0)
}

class FPU(cfg: FPUParams)(implicit p: Parameters) extends FPUModule()(p) {
  val io = IO(new FPUIO)

  val (useClockGating, useDebugROB) = coreParams match {
    case r: RocketCoreParams =>
      val sz = if (r.debugROB.isDefined) r.debugROB.get.size else 1
      (r.clockGate, sz < 1)
    case _ => (false, false)
  }
  val clock_en_reg = Reg(Bool())
  val clock_en = clock_en_reg || io.cp_req.valid
  val gated_clock =
    if (!useClockGating) clock
    else ClockGate(clock, clock_en, "fpu_clock_gate")

  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.inst := io.inst
  val id_ctrl = WireInit(fp_decoder.io.sigs)
  coreParams match { case r: RocketCoreParams => r.vector.map(v => {
    val v_decode = v.decoder(p) // Only need to get ren1
    v_decode.io.inst := io.inst
    v_decode.io.vconfig := DontCare // core deals with this
    v_decode.io.vconfig.vtype.vsew := io.v_sew
    when (v_decode.io.legal && v_decode.io.read_frs1) {
      id_ctrl.ren1 := true.B
      id_ctrl.swap12 := false.B
      id_ctrl.toint := true.B
      id_ctrl.typeTagIn := I
      id_ctrl.typeTagOut := Mux(io.v_sew === 3.U, D, S)
    }
    when (v_decode.io.write_frd) { id_ctrl.wen := true.B }
  })}

  val ex_reg_valid = RegNext(io.valid, false.B)
  val ex_reg_inst = RegEnable(io.inst, io.valid)
  val ex_reg_ctrl = RegEnable(id_ctrl, io.valid)
  val ex_ra = List.fill(3)(Reg(UInt()))

  // load/vector response
  val load_wb = RegNext(io.ll_resp_val)
  val load_wb_typeTag = RegEnable(io.ll_resp_type(1,0) - typeTagWbOffset, io.ll_resp_val)
  val load_wb_data = RegEnable(io.ll_resp_data, io.ll_resp_val)
  val load_wb_tag = RegEnable(io.ll_resp_tag, io.ll_resp_val)

  class FPUImpl { // entering gated-clock domain

  val req_valid = ex_reg_valid || io.cp_req.valid
  val ex_cp_valid = io.cp_req.fire
  val mem_cp_valid = RegNext(ex_cp_valid, false.B)
  val wb_cp_valid = RegNext(mem_cp_valid, false.B)
  val mem_reg_valid = RegInit(false.B)
  val killm = (io.killm || io.nack_mem) && !mem_cp_valid
  // Kill X-stage instruction if M-stage is killed.  This prevents it from
  // speculatively being sent to the div-sqrt unit, which can cause priority
  // inversion for two back-to-back divides, the first of which is killed.
  val killx = io.killx || mem_reg_valid && killm
  mem_reg_valid := ex_reg_valid && !killx || ex_cp_valid
  val mem_reg_inst = RegEnable(ex_reg_inst, ex_reg_valid)
  val wb_reg_valid = RegNext(mem_reg_valid && (!killm || mem_cp_valid), false.B)

  val cp_ctrl = Wire(new FPUCtrlSigs)
  cp_ctrl :<>= io.cp_req.bits.viewAsSupertype(new FPUCtrlSigs)
  io.cp_resp.valid := false.B
  io.cp_resp.bits.data := 0.U
  io.cp_resp.bits.exc := DontCare

  val ex_ctrl = Mux(ex_cp_valid, cp_ctrl, ex_reg_ctrl)
  val mem_ctrl = RegEnable(ex_ctrl, req_valid)
  val wb_ctrl = RegEnable(mem_ctrl, mem_reg_valid)

  // CoreMonitorBundle to monitor fp register file writes
  val frfWriteBundle = Seq.fill(2)(WireInit(new CoreMonitorBundle(xLen, fLen), DontCare))
  frfWriteBundle.foreach { i =>
    i.clock := clock
    i.reset := reset
    i.hartid := io.hartid
    i.timer := io.time(31,0)
    i.valid := false.B
    i.wrenx := false.B
    i.wrenf := false.B
    i.excpt := false.B
  }

  // regfile
  val regfile = Mem(32, Bits((fLen+1).W))
  when (load_wb) {
    val wdata = recode(load_wb_data, load_wb_typeTag)
    regfile(load_wb_tag) := wdata
    assert(consistent(wdata))
    if (enableCommitLog)
      printf("f%d p%d 0x%x\n", load_wb_tag, load_wb_tag + 32.U, ieee(wdata))
    if (useDebugROB)
      DebugROB.pushWb(clock, reset, io.hartid, load_wb, load_wb_tag + 32.U, ieee(wdata))
    frfWriteBundle(0).wrdst := load_wb_tag
    frfWriteBundle(0).wrenf := true.B
    frfWriteBundle(0).wrdata := ieee(wdata)
  }

  val ex_rs = ex_ra.map(a => regfile(a))
  when (io.valid) {
    when (id_ctrl.ren1) {
      when (!id_ctrl.swap12) { ex_ra(0) := io.inst(19,15) }
      when (id_ctrl.swap12) { ex_ra(1) := io.inst(19,15) }
    }
    when (id_ctrl.ren2) {
      when (id_ctrl.swap12) { ex_ra(0) := io.inst(24,20) }
      when (id_ctrl.swap23) { ex_ra(2) := io.inst(24,20) }
      when (!id_ctrl.swap12 && !id_ctrl.swap23) { ex_ra(1) := io.inst(24,20) }
    }
    when (id_ctrl.ren3) { ex_ra(2) := io.inst(31,27) }
  }
  val ex_rm = Mux(ex_reg_inst(14,12) === 7.U, io.fcsr_rm, ex_reg_inst(14,12))

  def fuInput(minT: Option[FType]): FPInput = {
    val req = Wire(new FPInput)
    val tag = ex_ctrl.typeTagIn
    req.viewAsSupertype(new Bundle with HasFPUCtrlSigs) :#= ex_ctrl.viewAsSupertype(new Bundle with HasFPUCtrlSigs)
    req.rm := ex_rm
    req.in1 := unbox(ex_rs(0), tag, minT)
    req.in2 := unbox(ex_rs(1), tag, minT)
    req.in3 := unbox(ex_rs(2), tag, minT)
    req.typ := ex_reg_inst(21,20)
    req.fmt := ex_reg_inst(26,25)
    req.fmaCmd := ex_reg_inst(3,2) | (!ex_ctrl.ren3 && ex_reg_inst(27))
    when (ex_cp_valid) {
      req := io.cp_req.bits
      when (io.cp_req.bits.swap12) {
        req.in1 := io.cp_req.bits.in2
        req.in2 := io.cp_req.bits.in1
      }
      when (io.cp_req.bits.swap23) {
        req.in2 := io.cp_req.bits.in3
        req.in3 := io.cp_req.bits.in2
      }
    }
    req
  }

  val sfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.S))
  sfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === S
  sfma.io.in.bits := fuInput(Some(sfma.t))

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := req_valid && (ex_ctrl.toint || ex_ctrl.div || ex_ctrl.sqrt || (ex_ctrl.fastpipe && ex_ctrl.wflags))
  fpiu.io.in.bits := fuInput(None)
  io.store_data := fpiu.io.out.bits.store
  io.toint_data := fpiu.io.out.bits.toint
  when(fpiu.io.out.valid && mem_cp_valid && mem_ctrl.toint){
    io.cp_resp.bits.data := fpiu.io.out.bits.toint
    io.cp_resp.valid := true.B
  }

  val ifpu = Module(new IntToFP(cfg.ifpuLatency))
  ifpu.io.in.valid := req_valid && ex_ctrl.fromint
  ifpu.io.in.bits := fpiu.io.in.bits
  ifpu.io.in.bits.in1 := Mux(ex_cp_valid, io.cp_req.bits.in1, io.fromint_data)

  val fpmu = Module(new FPToFP(cfg.fpmuLatency))
  fpmu.io.in.valid := req_valid && ex_ctrl.fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt

  val divSqrt_wen = WireDefault(false.B)
  val divSqrt_inFlight = WireDefault(false.B)
  val divSqrt_waddr = Reg(UInt(5.W))
  val divSqrt_cp = Reg(Bool())
  val divSqrt_typeTag = Wire(UInt(log2Up(floatTypes.size).W))
  val divSqrt_wdata = Wire(UInt((fLen+1).W))
  val divSqrt_flags = Wire(UInt(FPConstants.FLAGS_SZ.W))
  divSqrt_typeTag := DontCare
  divSqrt_wdata := DontCare
  divSqrt_flags := DontCare

  //<<<<<<<
class SCM(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val to_scm = new FPInput //
      val fn = UInt(FN_SZ.W)
    })
    val out = Output(new FPResult) 
  })

  val scm = Module(new SCM)
  scm.io.in :<>= fuInput(None) 
  scm.io.in.fn := Mux(req_valid && ex_ctrl.scm, ex_ctrl.fn, UInt(0))

  
  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    /// add one line
    Pipe(scm_pipe, cfg.scmLatency, (c: FPUCtrlSigs) => c.scm, scm_pipe.io.res),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === S, sfma.io.out.bits)) ++
    (fLen > 32).option({
          val dfma = Module(new FPUFMAPipe(cfg.dfmaLatency, FType.D))
          dfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === D
          dfma.io.in.bits := fuInput(Some(dfma.t))
          Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === D, dfma.io.out.bits)
        }) ++
    (minFLen == 16).option({
          val hfma = Module(new FPUFMAPipe(cfg.sfmaLatency, FType.H))
          hfma.io.in.valid := req_valid && ex_ctrl.fma && ex_ctrl.typeTagOut === H
          hfma.io.in.bits := fuInput(Some(hfma.t))
          Pipe(hfma, hfma.latency, (c: FPUCtrlSigs) => c.fma && c.typeTagOut === H, hfma.io.out.bits)
        })
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), (1 << p.lat-offset).U, 0.U)).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), p._2.U, 0.U)).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val memLatencyMask = latencyMask(mem_ctrl, 2)

  class WBInfo extends Bundle {
    val rd = UInt(5.W)
    val typeTag = UInt(log2Up(floatTypes.size).W)
    val cp = Bool()
    val pipeid = UInt(log2Ceil(pipes.size).W)
  }

  val wen = RegInit(0.U((maxLatency-1).W))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val mem_wen = mem_reg_valid && (mem_ctrl.fma || mem_ctrl.fastpipe || mem_ctrl.fromint || mem_ctrl.scm) /// add scm
  val write_port_busy = RegEnable(mem_wen && (memLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, req_valid)
  ccover(mem_reg_valid && write_port_busy, "WB_STRUCTURAL", "structural hazard on writeback")

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> 1
  when (mem_wen) {
    when (!killm) {
      wen := wen >> 1 | memLatencyMask
    }
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && memLatencyMask(i)) {
        wbInfo(i).cp := mem_cp_valid
        wbInfo(i).typeTag := mem_ctrl.typeTagOut
        wbInfo(i).pipeid := pipeid(mem_ctrl)
        wbInfo(i).rd := mem_reg_inst(11,7)
      }
    }
  }

  val waddr = Mux(divSqrt_wen, divSqrt_waddr, wbInfo(0).rd)
  val wb_cp = Mux(divSqrt_wen, divSqrt_cp, wbInfo(0).cp)
  val wtypeTag = Mux(divSqrt_wen, divSqrt_typeTag, wbInfo(0).typeTag)
  val wdata = box(Mux(divSqrt_wen, divSqrt_wdata, (pipes.map(_.res.data): Seq[UInt])(wbInfo(0).pipeid)), wtypeTag)
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wbInfo(0).pipeid)
  when ((!wbInfo(0).cp && wen(0)) || divSqrt_wen) {
    assert(consistent(wdata))
    regfile(waddr) := wdata
    if (enableCommitLog) {
      printf("f%d p%d 0x%x\n", waddr, waddr + 32.U, ieee(wdata))
    }
    frfWriteBundle(1).wrdst := waddr
    frfWriteBundle(1).wrenf := true.B
    frfWriteBundle(1).wrdata := ieee(wdata)
  }
  if (useDebugROB) {
    DebugROB.pushWb(clock, reset, io.hartid, (!wbInfo(0).cp && wen(0)) || divSqrt_wen, waddr + 32.U, ieee(wdata))
  }

  when (wb_cp && (wen(0) || divSqrt_wen)) {
    io.cp_resp.bits.data := wdata
    io.cp_resp.valid := true.B
  }

  assert(!io.cp_req.valid || pipes.forall(_.lat == pipes.head.lat).B,
    s"FPU only supports coprocessor if FMA pipes have uniform latency ${pipes.map(_.lat)}")
  // Avoid structural hazards and nacking of external requests
  // toint responds in the MEM stage, so an incoming toint can induce a structural hazard against inflight FMAs
  io.cp_req.ready := !(cp_ctrl.toint && wen =/= 0.U) && !divSqrt_inFlight

  val wb_toint_valid = wb_reg_valid && wb_ctrl.toint
  val wb_toint_exc = RegEnable(fpiu.io.out.bits.exc, mem_ctrl.toint)
  io.fcsr_flags.valid := wb_toint_valid || divSqrt_wen || wen(0)
  io.fcsr_flags.bits :=
    Mux(wb_toint_valid, wb_toint_exc, 0.U) |
    Mux(divSqrt_wen, divSqrt_flags, 0.U) |
    Mux(wen(0), wexc, 0.U)

  val divSqrt_write_port_busy = (mem_ctrl.div || mem_ctrl.sqrt) && wen.orR
  io.fcsr_rdy := !(ex_reg_valid && ex_ctrl.wflags || mem_reg_valid && mem_ctrl.wflags || wb_reg_valid && wb_ctrl.toint || wen.orR || divSqrt_inFlight)
  io.nack_mem := (write_port_busy || divSqrt_write_port_busy || divSqrt_inFlight || mem_cp_valid)
  io.dec <> id_ctrl
  def useScoreboard(f: ((Pipe, Int)) => Bool) = pipes.zipWithIndex.filter(_._1.lat > 3).map(x => f(x)).fold(false.B)(_||_)
  io.sboard_set := wb_reg_valid && !wb_cp_valid && RegNext(useScoreboard(_._1.cond(mem_ctrl)) || mem_ctrl.div || mem_ctrl.sqrt || mem_ctrl.vec)
  io.sboard_clr := !wb_cp_valid && (divSqrt_wen || (wen(0) && useScoreboard(x => wbInfo(0).pipeid === x._2.U)))
  io.sboard_clra := waddr
  ccover(io.sboard_clr && load_wb, "DUAL_WRITEBACK", "load and FMA writeback on same cycle")
  // we don't currently support round-max-magnitude (rm=4)
  io.illegal_rm := io.inst(14,12).isOneOf(5.U, 6.U) || io.inst(14,12) === 7.U && io.fcsr_rm >= 5.U

  if (cfg.divSqrt) {
    val divSqrt_inValid = mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt) && !divSqrt_inFlight
    val divSqrt_killed = RegNext(divSqrt_inValid && killm, true.B)
    when (divSqrt_inValid) {
      divSqrt_waddr := mem_reg_inst(11,7)
      divSqrt_cp := mem_cp_valid
    }

    ccover(divSqrt_inFlight && divSqrt_killed, "DIV_KILLED", "divide killed after issued to divider")
    ccover(divSqrt_inFlight && mem_reg_valid && (mem_ctrl.div || mem_ctrl.sqrt), "DIV_BUSY", "divider structural hazard")
    ccover(mem_reg_valid && divSqrt_write_port_busy, "DIV_WB_STRUCTURAL", "structural hazard on division writeback")

    for (t <- floatTypes) {
      val tag = mem_ctrl.typeTagOut
      val divSqrt = withReset(divSqrt_killed) { Module(new hardfloat.DivSqrtRecFN_small(t.exp, t.sig, 0)) }
      divSqrt.io.inValid := divSqrt_inValid && tag === typeTag(t).U
      divSqrt.io.sqrtOp := mem_ctrl.sqrt
      divSqrt.io.a := maxType.unsafeConvert(fpiu.io.out.bits.in.in1, t)
      divSqrt.io.b := maxType.unsafeConvert(fpiu.io.out.bits.in.in2, t)
      divSqrt.io.roundingMode := fpiu.io.out.bits.in.rm
      divSqrt.io.detectTininess := hardfloat.consts.tininess_afterRounding

      when (!divSqrt.io.inReady) { divSqrt_inFlight := true.B } // only 1 in flight

      when (divSqrt.io.outValid_div || divSqrt.io.outValid_sqrt) {
        divSqrt_wen := !divSqrt_killed
        divSqrt_wdata := sanitizeNaN(divSqrt.io.out, t)
        divSqrt_flags := divSqrt.io.exceptionFlags
        divSqrt_typeTag := typeTag(t).U
      }
    }

    when (divSqrt_killed) { divSqrt_inFlight := false.B }
  } else {
    when (id_ctrl.div || id_ctrl.sqrt) { io.illegal_rm := true.B }
  }

  // gate the clock
  clock_en_reg := !useClockGating.B ||
    io.keep_clock_enabled || // chicken bit
    io.valid || // ID stage
    req_valid || // EX stage
    mem_reg_valid || mem_cp_valid || // MEM stage
    wb_reg_valid || wb_cp_valid || // WB stage
    wen.orR || divSqrt_inFlight || // post-WB stage
    io.ll_resp_val // load writeback

  } // leaving gated-clock domain
  val fpuImpl = withClock (gated_clock) { new FPUImpl }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"FPU_$label", "Core;;" + desc)
}
