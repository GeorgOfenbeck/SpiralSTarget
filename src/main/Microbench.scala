/**
  * Created by rayda on 08-Jan-17.
  */

import scala.annotation.tailrec
import scala.util.Random
import org.scalameter._

import java.util.concurrent._
import scala.util.DynamicVariable


object Microbench extends App {

  val test: Array[Double] = (for (i <- 0 until 32) yield i * 3.3).toArray

  val w: Array[Double] = Array(0.73, 0.73, 0.700123, -0.54231)


  def apply(x1: Array[Double], x2: Array[Double]): ((Array[Double])) = {

    val x25 = x1(25)
    val x57 = x1(26)
    val x121 = x1(31)
    val x117 = x1(15)
    val x125 = x117 - x121
    val x84 = x1(12)
    val x10 = x1(16)
    val x110 = x1(23)
    val x78 = x1(21)
    val x106 = x1(7)
    val x73 = x1(4)
    val x45 = x1(19)
    val x54 = x1(11)
    val x115 = x1(14)
    val x7 = x1(1)
    val x5 = x1(0)
    val x13 = x5 + x10
    val x130 = 0 - x125
    val x112 = x106 + x110
    val x123 = x117 + x121
    val x129 = x112 - x123
    val x217 = -0.7071067811865476 * x129
    val x74 = x1(5)
    val x80 = x74 + x78
    val x127 = x112 + x123
    val x12 = x1(17)
    val x14 = x7 + x12
    val x16 = x7 - x12
    val x43 = x1(18)
    val x88 = x1(28)
    val x93 = x84 - x88
    val x91 = x84 + x88
    val x41 = x1(3)
    val x49 = x41 - x45
    val x85 = x1(13)
    val x114 = x106 - x110
    val x23 = x1(24)
    val x18 = x1(8)
    val x28 = x18 - x23
    val x38 = x16 - x28
    val x26 = x18 + x23
    val x32 = x13 - x26
    val x76 = x1(20)
    val x81 = x73 - x76
    val x119 = x1(30)
    val x124 = x115 - x119
    val x132 = x114 + x124
    val x134 = x114 - x124
    val x260 = -0.3826834323650898 * x134
    val x180 = 0.3826834323650898 * x132
    val x59 = x1(27)
    val x61 = x54 + x59
    val x63 = x54 - x59
    val x82 = x74 - x78
    val x101 = x82 + x93
    val x103 = x82 - x93
    val x244 = -0.7071067811865476 * x103
    val x162 = 0.7071067811865476 * x101
    val x36 = x16 + x28
    val x262 = -0.9238795325112867 * x134
    val x30 = x13 + x26
    val x52 = x1(10)
    val x60 = x52 + x57
    val x20 = x1(9)
    val x29 = x20 - x25
    val x34 = 0 - x29
    val x27 = x20 + x25
    val x33 = x14 - x27
    val x31 = x14 + x27
    val x108 = x1(22)
    val x242 = 0.7071067811865476 * x103
    val x39 = x1(2)
    val x46 = x39 + x43
    val x64 = x46 + x60
    val x66 = x46 - x60
    val x209 = 0.7071067811865476 * x66
    val x48 = x39 - x43
    val x79 = x73 + x76
    val x97 = x79 - x91
    val x206 = x33 + x97
    val x208 = x33 - x97
    val x95 = x79 + x91
    val x137 = x30 - x95
    val x135 = x30 + x95
    val x47 = x41 + x45
    val x65 = x47 + x61
    val x142 = x65 - x127
    val x140 = x65 + x127
    val x67 = x47 - x61
    val x151 = 0 - x142
    val x152 = x137 + x151
    val x156 = x137 - x151
    val x210 = 0.7071067811865476 * x67
    val x211 = x209 - x210
    val x212 = x210 + x209
    val x15 = x5 - x10
    val x37 = x15 - x34
    val x35 = x15 + x34
    val x68 = 0 - x63
    val x69 = x48 + x68
    val x71 = x48 - x68
    val x251 = 0.3826834323650898 * x71
    val x255 = 0.9238795325112867 * x71
    val x175 = 0.3826834323650898 * x69
    val x171 = 0.9238795325112867 * x69
    val x62 = x52 - x57
    val x70 = x49 + x62
    val x174 = 0.9238795325112867 * x70
    val x176 = x174 + x175
    val x172 = 0.3826834323650898 * x70
    val x173 = x171 - x172
    val x72 = x49 - x62
    val x252 = 0.9238795325112867 * x72
    val x253 = x251 - x252
    val x178 = 0.9238795325112867 * x132
    val x90 = x1(29)
    val x92 = x85 + x90
    val x96 = x80 + x92
    val x138 = x31 - x96
    val x98 = x80 - x92
    val x204 = 0 - x98
    val x205 = x32 + x204
    val x207 = x32 - x204
    val x136 = x31 + x96
    val x148 = x136 - x140
    val x144 = x136 + x140
    val x94 = x85 - x90
    val x99 = 0 - x94
    val x102 = x81 - x99
    val x245 = 0.7071067811865476 * x102
    val x241 = -0.7071067811865476 * x102
    val x243 = x241 - x242
    val x249 = x37 - x243
    val x247 = x37 + x243
    val x246 = x244 + x245
    val x248 = x38 + x246
    val x250 = x38 - x246
    val x254 = 0.3826834323650898 * x72
    val x256 = x254 + x255
    val x122 = x115 + x119
    val x215 = 0.7071067811865476 * x129
    val x100 = x81 + x99
    val x104 = x1(6)
    val x161 = 0.7071067811865476 * x100
    val x164 = x162 + x161
    val x166 = x36 + x164
    val x113 = x104 - x108
    val x133 = x113 - x130
    val x259 = -0.9238795325112867 * x133
    val x261 = x259 - x260
    val x265 = x253 + x261
    val x269 = x247 + x265
    val x267 = x253 - x261
    val x279 = x250 + x267
    val x263 = -0.3826834323650898 * x133
    val x264 = x262 + x263
    val x266 = x256 + x264
    val x270 = x248 + x266
    val x274 = x248 - x266
    val x168 = x36 - x164
    val x273 = x247 - x265
    val x268 = x256 - x264
    val x283 = x250 - x267
    val x131 = x113 + x130
    val x181 = 0.9238795325112867 * x131
    val x177 = 0.3826834323650898 * x131
    val x179 = x177 - x178
    val x185 = x173 - x179
    val x201 = x168 - x185
    val x197 = x168 + x185
    val x182 = x180 + x181
    val x184 = x176 + x182
    val x192 = x166 - x184
    val x188 = x166 + x184
    val x186 = x176 - x182
    val x183 = x173 + x179
    val x195 = 0 - x186
    val x163 = x161 - x162
    val x165 = x35 + x163
    val x191 = x165 - x183
    val x187 = x165 + x183
    val x167 = x35 - x163
    val x196 = x167 + x195
    val x200 = x167 - x195
    val x277 = 0 - x268
    val x111 = x104 + x108
    val x128 = x111 - x122
    val x278 = x249 + x277
    val x282 = x249 - x277
    val x218 = 0.7071067811865476 * x128
    val x214 = -0.7071067811865476 * x128
    val x126 = x111 + x122
    val x216 = x214 - x215
    val x220 = x211 + x216
    val x228 = x205 - x220
    val x224 = x205 + x220
    val x141 = x64 - x126
    val x153 = x138 + x141
    val x157 = x138 - x141
    val x219 = x217 + x218
    val x221 = x212 + x219
    val x229 = x206 - x221
    val x225 = x206 + x221
    val x223 = x212 - x219
    val x139 = x64 + x126
    val x147 = x135 - x139
    val x143 = x135 + x139
    val x222 = x211 - x216
    val x234 = x208 + x222
    val x238 = x208 - x222
    val x145 = {
      x2.update(0, x143); x2
    }
    val x232 = 0 - x223
    val x146 = {
      x145.update(1, x144); x145
    }
    val x233 = x207 + x232
    val x237 = x207 - x232
    val x149 = {
      x146.update(16, x147); x146
    }
    val x150 = {
      x149.update(17, x148); x149
    }
    val x154 = {
      x150.update(8, x152); x150
    }
    val x155 = {
      x154.update(9, x153); x154
    }
    val x158 = {
      x155.update(24, x156); x155
    }
    val x159 = {
      x158.update(25, x157); x158
    }
    val x189 = {
      x159.update(2, x187); x159
    }
    val x190 = {
      x189.update(3, x188); x189
    }
    val x193 = {
      x190.update(18, x191); x190
    }
    val x194 = {
      x193.update(19, x192); x193
    }
    val x198 = {
      x194.update(10, x196); x194
    }
    val x199 = {
      x198.update(11, x197); x198
    }
    val x202 = {
      x199.update(26, x200); x199
    }
    val x203 = {
      x202.update(27, x201); x202
    }
    val x226 = {
      x203.update(4, x224); x203
    }
    val x227 = {
      x226.update(5, x225); x226
    }
    val x230 = {
      x227.update(20, x228); x227
    }
    val x231 = {
      x230.update(21, x229); x230
    }
    val x235 = {
      x231.update(12, x233); x231
    }
    val x236 = {
      x235.update(13, x234); x235
    }
    val x239 = {
      x236.update(28, x237); x236
    }
    val x240 = {
      x239.update(29, x238); x239
    }
    val x271 = {
      x240.update(6, x269); x240
    }
    val x272 = {
      x271.update(7, x270); x271
    }
    val x275 = {
      x272.update(22, x273); x272
    }
    val x276 = {
      x275.update(23, x274); x275
    }
    val x280 = {
      x276.update(14, x278); x276
    }
    val x281 = {
      x280.update(15, x279); x280
    }
    val x284 = {
      x281.update(30, x282); x281
    }
    val x285 = {
      x284.update(31, x283); x284
    }

    (x285)
  }

  def cftf161(a: Array[Double], offa: Int, w: Array[Double], startw: Int) {
    var wn4r = .0
    var wk1r = .0
    var wk1i = .0
    var x0r = .0
    var x0i = .0
    var x1r = .0
    var x1i = .0
    var x2r = .0
    var x2i = .0
    var x3r = .0
    var x3i = .0
    var y0r = .0
    var y0i = .0
    var y1r = .0
    var y1i = .0
    var y2r = .0
    var y2i = .0
    var y3r = .0
    var y3i = .0
    var y4r = .0
    var y4i = .0
    var y5r = .0
    var y5i = .0
    var y6r = .0
    var y6i = .0
    var y7r = .0
    var y7i = .0
    var y8r = .0
    var y8i = .0
    var y9r = .0
    var y9i = .0
    var y10r = .0
    var y10i = .0
    var y11r = .0
    var y11i = .0
    var y12r = .0
    var y12i = .0
    var y13r = .0
    var y13i = .0
    var y14r = .0
    var y14i = .0
    var y15r = .0
    var y15i = .0
    wn4r = w(startw + 1)
    wk1r = w(startw + 2)
    wk1i = w(startw + 3)
    x0r = a(offa) + a(offa + 16)
    x0i = a(offa + 1) + a(offa + 17)
    x1r = a(offa) - a(offa + 16)
    x1i = a(offa + 1) - a(offa + 17)
    x2r = a(offa + 8) + a(offa + 24)
    x2i = a(offa + 9) + a(offa + 25)
    x3r = a(offa + 8) - a(offa + 24)
    x3i = a(offa + 9) - a(offa + 25)
    y0r = x0r + x2r
    y0i = x0i + x2i
    y4r = x0r - x2r
    y4i = x0i - x2i
    y8r = x1r - x3i
    y8i = x1i + x3r
    y12r = x1r + x3i
    y12i = x1i - x3r
    x0r = a(offa + 2) + a(offa + 18)
    x0i = a(offa + 3) + a(offa + 19)
    x1r = a(offa + 2) - a(offa + 18)
    x1i = a(offa + 3) - a(offa + 19)
    x2r = a(offa + 10) + a(offa + 26)
    x2i = a(offa + 11) + a(offa + 27)
    x3r = a(offa + 10) - a(offa + 26)
    x3i = a(offa + 11) - a(offa + 27)
    y1r = x0r + x2r
    y1i = x0i + x2i
    y5r = x0r - x2r
    y5i = x0i - x2i
    x0r = x1r - x3i
    x0i = x1i + x3r
    y9r = wk1r * x0r - wk1i * x0i
    y9i = wk1r * x0i + wk1i * x0r
    x0r = x1r + x3i
    x0i = x1i - x3r
    y13r = wk1i * x0r - wk1r * x0i
    y13i = wk1i * x0i + wk1r * x0r
    x0r = a(offa + 4) + a(offa + 20)
    x0i = a(offa + 5) + a(offa + 21)
    x1r = a(offa + 4) - a(offa + 20)
    x1i = a(offa + 5) - a(offa + 21)
    x2r = a(offa + 12) + a(offa + 28)
    x2i = a(offa + 13) + a(offa + 29)
    x3r = a(offa + 12) - a(offa + 28)
    x3i = a(offa + 13) - a(offa + 29)
    y2r = x0r + x2r
    y2i = x0i + x2i
    y6r = x0r - x2r
    y6i = x0i - x2i
    x0r = x1r - x3i
    x0i = x1i + x3r
    y10r = wn4r * (x0r - x0i)
    y10i = wn4r * (x0i + x0r)
    x0r = x1r + x3i
    x0i = x1i - x3r
    y14r = wn4r * (x0r + x0i)
    y14i = wn4r * (x0i - x0r)
    x0r = a(offa + 6) + a(offa + 22)
    x0i = a(offa + 7) + a(offa + 23)
    x1r = a(offa + 6) - a(offa + 22)
    x1i = a(offa + 7) - a(offa + 23)
    x2r = a(offa + 14) + a(offa + 30)
    x2i = a(offa + 15) + a(offa + 31)
    x3r = a(offa + 14) - a(offa + 30)
    x3i = a(offa + 15) - a(offa + 31)
    y3r = x0r + x2r
    y3i = x0i + x2i
    y7r = x0r - x2r
    y7i = x0i - x2i
    x0r = x1r - x3i
    x0i = x1i + x3r
    y11r = wk1i * x0r - wk1r * x0i
    y11i = wk1i * x0i + wk1r * x0r
    x0r = x1r + x3i
    x0i = x1i - x3r
    y15r = wk1r * x0r - wk1i * x0i
    y15i = wk1r * x0i + wk1i * x0r
    x0r = y12r - y14r
    x0i = y12i - y14i
    x1r = y12r + y14r
    x1i = y12i + y14i
    x2r = y13r - y15r
    x2i = y13i - y15i
    x3r = y13r + y15r
    x3i = y13i + y15i
    a(offa + 24) = x0r + x2r
    a(offa + 25) = x0i + x2i
    a(offa + 26) = x0r - x2r
    a(offa + 27) = x0i - x2i
    a(offa + 28) = x1r - x3i
    a(offa + 29) = x1i + x3r
    a(offa + 30) = x1r + x3i
    a(offa + 31) = x1i - x3r
    x0r = y8r + y10r
    x0i = y8i + y10i
    x1r = y8r - y10r
    x1i = y8i - y10i
    x2r = y9r + y11r
    x2i = y9i + y11i
    x3r = y9r - y11r
    x3i = y9i - y11i
    a(offa + 16) = x0r + x2r
    a(offa + 17) = x0i + x2i
    a(offa + 18) = x0r - x2r
    a(offa + 19) = x0i - x2i
    a(offa + 20) = x1r - x3i
    a(offa + 21) = x1i + x3r
    a(offa + 22) = x1r + x3i
    a(offa + 23) = x1i - x3r
    x0r = y5r - y7i
    x0i = y5i + y7r
    x2r = wn4r * (x0r - x0i)
    x2i = wn4r * (x0i + x0r)
    x0r = y5r + y7i
    x0i = y5i - y7r
    x3r = wn4r * (x0r - x0i)
    x3i = wn4r * (x0i + x0r)
    x0r = y4r - y6i
    x0i = y4i + y6r
    x1r = y4r + y6i
    x1i = y4i - y6r
    a(offa + 8) = x0r + x2r
    a(offa + 9) = x0i + x2i
    a(offa + 10) = x0r - x2r
    a(offa + 11) = x0i - x2i
    a(offa + 12) = x1r - x3i
    a(offa + 13) = x1i + x3r
    a(offa + 14) = x1r + x3i
    a(offa + 15) = x1i - x3r
    x0r = y0r + y2r
    x0i = y0i + y2i
    x1r = y0r - y2r
    x1i = y0i - y2i
    x2r = y1r + y3r
    x2i = y1i + y3i
    x3r = y1r - y3r
    x3i = y1i - y3i
    a(offa) = x0r + x2r
    a(offa + 1) = x0i + x2i
    a(offa + 2) = x0r - x2r
    a(offa + 3) = x0i - x2i
    a(offa + 4) = x1r - x3i
    a(offa + 5) = x1i + x3r
    a(offa + 6) = x1r + x3i
    a(offa + 7) = x1i - x3r
  }


  val standardConfig = config(
    Key.exec.minWarmupRuns -> 1000,
    Key.exec.maxWarmupRuns -> 10000,
    Key.exec.benchRuns -> 10000,
    Key.verbose -> false
  ) withWarmer (new Warmer.Default)

  val t1 = standardConfig measure {
    apply(test, test)
  }

  val t2 = standardConfig measure {
    cftf161(test, 0, w, 0)
  }

  val t3 = standardConfig measure {
    apply(test, test)
  }

  val t4 = standardConfig measure {
    cftf161(test, 0, w, 0)
  }

  def gf(t: Double) = {
    (5 * 16.0 * Math.log10(16) / Math.log10(16)) / (t * 1000000)
  }

  Vector(t1, t2, t3, t4).map(t => println(gf(t)))


  val arr = new Array[Double](32)
  val arr2 = new Array[Double](32)
  var i = 0
  while (i < 32) {
    {
      arr(i) = i * 3.3
      arr2(i) = 0.734
    }
    {
      i += 1;
      i - 1
    }
  }
  val repeats_inner = 10000
  var min_time = Double.MaxValue
  var j = 0
  while (j < 1000) {
    {
      var elapsedTime = System.nanoTime
      var i = 0
      while (i < repeats_inner) {
        {
          cftf161(arr, 0, arr2, 0)
        }
        {
          i += 1;
          i - 1
        }
      }
      elapsedTime = System.nanoTime - elapsedTime
      elapsedTime = elapsedTime / repeats_inner
      if (elapsedTime < min_time) min_time = elapsedTime
    }
    {
      j += 1;
      j - 1
    }
  }
  val flop = 5 * 16.0 * Math.log10(16) / Math.log10(2)
  val gflops = gf(min_time/1000000)
  System.out.println(flop + " flops " + min_time + " time " + gflops + "gflops")
}
