/**
 * Created by rayda on 08-Jan-17.
 */
public class Microbench_java {

    public static void main(String[] args) {
        System.out.println("blablub");


        double[] arr = new double[32];
        double[] arr2 = new double[32];

        for (int i = 0; i < 32; i++ ){
            arr[i] = i * 3.3;
            arr2[i] = 0.734;
        }
        {
            int repeats_inner = 10000;

            double min_time = Double.MAX_VALUE;
            for (int j = 0; j < 1000; j++) {
                long elapsedTime = System.nanoTime();
                for (int i = 0; i < repeats_inner; i++) {
                    cftf161(arr, 0, arr2, 0);
                }
                elapsedTime = System.nanoTime() - elapsedTime;
                elapsedTime = elapsedTime / repeats_inner;
                if (elapsedTime < min_time) {
                    min_time = elapsedTime;
                }
            }

            double flop = 5 * 16.0 * Math.log10(16) / Math.log10(2);
            double gflops = flop / min_time;

            System.out.println(flop + " flops " + min_time + " time " + gflops + "gflops");
        }
        {
            int repeats_inner = 10000;

            double min_time = Double.MAX_VALUE;
            for (int j = 0; j < 1000; j++) {
                long elapsedTime = System.nanoTime();
                for (int i = 0; i < repeats_inner; i++) {
                    apply(arr,arr2);
                }
                elapsedTime = System.nanoTime() - elapsedTime;
                elapsedTime = elapsedTime / repeats_inner;
                if (elapsedTime < min_time) {
                    min_time = elapsedTime;
                }
            }

            double flop = 5 * 16.0 * Math.log10(16) / Math.log10(2);
            double gflops = flop / min_time;

            System.out.println(flop + " flops " + min_time + " time " + gflops + "gflops");
        }


    }

    static double[] apply(double[] x1, double[] x2) {

        double x25 = x1[25];
        double x57 = x1[26];
        double x121 = x1[31];
        double x117 = x1[15];
        double x125 = x117 - x121;
        double x84 = x1[12];
        double x10 = x1[16];
        double x110 = x1[23];
        double x78 = x1[21];
        double x106 = x1[7];
        double x73 = x1[4];
        double x45 = x1[19];
        double x54 = x1[11];
        double x115 = x1[14];
        double x7 = x1[1];
        double x5 = x1[0];
        double x13 = x5 + x10;
        double x130 = 0 - x125;
        double x112 = x106 + x110;
        double x123 = x117 + x121;
        double x129 = x112 - x123;
        double x217 = -0.7071067811865476 * x129;
        double x74 = x1[5];
        double x80 = x74 + x78;
        double x127 = x112 + x123;
        double x12 = x1[17];
        double x14 = x7 + x12;
        double x16 = x7 - x12;
        double x43 = x1[18];
        double x88 = x1[28];
        double x93 = x84 - x88;
        double x91 = x84 + x88;
        double x41 = x1[3];
        double x49 = x41 - x45;
        double x85 = x1[13];
        double x114 = x106 - x110;
        double x23 = x1[24];
        double x18 = x1[8];
        double x28 = x18 - x23;
        double x38 = x16 - x28;
        double x26 = x18 + x23;
        double x32 = x13 - x26;
        double x76 = x1[20];
        double x81 = x73 - x76;
        double x119 = x1[30];
        double x124 = x115 - x119;
        double x132 = x114 + x124;
        double x134 = x114 - x124;
        double x260 = -0.3826834323650898 * x134;
        double x180 = 0.3826834323650898 * x132;
        double x59 = x1[27];
        double x61 = x54 + x59;
        double x63 = x54 - x59;
        double x82 = x74 - x78;
        double x101 = x82 + x93;
        double x103 = x82 - x93;
        double x244 = -0.7071067811865476 * x103;
        double x162 = 0.7071067811865476 * x101;
        double x36 = x16 + x28;
        double x262 = -0.9238795325112867 * x134;
        double x30 = x13 + x26;
        double x52 = x1[10];
        double x60 = x52 + x57;
        double x20 = x1[9];
        double x29 = x20 - x25;
        double x34 = 0 - x29;
        double x27 = x20 + x25;
        double x33 = x14 - x27;
        double x31 = x14 + x27;
        double x108 = x1[22];
        double x242 = 0.7071067811865476 * x103;
        double x39 = x1[2];
        double x46 = x39 + x43;
        double x64 = x46 + x60;
        double x66 = x46 - x60;
        double x209 = 0.7071067811865476 * x66;
        double x48 = x39 - x43;
        double x79 = x73 + x76;
        double x97 = x79 - x91;
        double x206 = x33 + x97;
        double x208 = x33 - x97;
        double x95 = x79 + x91;
        double x137 = x30 - x95;
        double x135 = x30 + x95;
        double x47 = x41 + x45;
        double x65 = x47 + x61;
        double x142 = x65 - x127;
        double x140 = x65 + x127;
        double x67 = x47 - x61;
        double x151 = 0 - x142;
        double x152 = x137 + x151;
        double x156 = x137 - x151;
        double x210 = 0.7071067811865476 * x67;
        double x211 = x209 - x210;
        double x212 = x210 + x209;
        double x15 = x5 - x10;
        double x37 = x15 - x34;
        double x35 = x15 + x34;
        double x68 = 0 - x63;
        double x69 = x48 + x68;
        double x71 = x48 - x68;
        double x251 = 0.3826834323650898 * x71;
        double x255 = 0.9238795325112867 * x71;
        double x175 = 0.3826834323650898 * x69;
        double x171 = 0.9238795325112867 * x69;
        double x62 = x52 - x57;
        double x70 = x49 + x62;
        double x174 = 0.9238795325112867 * x70;
        double x176 = x174 + x175;
        double x172 = 0.3826834323650898 * x70;
        double x173 = x171 - x172;
        double x72 = x49 - x62;
        double x252 = 0.9238795325112867 * x72;
        double x253 = x251 - x252;
        double x178 = 0.9238795325112867 * x132;
        double x90 = x1[29];
        double x92 = x85 + x90;
        double x96 = x80 + x92;
        double x138 = x31 - x96;
        double x98 = x80 - x92;
        double x204 = 0 - x98;
        double x205 = x32 + x204;
        double x207 = x32 - x204;
        double x136 = x31 + x96;
        double x148 = x136 - x140;
        double x144 = x136 + x140;
        double x94 = x85 - x90;
        double x99 = 0 - x94;
        double x102 = x81 - x99;
        double x245 = 0.7071067811865476 * x102;
        double x241 = -0.7071067811865476 * x102;
        double x243 = x241 - x242;
        double x249 = x37 - x243;
        double x247 = x37 + x243;
        double x246 = x244 + x245;
        double x248 = x38 + x246;
        double x250 = x38 - x246;
        double x254 = 0.3826834323650898 * x72;
        double x256 = x254 + x255;
        double x122 = x115 + x119;
        double x215 = 0.7071067811865476 * x129;
        double x100 = x81 + x99;
        double x104 = x1[6];
        double x161 = 0.7071067811865476 * x100;
        double x164 = x162 + x161;
        double x166 = x36 + x164;
        double x113 = x104 - x108;
        double x133 = x113 - x130;
        double x259 = -0.9238795325112867 * x133;
        double x261 = x259 - x260;
        double x265 = x253 + x261;
        double x269 = x247 + x265;
        double x267 = x253 - x261;
        double x279 = x250 + x267;
        double x263 = -0.3826834323650898 * x133;
        double x264 = x262 + x263;
        double x266 = x256 + x264;
        double x270 = x248 + x266;
        double x274 = x248 - x266;
        double x168 = x36 - x164;
        double x273 = x247 - x265;
        double x268 = x256 - x264;
        double x283 = x250 - x267;
        double x131 = x113 + x130;
        double x181 = 0.9238795325112867 * x131;
        double x177 = 0.3826834323650898 * x131;
        double x179 = x177 - x178;
        double x185 = x173 - x179;
        double x201 = x168 - x185;
        double x197 = x168 + x185;
        double x182 = x180 + x181;
        double x184 = x176 + x182;
        double x192 = x166 - x184;
        double x188 = x166 + x184;
        double x186 = x176 - x182;
        double x183 = x173 + x179;
        double x195 = 0 - x186;
        double x163 = x161 - x162;
        double x165 = x35 + x163;
        double x191 = x165 - x183;
        double x187 = x165 + x183;
        double x167 = x35 - x163;
        double x196 = x167 + x195;
        double x200 = x167 - x195;
        double x277 = 0 - x268;
        double x111 = x104 + x108;
        double x128 = x111 - x122;
        double x278 = x249 + x277;
        double x282 = x249 - x277;
        double x218 = 0.7071067811865476 * x128;
        double x214 = -0.7071067811865476 * x128;
        double x126 = x111 + x122;
        double x216 = x214 - x215;
        double x220 = x211 + x216;
        double x228 = x205 - x220;
        double x224 = x205 + x220;
        double x141 = x64 - x126;
        double x153 = x138 + x141;
        double x157 = x138 - x141;
        double x219 = x217 + x218;
        double x221 = x212 + x219;
        double x229 = x206 - x221;
        double x225 = x206 + x221;
        double x223 = x212 - x219;
        double x139 = x64 + x126;
        double x147 = x135 - x139;
        double x143 = x135 + x139;
        double x222 = x211 - x216;
        double x234 = x208 + x222;
        double x238 = x208 - x222;
        x2[0] = x143;
        double[] x145 = x2;
        double x232 = 0 - x223;
        x145[1] = x144;
        double[] x146 = x145;
        double x233 = x207 + x232;
        double x237 = x207 - x232;
        x146[16] = x147;
        double[] x149 = x146;
        x149[17] = x148;
        double[] x150 = x149;
        x150[8] = x152;
        double[] x154 = x150;
        x154[9] = x153;
        double[] x155 = x154;
        x155[24] = x156;
        double[] x158 = x155;
        x158[25] = x157;
        double[] x159 = x158;
        x159[2] = x187;
        double[] x189 = x159;
        x189[3] = x188;
        double[] x190 = x189;
        x190[18] = x191;
        double[] x193 = x190;
        x193[19] = x192;
        double[] x194 = x193;
        x194[10] = x196;
        double[] x198 = x194;
        x198[11] = x197;
        double[] x199 = x198;
        x199[26] = x200;
        double[] x202 = x199;
        x202[27] = x201;
        double[] x203 = x202;
        x203[4] = x224;
        double[] x226 = x203;
        x226[5] = x225;
        double[] x227 = x226;
        x227[20] = x228;
        double[] x230 = x227;
        x230[21] = x229;
        double[] x231 = x230;
        x231[12] = x233;
        double[] x235 = x231;
        x235[13] = x234;
        double[] x236 = x235;
        x236[28] = x237;
        double[] x239 = x236;
        x239[29] = x238;
        double[] x240 = x239;
        x240[6] = x269;
        double[] x271 = x240;
        x271[7] = x270;
        double[] x272 = x271;
        x272[22] = x273;
        double[] x275 = x272;
        x275[23] = x274;
        double[] x276 = x275;
        x276[14] = x278;
        double[] x280 = x276;
        x280[15] = x279;
        double[] x281 = x280;
        x281[30] = x282;
        double[] x284 = x281;
        x284[31] = x283;
        double[] x285 = x284;

        return x285;
    }


    public static void cftf161(double[] a, int offa, double[] w, int startw)
    {
        double wn4r, wk1r, wk1i, x0r, x0i, x1r, x1i, x2r, x2i, x3r, x3i, y0r, y0i, y1r, y1i, y2r, y2i, y3r, y3i, y4r, y4i, y5r, y5i, y6r, y6i, y7r, y7i, y8r, y8i, y9r, y9i, y10r, y10i, y11r, y11i, y12r, y12i, y13r, y13i, y14r, y14i, y15r, y15i;

        wn4r = w[startw + 1];
        wk1r = w[startw + 2];
        wk1i = w[startw + 3];

        x0r = a[offa] + a[offa + 16];
        x0i = a[offa + 1] + a[offa + 17];
        x1r = a[offa] - a[offa + 16];
        x1i = a[offa + 1] - a[offa + 17];
        x2r = a[offa + 8] + a[offa + 24];
        x2i = a[offa + 9] + a[offa + 25];
        x3r = a[offa + 8] - a[offa + 24];
        x3i = a[offa + 9] - a[offa + 25];
        y0r = x0r + x2r;
        y0i = x0i + x2i;
        y4r = x0r - x2r;
        y4i = x0i - x2i;
        y8r = x1r - x3i;
        y8i = x1i + x3r;
        y12r = x1r + x3i;
        y12i = x1i - x3r;
        x0r = a[offa + 2] + a[offa + 18];
        x0i = a[offa + 3] + a[offa + 19];
        x1r = a[offa + 2] - a[offa + 18];
        x1i = a[offa + 3] - a[offa + 19];
        x2r = a[offa + 10] + a[offa + 26];
        x2i = a[offa + 11] + a[offa + 27];
        x3r = a[offa + 10] - a[offa + 26];
        x3i = a[offa + 11] - a[offa + 27];
        y1r = x0r + x2r;
        y1i = x0i + x2i;
        y5r = x0r - x2r;
        y5i = x0i - x2i;
        x0r = x1r - x3i;
        x0i = x1i + x3r;
        y9r = wk1r * x0r - wk1i * x0i;
        y9i = wk1r * x0i + wk1i * x0r;
        x0r = x1r + x3i;
        x0i = x1i - x3r;
        y13r = wk1i * x0r - wk1r * x0i;
        y13i = wk1i * x0i + wk1r * x0r;
        x0r = a[offa + 4] + a[offa + 20];
        x0i = a[offa + 5] + a[offa + 21];
        x1r = a[offa + 4] - a[offa + 20];
        x1i = a[offa + 5] - a[offa + 21];
        x2r = a[offa + 12] + a[offa + 28];
        x2i = a[offa + 13] + a[offa + 29];
        x3r = a[offa + 12] - a[offa + 28];
        x3i = a[offa + 13] - a[offa + 29];
        y2r = x0r + x2r;
        y2i = x0i + x2i;
        y6r = x0r - x2r;
        y6i = x0i - x2i;
        x0r = x1r - x3i;
        x0i = x1i + x3r;
        y10r = wn4r * (x0r - x0i);
        y10i = wn4r * (x0i + x0r);
        x0r = x1r + x3i;
        x0i = x1i - x3r;
        y14r = wn4r * (x0r + x0i);
        y14i = wn4r * (x0i - x0r);
        x0r = a[offa + 6] + a[offa + 22];
        x0i = a[offa + 7] + a[offa + 23];
        x1r = a[offa + 6] - a[offa + 22];
        x1i = a[offa + 7] - a[offa + 23];
        x2r = a[offa + 14] + a[offa + 30];
        x2i = a[offa + 15] + a[offa + 31];
        x3r = a[offa + 14] - a[offa + 30];
        x3i = a[offa + 15] - a[offa + 31];
        y3r = x0r + x2r;
        y3i = x0i + x2i;
        y7r = x0r - x2r;
        y7i = x0i - x2i;
        x0r = x1r - x3i;
        x0i = x1i + x3r;
        y11r = wk1i * x0r - wk1r * x0i;
        y11i = wk1i * x0i + wk1r * x0r;
        x0r = x1r + x3i;
        x0i = x1i - x3r;
        y15r = wk1r * x0r - wk1i * x0i;
        y15i = wk1r * x0i + wk1i * x0r;
        x0r = y12r - y14r;
        x0i = y12i - y14i;
        x1r = y12r + y14r;
        x1i = y12i + y14i;
        x2r = y13r - y15r;
        x2i = y13i - y15i;
        x3r = y13r + y15r;
        x3i = y13i + y15i;
        a[offa + 24] = x0r + x2r;
        a[offa + 25] = x0i + x2i;
        a[offa + 26] = x0r - x2r;
        a[offa + 27] = x0i - x2i;
        a[offa + 28] = x1r - x3i;
        a[offa + 29] = x1i + x3r;
        a[offa + 30] = x1r + x3i;
        a[offa + 31] = x1i - x3r;
        x0r = y8r + y10r;
        x0i = y8i + y10i;
        x1r = y8r - y10r;
        x1i = y8i - y10i;
        x2r = y9r + y11r;
        x2i = y9i + y11i;
        x3r = y9r - y11r;
        x3i = y9i - y11i;
        a[offa + 16] = x0r + x2r;
        a[offa + 17] = x0i + x2i;
        a[offa + 18] = x0r - x2r;
        a[offa + 19] = x0i - x2i;
        a[offa + 20] = x1r - x3i;
        a[offa + 21] = x1i + x3r;
        a[offa + 22] = x1r + x3i;
        a[offa + 23] = x1i - x3r;
        x0r = y5r - y7i;
        x0i = y5i + y7r;
        x2r = wn4r * (x0r - x0i);
        x2i = wn4r * (x0i + x0r);
        x0r = y5r + y7i;
        x0i = y5i - y7r;
        x3r = wn4r * (x0r - x0i);
        x3i = wn4r * (x0i + x0r);
        x0r = y4r - y6i;
        x0i = y4i + y6r;
        x1r = y4r + y6i;
        x1i = y4i - y6r;
        a[offa + 8] = x0r + x2r;
        a[offa + 9] = x0i + x2i;
        a[offa + 10] = x0r - x2r;
        a[offa + 11] = x0i - x2i;
        a[offa + 12] = x1r - x3i;
        a[offa + 13] = x1i + x3r;
        a[offa + 14] = x1r + x3i;
        a[offa + 15] = x1i - x3r;
        x0r = y0r + y2r;
        x0i = y0i + y2i;
        x1r = y0r - y2r;
        x1i = y0i - y2i;
        x2r = y1r + y3r;
        x2i = y1i + y3i;
        x3r = y1r - y3r;
        x3i = y1i - y3i;
        a[offa] = x0r + x2r;
        a[offa + 1] = x0i + x2i;
        a[offa + 2] = x0r - x2r;
        a[offa + 3] = x0i - x2i;
        a[offa + 4] = x1r - x3i;
        a[offa + 5] = x1i + x3r;
        a[offa + 6] = x1r + x3i;
        a[offa + 7] = x1i - x3r;
    }
}
