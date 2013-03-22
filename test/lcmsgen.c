/*
   Copyright (c) 2013 Török Edwin
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Török Edwin nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/* Usage:
 * $ gcc lcmsgen.c `pkg-config lcms2 --libs --cflags` -O2 -o lcmsgen
 * $ ./lcmsgen >rgbtest.csv
 */
#include <lcms2.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

static double frand(void)
{
    return (double)rand() / (double)RAND_MAX;
}

static cmsHTRANSFORM xform_srgb_to_lrgb;
static cmsHTRANSFORM xform_srgb_to_lab;
static cmsHTRANSFORM xform_srgb_to_lgray;

static void generate_testcase(double r, double g, double b)
{
    double SRGB[3] = {r,g,b};
    double LRGB[3];
    cmsCIELab Lab;
    double gray;
    cmsDoTransform(xform_srgb_to_lrgb, SRGB, LRGB, 1);
    cmsDoTransform(xform_srgb_to_lab, SRGB, &Lab, 1);
    cmsDoTransform(xform_srgb_to_lgray, SRGB, &gray, 1);
    /* Use 6 digits precision. do not use more because you'd start seeing
     * small errors, probably because LCMS is using single precision float
     * internally.
     * This means 4 fractional digits for Lab, since it already has ~2 before.
     * */
    printf("%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.4f,%.4f,%.4f,%6f\n",
           SRGB[0],SRGB[1],SRGB[2],
           LRGB[0],LRGB[1],LRGB[2],
           Lab.L, Lab.a, Lab.b, gray);
}

static void errlog(cmsContext id, cmsUInt32Number code, const char *text)
{
    printf("Error (%x): %s\n", code, text);
}

static void init(void)
{
   cmsCIExyY D65;
   cmsWhitePointFromTemp(&D65, 6504);
   cmsToneCurve *linear = cmsBuildGamma(NULL, 1.0);
   cmsToneCurve *linrgb[3] = {linear,linear,linear};
   cmsCIExyYTRIPLE primaries = {
       {0.64, 0.33, 1.0},
       {0.30, 0.60, 1.0},
       {0.15, 0.06, 1.0}
   };

   cmsHPROFILE hsRGB = cmsCreate_sRGBProfile();
   cmsHPROFILE hLab = cmsCreateLab4Profile(NULL);
   cmsHPROFILE hlRGB = cmsCreateRGBProfile(&D65, &primaries, linrgb);
   cmsHPROFILE hlGray = cmsCreateGrayProfile(cmsD50_xyY(), linear);

   cmsSetHeaderFlags(hlGray, cmsEmbeddedProfileTrue);
   cmsSaveProfileToFile(hlGray,"lgray.icc");
   cmsSetHeaderFlags(hlRGB, cmsEmbeddedProfileTrue);
   cmsSaveProfileToFile(hlRGB,"lrgb.icc");

   xform_srgb_to_lrgb = cmsCreateTransform(hsRGB, TYPE_RGB_DBL,
                                           hlRGB, TYPE_RGB_DBL,
                                           INTENT_RELATIVE_COLORIMETRIC,
                                           cmsFLAGS_NOOPTIMIZE  /* preserve precision */
                                         );
   xform_srgb_to_lab = cmsCreateTransform(hsRGB, TYPE_RGB_DBL, hLab,
                                          TYPE_Lab_DBL, INTENT_RELATIVE_COLORIMETRIC,
                                          cmsFLAGS_NOOPTIMIZE);
   xform_srgb_to_lgray = cmsCreateTransform(hsRGB, TYPE_RGB_DBL, hlGray,
                                           TYPE_GRAY_DBL, INTENT_RELATIVE_COLORIMETRIC,
                                           cmsFLAGS_NOOPTIMIZE);
   cmsCloseProfile(hsRGB);
   cmsCloseProfile(hlRGB);
   cmsCloseProfile(hLab);
   cmsCloseProfile(hlGray);
   cmsFreeToneCurve(linear);
   cmsSetLogErrorHandler(errlog);
   /* sRGB, RGB, Lab, Gray */
   printf("R',G',B',R,G,B,L,a,b,Gray\n");
}

static void done(void)
{
    cmsDeleteTransform(xform_srgb_to_lrgb);
    cmsDeleteTransform(xform_srgb_to_lab);
    cmsDeleteTransform(xform_srgb_to_lgray);
}

int main(void)
{
    unsigned i;
    init();
    /* Generate testcases for in-gamut colors */

    /* generate testcases for monochromatic values */
    for (i=0;i<256;i++) {
        double v = ((double)i) / 255.0;
        generate_testcase(v,v,v);
    }
    /* generate some color testcases */
    for (i=0;i<256;i++) {
        double r = ((double)i) / 255.0;
        double g = fmod((double)i + 128.5, 255.0) / 255.0;
        double b = fmod(255.25 - (double)i, 255.0) / 255.0;
        generate_testcase(r,g,b);
    }
    srand(1);
    /* generate some random colors */
    for (i=0;i<256;i++) {
        generate_testcase(frand(),frand(),frand());
    }
    done();
}
