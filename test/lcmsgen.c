/*---------------------------------------------------------------------------
   Copyright (c) 2013 Török Edwin

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/

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
   cmsCIEXYZ D65_XYZ = {0.95047, 1.0, 1.08883 };
   cmsCIExyY D65;
   cmsXYZ2xyY(&D65, &D65_XYZ);

   cmsToneCurve *linear = cmsBuildGamma(NULL, 1.0);
   cmsToneCurve *linrgb[3] = {linear,linear,linear};
   cmsCIExyYTRIPLE primaries = {
       {0.64, 0.33, 1.0},
       {0.30, 0.60, 1.0},
       {0.15, 0.06, 1.0}
   };

   cmsFloat64Number P[5] = { 2.4, 1. / 1.055, 0.055 / 1.055, 1. / 12.92, 0.04045 };
   cmsToneCurve *srgb = cmsBuildParametricToneCurve(NULL, 4, P);
   cmsToneCurve *srgbcurve[3] = {srgb,srgb,srgb};
   cmsHPROFILE hsRGB = cmsCreateRGBProfile(&D65, &primaries, srgbcurve);
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
   cmsFreeToneCurve(srgb);
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
