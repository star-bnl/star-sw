#include <math.h>

#include "sft.h"
#include "bracket2.h"

extern int icall;

double WeightMean ();


double
sft_find_vertex (double fZlow, double fZhigh)
{

   POINT   pPoints[NUM_POINTS],
           pFitPoints[NUM_POINTS];
   int     i,
           j,
           iLowX,
           iLowY,
           iHighY,
           iActualTracks,
           iNumFitPoints,
           iNumPoints;
   double  fMax = -10000.0, fTracks;
   double  fPrevMax, fWeightMax;
   float ax, bx, cx, fa, fb, fc;
   float d, p, q, r;

   icall = 1;


   iNumPoints = bracket2 (sft_vf_ntrack, 0.0, 0.1, &ax, &bx, &cx, &fa, &fb, &fc);

   printf ("point a:   % 12.5f    % 12.5f\n", ax, fa);
   printf ("point b:   % 12.5f    % 12.5f\n", bx, fb);
   printf ("point c:   % 12.5f    % 12.5f\n", cx, fc);

   r = (bx - cx) * (fb - fa);
   q = (bx - ax) * (fb - fc);
   p = (bx - ax)*q - (bx - cx)*r;
   q = 2.0 * (q - r);

   if (q > 0.0)
      p = -p;
   else
      q = -q;

   d = bx + p / q;

   return d;
}

