#include <stdio.h>
#include <math.h>
#include "bracket2.h"

#define DEBUG

int
bracket2 (float  (*func) (double),
          float    fStart,
          float    fStep,
          float   *ax,
          float   *bx,
          float   *cx,
          float   *fa,
          float   *fb,
          float   *fc)
{
   int     iNumPoints = 0,
           iDone,
           iStep,
           iMaxPos,
           iLeftPos,
           iRightPos,
           i;
   float   fX,
           fVal,
           fMaxVal = 0.0,
           fPos,
           fMaxPos,
           fXlow,
           fXhigh;
   POINT   pPoints[100];
   int     iMaxPoints = 100;

   /* start in the middle, and work our way outwards */

   iStep = 1;

   fX = fStart;
   fVal = (*func)(fX);
   pPoints[iNumPoints].x = fX;
   pPoints[iNumPoints].y = fVal;
   iNumPoints++;

   fX = fStart - fStep;
   fVal = (*func)(fX);
   pPoints[iNumPoints].x = fX;
   pPoints[iNumPoints].y = fVal;
   iNumPoints++;

   fX = fStart + fStep;
   fVal = (*func)(fX);
   pPoints[iNumPoints].x = fX;
   pPoints[iNumPoints].y = fVal;
   iNumPoints++;

   iDone = 0;
   while (!iDone && iNumPoints < iMaxPoints)
   {
      if (fVal < fMaxVal)
      {
         fMaxVal = fVal;
         iMaxPos = iNumPoints - 1;
         fMaxPos = fX;
      }

      /*  If the max wasn't one of the last two points we
       *  looked at, it we must have bracketed it.
       */

      if (iNumPoints - iMaxPos > 2 && iNumPoints > 2)
      {
         iDone = 1;
         break;
      }

      /*  Didn't find a max?  Keep going! */

      fX += iStep * fStep;
      iStep = (iStep > 0 ? -1 * (iStep + 1) : -1 * (iStep - 1));
   }

   /*  OK; we have the minimum bracketed.  Now let's pick out
    *  the three bracketing points.
    */

   if (iNumPoints > 3)
   {
      iLeftPos  = max (0, iMaxPos - 2);
      iRightPos = iMaxPos + 2;
   }
   else
   {
      iLeftPos = 0;
      iRightPos = 2;
   }

   *ax = pPoints[iLeftPos].x;
   *fa = pPoints[iLeftPos].y;
   *bx = pPoints[iMaxPos].x;
   *fb = pPoints[iMaxPos].y;
   *cx = pPoints[iRightPos].x;
   *fc = pPoints[iRightPos].y;

   return iNumPoints;
}



