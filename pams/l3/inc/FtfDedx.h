//:>----------------------------------------------------------------------
//: FILE:      FtfDedx.h
//: HISTORY:
//:>----------------------------------------------------------------------
#ifndef FTFDEDX
#define FTFDEDX
#include "FtfGeneral.h"
#include "FtfTrack.h"
#include "sl3CoordinateTransform.h"

#define ClassDef(a,b)

struct vector {
    double x;
    double y;
};


class FtfDedx {

 public:
   FtfDedx (FtfTrack *track);
   FtfDedx (FtfTrack *track, float cutLow, float cutHigh);
   FtfDedx (FtfTrack *track, float cutLow, float cutHigh, float driftLoss);
   ~FtfDedx () {};

   int  TruncatedMean ();
   void PabloDedx ();

 private:

   FtfTrack *fTrack;
   double    fDedxArray[45];

   float     fCutLow ;     // Truncated Mean: lower cut in %
   float     fCutHigh ;    // Truncated Mean: higher cut in %
   short     fNTruncate ;  // Pablo: # points to truncate in dEdx
   float     fDriftLoss ;  // corr. factor for drift length dependence of charge loss in m^-1
   struct vector fUnitVec[24];

   ClassDef(FtfDedx,1)
};
#endif
