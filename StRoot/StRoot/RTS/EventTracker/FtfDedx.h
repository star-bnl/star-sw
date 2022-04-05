//:>----------------------------------------------------------------------
//: FILE:      FtfDedx.h
//: HISTORY:
//:>----------------------------------------------------------------------
#ifndef FTFDEDX
#define FTFDEDX
#include "FtfGeneral.h"
#include "FtfTrack.h"
#include "l3CoordinateTransformer.h"


#define padLengthInnerSector    1.15     // cm
#define padLengthOuterSector    1.95     // cm

struct christofs_2d_vector {
    double x;
    double y;
};


class FtfDedx {

 public:
   FtfDedx(l3CoordinateTransformer *trafo, float cutLow = 0, float cutHigh = 0.7,
	   float driftLoss = 0);
   ~FtfDedx () {};

   int  TruncatedMean(FtfTrack *track);

 private:

   //FtfTrack *fTrack;
   l3CoordinateTransformer *fCoordTransformer;
   double    fDedxArray[45];

   float     fCutLow ;     // Truncated Mean: lower cut in %
   float     fCutHigh ;    // Truncated Mean: higher cut in %
   short     fNTruncate ;  // Pablo: # points to truncate in dEdx
   float     fDriftLoss ;  // corr. factor for drift length dependence of charge loss in m^-1
   struct christofs_2d_vector fUnitVec[24];
};
#endif
