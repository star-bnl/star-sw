//:>----------------------------------------------------------------------
//: FILE:      FtfDedx.h
//: HISTORY:
//:>----------------------------------------------------------------------
#ifndef FTFDEDX
#define FTFDEDX
#include "Stl3Util/ftf/FtfGeneral.h"
#include "Stl3Util/ftf/FtfTrack.h"
#include "Stl3Util/base/St_l3_Coordinate_Transformer.h"


#define padLengthInnerSector    1.15     // cm
#define padLengthOuterSector    1.95     // cm

struct christofs_2d_vector {
    double x;
    double y;
};


class FtfDedx {

 public:
   FtfDedx(St_l3_Coordinate_Transformer *trafo, float cutLow = 0, float cutHigh = 0.7,
	   float driftLoss = 0);
   ~FtfDedx () {};

   int  TruncatedMean(FtfTrack *track);

 private:

   //FtfTrack *fTrack;
   St_l3_Coordinate_Transformer *fCoordTransformer;
   double    fDedxArray[45];

   float     fCutLow ;     // Truncated Mean: lower cut in %
   float     fCutHigh ;    // Truncated Mean: higher cut in %
   short     fNTruncate ;  // Pablo: # points to truncate in dEdx
   float     fDriftLoss ;  // corr. factor for drift length dependence of charge loss in m^-1
   struct christofs_2d_vector fUnitVec[24];
};
#endif
