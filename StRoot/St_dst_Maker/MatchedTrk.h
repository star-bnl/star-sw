#ifndef MATCHEDTRK_H
#define MATCHEDTRK_H

#include "CtbResponse.h"

#include <vector>
#include "SystemOfUnits.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std::vector;
using namespace units;
#endif

#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "TMath.h"
#include "tables/St_dst_track_Table.h"

#include "StEventTypes.h" // for StEvent only

#include "math_constants.h"

#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

struct Jtrk { 
  StPhysicalHelixD  helix;
  double  sigma;
  dst_track_st *glb_track_pointer;
};

class MatchedTrk {
 public:
  MatchedTrk(StPrimaryMaker*, int*, float*, CtbResponse*,St_dst_track * );
  vector <Jtrk> tracks[MxTimeSlot];
  void * GVER[MxTimeSlot];
  int getTrigBXing(){ return (int)(30./bXingTimeSep -firstBXing);}
  int getPileupBXing();
};

#endif
