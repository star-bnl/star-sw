/***************************************************************************
 *
 * StMcTpcHit.hh
 *
 **************************************************************************/
#ifndef StMcTpcHit_hh
#define StMcTpcHit_hh

#include "StMcEvent/StMcHit.hh"
#include "tables/g2t_tpc_hit.h"  // Do we need a different table than default?

class StMcTrack;
class StThreeVectorF;
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcTpcHit : public StMcHit {
public:
    StMcTpcHit(const StThreeVectorF&,
	     const float, const float, StMcTrack*);
    StMcTpcHit(g2t_tpc_hit_st*);
    


// Here we need to include special functions specific to the TPC,
// like going to local coordinates.

};


#endif
