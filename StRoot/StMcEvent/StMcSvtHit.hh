/***************************************************************************
 *
 * StMcSvtHit.hh
 *
 **************************************************************************/
#ifndef StMcSvtHit_hh
#define StMcSvtHit_hh

#include "StMcEvent/StMcHit.hh"
#include "tables/g2t_svt_hit.h"

class StMcTrack;
class StThreeVectorF;
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcSvtHit : public StMcHit {
public:
    StMcSvtHit(const StThreeVectorF&,
	     const float, const float,
	     StMcTrack*);
    StMcSvtHit(g2t_svt_hit_st*);
    

};

// Here we need to include special functions specific to the SVT,
// like going to local coordinates.

#endif
