/***************************************************************************
 *
 * StMcFtpcHit.hh
 *
 **************************************************************************/
#ifndef StMcFtpcHit_hh
#define StMcFtpcHit_hh

#include "StMcEvent/StMcHit.hh"
#include "tables/g2t_ftp_hit.h" 

class StMcTrack;

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcFtpcHit : public StMcHit {
public:
    StMcFtpcHit(const StThreeVector<float>&,
	     const float, const float, StMcTrack*);
    StMcFtpcHit(g2t_ftp_hit_st*);
    


// Here we need to include special functions specific to the FTPC,
// like going to local coordinates.

};

#endif
