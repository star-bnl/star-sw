/***************************************************************************
 *
 * $Id: StMcTpcHit.hh,v 2.0 1999/11/17 02:12:16 calderon Exp $
 * $Log: StMcTpcHit.hh,v $
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcTpcHit_hh
#define StMcTpcHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_tpc_hit_Table.h"  // Do we need a different table than default?

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
