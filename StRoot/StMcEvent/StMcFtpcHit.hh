/***************************************************************************
 *
 * $Id: StMcFtpcHit.hh,v 1.3 1999/09/23 21:25:51 calderon Exp $
 * $Log: StMcFtpcHit.hh,v $
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcFtpcHit_hh
#define StMcFtpcHit_hh

#include "StMcHit.hh"
#include "tables/g2t_ftp_hit.h" 

class StMcTrack;
class StThreeVectorF;

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StMcFtpcHit : public StMcHit {
public:
    StMcFtpcHit(const StThreeVectorF&,
	     const float, const float, StMcTrack*);
    StMcFtpcHit(g2t_ftp_hit_st*);
    


// Here we need to include special functions specific to the FTPC,
// like going to local coordinates.

};

#endif
