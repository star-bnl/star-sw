/***************************************************************************
 *
 * $Id: StMcFtpcHit.hh,v 2.0 1999/11/17 02:12:16 calderon Exp $
 * $Log: StMcFtpcHit.hh,v $
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:16  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcFtpcHit_hh
#define StMcFtpcHit_hh

#include "StMcHit.hh"
#include "tables/St_g2t_ftp_hit_Table.h" 

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
