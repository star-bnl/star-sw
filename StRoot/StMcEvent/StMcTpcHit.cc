/***************************************************************************
 *
 * $Id: StMcTpcHit.cc,v 2.5 2000/04/18 00:55:14 calderon Exp $
 * $Log: StMcTpcHit.cc,v $
 * Revision 2.5  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.4  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:33  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:53  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StMcTpcHit.hh"
#include "StMcTrack.hh"
#include "tables/St_g2t_tpc_hit_Table.h"  

static const char rcsid[] = "$Id: StMcTpcHit.cc,v 2.5 2000/04/18 00:55:14 calderon Exp $";

StMemoryPool StMcTpcHit::mPool(sizeof(StMcTpcHit));

StMcTpcHit::StMcTpcHit() { /* noop */ }

StMcTpcHit::StMcTpcHit(const StThreeVectorF& p,
		       const float de, const float ds,
		       StMcTrack* parent)  : StMcHit(p, de, ds, parent)
{ /* noop */ }

StMcTpcHit::StMcTpcHit(g2t_tpc_hit_st* pt)
{
  mdE = pt->de;
  mdS = pt->ds;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  mLocalMomentum.setX(pt->p[0]); 
  mLocalMomentum.setY(pt->p[1]);
  mLocalMomentum.setZ(pt->p[2]);
  mVolumeId = pt->volume_id;
  // The Local and Pad coordinates will be filled in the maker,
  // since they use the coordinate transforms.
  
}

StMcTpcHit::~StMcTpcHit() {/* noop */}

ostream&  operator<<(ostream& os, const StMcTpcHit& h)
{
    os << "Position      : " << h.position() << endl; 
    os << "Local Momentum: " << h.localMomentum() << endl; 
    os << "Sector        : " << h.sector()     << endl;
    os << "Pad Row       : " << h.padrow()     << endl;
    return os;
}



