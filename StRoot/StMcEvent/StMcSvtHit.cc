/***************************************************************************
 *
 * $Id: StMcSvtHit.cc,v 2.2 1999/12/03 00:51:52 calderon Exp $
 * $Log: StMcSvtHit.cc,v $
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
 * Revision 1.3  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StMcSvtHit.hh"
#include "StMcTrack.hh"
#include "tables/St_g2t_svt_hit_Table.h"

static const char rcsid[] = "$Id: StMcSvtHit.cc,v 2.2 1999/12/03 00:51:52 calderon Exp $";

StMemoryPool StMcSvtHit::mPool(sizeof(StMcSvtHit));

StMcSvtHit::StMcSvtHit() { /* noop */ };

StMcSvtHit::StMcSvtHit(const StThreeVectorF& p,
		       const float de, const float ds,
		       StMcTrack* parent)  : StMcHit(p, de, ds, parent)
{ /* noop */ }


StMcSvtHit::StMcSvtHit(g2t_svt_hit_st* pt)
{
  mdE = pt->de;
  mdS = pt->ds;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  mVolumeId = pt->volume_id;
}

StMcSvtHit::~StMcSvtHit() {/* noop */}

ostream&  operator<<(ostream& os, const StMcSvtHit& h)
{
    os << "Position      : " << h.position() << endl; 
    os << "Layer         : " << h.layer()    << endl;
    os << "Ladder        : " << h.ladder()   << endl;
    os << "Wafer         : " << h.wafer()    << endl;
    os << "Barrel        : " << h.barrel()   << endl;
    return os;
}
