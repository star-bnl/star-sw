/***************************************************************************
 *
 * $Id: StMcRichHit.cc,v 2.2 2000/04/18 00:55:14 calderon Exp $
 * $Log: StMcRichHit.cc,v $
 * Revision 2.2  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.1  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 *
 **************************************************************************/
#include "StMcRichHit.hh"
#include "StMcTrack.hh"
#include "tables/St_g2t_rch_hit_Table.h"

static const char rcsid[] = "$Id: StMcRichHit.cc,v 2.2 2000/04/18 00:55:14 calderon Exp $";

StMemoryPool StMcRichHit::mPool(sizeof(StMcRichHit));

StMcRichHit::StMcRichHit() { /* noop */ };

StMcRichHit::StMcRichHit(const StThreeVectorF& p,
		       const float de, const float ds,
		       StMcTrack* parent)  : StMcHit(p, de, ds, parent)
{ /* noop */ }


StMcRichHit::StMcRichHit(g2t_rch_hit_st* pt)
{
  mdE = pt->de;
  //mdS = pt->ds;
  mdS = 0;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  mLocalMomentum.setX(pt->p[0]);
  mLocalMomentum.setY(pt->p[1]);
  mLocalMomentum.setZ(pt->p[2]);
  mVolumeId = pt->volume_id;
  mTof      = pt->tof;
}

StMcRichHit::~StMcRichHit() {/* noop */}

ostream&  operator<<(ostream& os, const StMcRichHit& h)
{
    os << "Position       : " << h.position() << endl; 
    os << "Local Momentum : " << h.localMomentum()    << endl;
    os << "Pad            : " << h.pad()    << endl;
    os << "Row            : " << h.row()   << endl;
    os << "T. of Flight   : " << h.tof()   << endl;
    return os;
}
