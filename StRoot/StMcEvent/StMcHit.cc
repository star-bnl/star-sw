/***************************************************************************
 *
 * $Id: StMcHit.cc,v 2.6 2000/06/06 02:58:41 calderon Exp $
 * $Log: StMcHit.cc,v $
 * Revision 2.6  2000/06/06 02:58:41  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.5  2000/05/05 15:25:43  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.4  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.3  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.2  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.1  1999/11/19 19:06:32  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StMcHit.hh"

#include "tables/St_g2t_hits_Table.h"

static const char rcsid[] = "$Id: StMcHit.cc,v 2.6 2000/06/06 02:58:41 calderon Exp $";

StMcHit::StMcHit()
    : mPosition(0.,0.,0.), mdE(0),mdS(0),mParentTrack(0)
{ /* noop */   }

StMcHit::StMcHit(const StThreeVectorF& x,const StThreeVectorF& p,
		 float de, float ds, long k, long volId, StMcTrack* parent)
    : mPosition(x), mLocalMomentum(p),
      mdE(de), mdS(ds), mKey(k), mVolumeId(volId),
      mParentTrack(parent)
{ /* noop */ }

StMcHit::StMcHit(g2t_hits_st* pt)
{
  mdE = pt->de;
  mdS = pt->ds;
  mKey = pt->id;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  mLocalMomentum.setX(pt->p[0]); 
  mLocalMomentum.setY(pt->p[1]);
  mLocalMomentum.setZ(pt->p[2]);
  mParentTrack = 0;
  // For parent track, the g2t_hits table only gives the id of 
  // the parent track :  pt->track_p  .  We need to decode this and assign
  // mParentTrack to the pointer to the parent track.
}

StMcHit::~StMcHit() { /* noop */ }
    
int StMcHit::operator==(const StMcHit& h) const
{
    return h.mKey == mKey && h.mPosition == mPosition &&
           h.mdE   == mdE && h.mdS == mdS ;
}

int StMcHit::operator!=(const StMcHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StMcHit::setPosition(const StThreeVectorF& val) { mPosition = val; }

void StMcHit::setLocalMomentum(const StThreeVectorF& val) { mLocalMomentum = val; }

void StMcHit::setdE(float val) { mdE = val; }

void StMcHit::setdS(float val) { mdS = val; }

void StMcHit::setKey(long val) { mKey = val; }

void StMcHit::setVolumeId(long val) { mVolumeId = val; }

void StMcHit::setParentTrack(StMcTrack* val) { mParentTrack = val; }
    
ostream& operator<<(ostream& os, const StMcHit& h)
{
    os << "Id: " << h.key() << endl;
    os << "Position: " << h.position() << endl;
    os << "Local Momentum: " << h.localMomentum() << endl; 
    os << "dE: " << h.dE() << endl;
    os << "dS: " << h.dS() << endl;
    os << "Vol Id: " << h.volumeId() << endl;
    return os;
}
