/***************************************************************************
 *
 * StMcHit.cc
 *
 **************************************************************************/

#include "StMcEvent/StMcHit.hh"
#include "StMcEvent/StMcTrack.hh"


static const char rcsid[] = "$Id: StMcHit.cc,v 1.1.1.1 1999/07/13 18:10:12 uid2620 Exp $";

StMcHit::StMcHit()
{
    mdE = 0;
    mdS = 0;
    mParentTrack = 0;
    
}

StMcHit::StMcHit(const StThreeVector<float>& p,
		 float de, float ds, StMcTrack* parent)
    : mPosition(p), mdE(de), mdS(ds), mParentTrack(parent)
{ /* noop */ }

StMcHit::StMcHit(g2t_hits_st* pt)
{
  mdE = pt->de;
  mdS = pt->ds;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  // For parent track, the g2t_hits table only gives the id of 
  // the parent track :  pt->track_p  .  We need to decode this and assign
  // mParentTrack to the pointer to the parent track.
}

StMcHit::~StMcHit() { /* noop */ }
    
int StMcHit::operator==(const StMcHit& h) const
{
    return h.mPosition == mPosition &&
           h.mdE   == mdE && h.mdS == mdS ;
}

int StMcHit::operator!=(const StMcHit& h) const
{
    return !(*this == h);  // use operator==()
}

void StMcHit::setPosition(const StThreeVector<float>& val) { mPosition = val; }

void StMcHit::setdE(float val) { mdE = val; }

void StMcHit::setdS(float val) { mdS = val; }

void StMcHit::setParentTrack(StMcTrack* val) { mParentTrack = val; }
    
ostream& operator<<(ostream& os, const StMcHit& h)
{
    os << "Position: " << h.position() << endl;
    os << "dE: " << h.dE() << endl;
    os << "dS: " << h.dS() << endl;
    return os;
}
