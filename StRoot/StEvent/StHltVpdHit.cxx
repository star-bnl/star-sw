/***************************************************************************
 *
 * $Id: StHltVpdHit.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltVpdHit.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltVpdHit.h"
#include "StHltTrackNode.h"

ClassImp(StHltVpdHit)

StHltVpdHit::StHltVpdHit()
{
	mDirection = east;
	mChannel = 0;       ///< total 19 channels for each side (east/west)
	mTdc = 0;
	mTot = 0;
	mTof = 0;
	mTriggerTime = 0;
}

StHltVpdHit::~StHltVpdHit(){/* noop */}

void
StHltVpdHit::setDirection(StBeamDirection val)
{
	mDirection = val;
}

void
StHltVpdHit::setChannel(short val)
{
	mChannel = val;
}

void
StHltVpdHit::setTdc(float val)
{
	mTdc = val;
}

void
StHltVpdHit::setTot(float val)
{
	mTot = val;
}

void
StHltVpdHit::setTof(float val)
{
	mTof = val;
}

void
StHltVpdHit::setTriggerTime(float val)
{
	mTriggerTime = val;
}

ostream&
operator<<(ostream &os, const StHltVpdHit& hit)
{
	os << " direction "<<hit.direction()<<" channel "<<hit.channel()
    << " module "<<hit.module()<<" cell "<<hit.cell()<<endl
    << " tdc "<<hit.tdc()<<" tot "<<hit.tot()<<" tof "<<hit.tof()<<" triggerTime "<<hit.triggerTime()<<endl;
	return os;
}

