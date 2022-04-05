/***************************************************************************
 *
 * $Id: StRpsCluster.cxx,v 2.2 2015/10/02 19:50:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsCluster.cxx,v $
 * Revision 2.2  2015/10/02 19:50:09  ullrich
 * Added mPositionRMS and accessors.
 *
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRpsCluster.h"

static const char rcsid[] = "$Id: StRpsCluster.cxx,v 2.2 2015/10/02 19:50:09 ullrich Exp $";

ClassImp(StRpsCluster)

StRpsCluster::StRpsCluster()
{
    mPosition = mEnergy = mXY = 0;
    mLength = 0;
    mQuality = 0;
    mPlaneId = mRomanPotId = 0;
}

StRpsCluster::StRpsCluster(double pos, double posRMS, short len,
                           double e, double xy, unsigned char qual)
{
    mPosition = pos;
    mPositionRMS = posRMS;
    mLength = len;
    mEnergy = e;
    mXY = xy;
    mQuality = qual;
    mPlaneId = mRomanPotId = 0;
    // mPlaneId and mRomanPotId are later set by StRpsPlane::addCluster()
}

StRpsCluster::~StRpsCluster() { /* noop */ };

double 
StRpsCluster::position() const { return mPosition; }

double 
StRpsCluster::positionRMS() const { return mPositionRMS; }

short  
StRpsCluster::length() const { return mLength; }

double 
StRpsCluster::energy() const { return mEnergy; }

double 
StRpsCluster::xy() const { return mXY; }

unsigned char 
StRpsCluster::quality() const { return mQuality; }

unsigned int 
StRpsCluster::romanPotId() const { return mRomanPotId; }

unsigned int 
StRpsCluster::planeId() const { return mPlaneId; }

void 
StRpsCluster::setPosition(double val) { mPosition = val; }

void 
StRpsCluster::setPositionRMS(double val) { mPositionRMS = val; }

void 
StRpsCluster::setLength(short val) { mLength = val; }

void 
StRpsCluster::setEnergy(double val) { mEnergy = val; }

void 
StRpsCluster::setXY(double val) { mXY = val; }

void 
StRpsCluster::setQuality(unsigned char val) { mQuality = val; }

void 
StRpsCluster::setPlaneId(unsigned char val) { mPlaneId = val; }

void 
StRpsCluster::setRomanPotId(unsigned char val) { mRomanPotId = val; }

// 
// Non class methods
//
ostream& operator<<(ostream& os, const StRpsCluster& cluster)
{
    os << "position = " << cluster.position() << endl;
    os << "position rms = " << cluster.positionRMS() << endl;
    os << "length = " << cluster.length() << endl;
    os << "energy = " << cluster.energy() << endl;
    os << "xy = " << cluster.xy() << endl;
    os << "quality = " << static_cast<unsigned int>(cluster.quality()) << endl;
    os << "roman pot id = " << cluster.romanPotId() << endl;
    os << "plane id = " << cluster.planeId() << endl;
    return os;
}
