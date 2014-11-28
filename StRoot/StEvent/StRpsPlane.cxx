/***************************************************************************
 *
 * $Id: StRpsPlane.cxx,v 2.1 2009/11/23 22:18:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:  Reconstructed cluster in the Roman Pot Silicon 
 *               detectors.         
 *
 ***************************************************************************
 *
 * $Log: StRpsPlane.cxx,v $
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRpsPlane.h"
#include "StRpsCluster.h"

static const char rcsid[] = "$Id: StRpsPlane.cxx,v 2.1 2009/11/23 22:18:25 ullrich Exp $";

ClassImp(StRpsPlane)

StRpsPlane::StRpsPlane()
{
    mOffset = mZ = mAngle = 0;
    mOrientation = 0;
    mStatus = 0;
    mRomanPotId = mPlaneId = 0;
}

StRpsPlane::~StRpsPlane() { /* noop */ }

unsigned int 
StRpsPlane::planeId() const { return mPlaneId; }

double 
StRpsPlane::offset() const { return mOffset; }

double 
StRpsPlane::z() const { return mZ; }

double 
StRpsPlane::angle() const { return mAngle; } 

short  
StRpsPlane::orientation() const { return mOrientation; } 

unsigned char 
StRpsPlane::status() const { return mStatus; } 

unsigned int  
StRpsPlane::numberOfClusters() const { return mClusters.size(); }

const StRpsCluster* 
StRpsPlane::cluster(unsigned int i) const
{
    if (i < mClusters.size())
        return mClusters[i];
    else
        return 0;
}

StRpsCluster* 
StRpsPlane::cluster(unsigned int i)
{
    if (i < mClusters.size())
        return mClusters[i];
    else
        return 0;
}

unsigned int  
StRpsPlane::romanPotId() const { return mRomanPotId; }

const StSPtrVecRpsCluster& 
StRpsPlane::clusters() const { return mClusters; }

StSPtrVecRpsCluster&       
StRpsPlane::clusters() { return mClusters; }

void 
StRpsPlane::addCluster(StRpsCluster* cluster) 
{
    if (cluster) {
        mClusters.push_back(cluster);
        cluster->setPlaneId(mPlaneId);
        cluster->setRomanPotId(mRomanPotId);
    }
}

void 
StRpsPlane::setOffset(double val) { mOffset = val; }

void 
StRpsPlane::setZ(double val) { mZ = val; }

void 
StRpsPlane::setAngle(double val) { mAngle = val; }

void 
StRpsPlane::setOrientation(short val) { mOrientation = val; }

void 
StRpsPlane::setStatus(unsigned char val) { mStatus = val; }

void 
StRpsPlane::setPlaneId(unsigned char i) { mPlaneId = i; }

void 
StRpsPlane::setRomanPotId(unsigned char i) { mRomanPotId = i; }

