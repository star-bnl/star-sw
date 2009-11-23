/***************************************************************************
 *
 * $Id: StRpsRomanPot.cxx,v 2.1 2009/11/23 22:18:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsRomanPot.cxx,v $
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRpsRomanPot.h"
#include "StRpsCollection.h"
#include "StRpsCluster.h"

static const char rcsid[] = "$Id: StRpsRomanPot.cxx,v 2.1 2009/11/23 22:18:25 ullrich Exp $";

ClassImp(StRpsRomanPot)

StRpsRomanPot::StRpsRomanPot()
{
    mStatus = 0;
    mAdc[0] = mAdc[1] = mTac[0] = mTac[1] = 0;
    // Roman Pot ID gets set in 
    // StRpsCollection constructor    
}

StRpsRomanPot::~StRpsRomanPot() { /* noop */ }

unsigned int 
StRpsRomanPot::romanPotId() const
{
    return mRomanPotId;
}

unsigned int 
StRpsRomanPot::numberOfPlanes() const
{
    return mNumberOfPlanes;
}

unsigned int 
StRpsRomanPot::numberOfPlanesWithClusters() const
{
    unsigned int count = 0;
    for (int i=0; i<mNumberOfPlanes; i++)  
        count += mPlanes[i].numberOfClusters();
    return count;
}

unsigned int 
StRpsRomanPot::adc(unsigned int i) const
{
    return (i < 2 ? mAdc[i] : 0);
}

unsigned int 
StRpsRomanPot::tac(unsigned int i) const
{
    return (i < 2 ? mTac[i] : 0);
}

unsigned char 
StRpsRomanPot::status() const
{
    return mStatus;
}

const StRpsPlane* 
StRpsRomanPot::plane(unsigned int i) const
{
    if (i < mNumberOfPlanes)
        return &mPlanes[i];
    else
        return 0;
}

StRpsPlane* StRpsRomanPot::plane(unsigned int i)
{
    if (i < mNumberOfPlanes)
        return &mPlanes[i];
    else
        return 0;
}

void 
StRpsRomanPot::setStatus(unsigned char val)
{
    mStatus = val;
}

void StRpsRomanPot::setRomanPotId(unsigned char i) 
{
    mRomanPotId = i;
}

void StRpsRomanPot::setAdc(unsigned int adc0, unsigned int adc1) 
{
    mAdc[0] = adc0;
    mAdc[1] = adc1;
}

void StRpsRomanPot::setTac(unsigned int tac0, unsigned int tac1) 
{
    mTac[0] = tac0;
    mTac[1] = tac1;
}

