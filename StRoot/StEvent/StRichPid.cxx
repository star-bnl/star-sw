/***************************************************************************
 *
 * $Id: StRichPid.cxx,v 2.6 2001/04/05 04:00:53 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description: Definition of PID object
 *
 ***************************************************************************
 *
 * $Log: StRichPid.cxx,v $
 * Revision 2.6  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2000/12/08 03:50:31  ullrich
 * Removed compiler warning (signed/unsigned comparison).
 *
 * Revision 2.4  2000/11/25 11:51:49  lasiuk
 * remove D vector and replace with a container of StRichPhotonInfo
 *
 * Revision 2.3  2000/11/21 19:47:33  lasiuk
 * add the d information for each hit
 * use the TArrayF
 *
 * Revision 2.2  2000/11/01 16:45:43  lasiuk
 * Keep the pointers to the hits that are associated with the track
 * in order to use the bit flag information.  These are kept
 * in an StPtrVec (does not own the hits)  The PDG encoded number
 * is kept as a data member now
 *
 * Revision 2.1  2000/09/28 10:53:56  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "StRichPid.h"

static const char rcsid[] = "$Id: StRichPid.cxx,v 2.6 2001/04/05 04:00:53 ullrich Exp $";

ClassImp(StRichPid)

StRichPid::StRichPid()
    : mParticleNumber(0),
      mTotalAzimuth(0), mTotalArea(0), mTotalHits(0), mTotalDensity(0),
      mTruncatedAzimuth(0), mTruncatedArea(0), mTruncatedHits(0), mTruncatedDensity(0) {/* noop */}

StRichPid::StRichPid(StParticleDefinition* type, StThreeVectorD resid, float totAzim,
                     float totArea, unsigned short totHits, float trunAzim, float trunArea,
                     unsigned short trunHits)
    : mMipResidual(resid), mTotalAzimuth(totAzim), mTotalArea(totArea),
      mTotalHits(totHits), mTruncatedAzimuth(trunAzim), mTruncatedArea(trunArea),
      mTruncatedHits(trunHits) {

    mParticleNumber = type->pdgEncoding();
    
    if (mTotalArea>0) {mTotalDensity=mTotalHits/mTotalArea;}
    else mTotalDensity=0;
    
    if (mTruncatedArea>0) {mTruncatedDensity=mTruncatedHits/mTruncatedArea;}
    else mTruncatedDensity=0;
}

StRichPid::~StRichPid() {/* noop */}

void StRichPid::setRingType(StParticleDefinition* t)   {
    mParticleType=t;
    mParticleNumber = t->pdgEncoding();
}

const StSPtrVecRichPhotonInfo& StRichPid::getPhotonInfo() {return mPhotonInfo;}

StRichPhotonInfo* StRichPid::getPhotonInfo(int i)
{
    if(static_cast<unsigned int>(i) > mPhotonInfo.size())
        return 0;
    else
        return mPhotonInfo[i];
}


void StRichPid::addPhotonInfo(StRichPhotonInfo* i)
{
    mPhotonInfo.push_back(i);
}

int
StRichPid::operator==(const StRichPid& pid) const {
    return ( mParticleNumber   == pid.getParticleNumber() &&
             mMipResidual      == pid.getMipResidual() &&
             mTotalAzimuth     == pid.getTotalAzimuth() &&
             mTotalArea        == pid.getTotalArea() &&
             mTotalHits        == pid.getTotalHits() &&
             mTotalDensity     == pid.getTotalDensity() &&
             mTruncatedAzimuth == pid.getTruncatedAzimuth() &&
             mTruncatedArea    == pid.getTruncatedArea() &&
             mTruncatedHits    == pid.getTruncatedHits() &&
             mTruncatedDensity == pid.getTruncatedDensity() );
}
