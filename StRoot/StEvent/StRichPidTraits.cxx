/***************************************************************************
 *
 * $Id: StRichPidTraits.cxx,v 2.7 2002/02/19 23:21:30 ullrich Exp $
 *
 * Author: Matt Horsley, Sep 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPidTraits.cxx,v $
 * Revision 2.7  2002/02/19 23:21:30  ullrich
 * Added copy constructor and assignment operator. New destructor.
 *
 * Revision 2.6  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.5  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/02/22 21:05:00  lasiuk
 * add production version, associated MIP, dca residual 3Vectors
 *
 * Revision 2.3  2000/11/25 11:53:36  lasiuk
 * initialize data members in c'tor
 *
 * Revision 2.2  2000/11/01 16:46:59  lasiuk
 * Keep the StRichPid as the owner (use a StSPtrVec)
 * also check the pdg encoded number now
 *
 * Revision 2.1  2000/09/28 10:54:46  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "TClass.h"
#include "StRichPidTraits.h"

static const char rcsid[] = "$Id: StRichPidTraits.cxx,v 2.7 2002/02/19 23:21:30 ullrich Exp $";

ClassImp(StRichPidTraits)

StRichPidTraits::StRichPidTraits()
    : StTrackPidTraits(kRichId), mId(0), mProbability(0) {
    mThePids.clear();
    mThePids.resize(0);
    mRichSpectra = 0;
}

StRichPidTraits::~StRichPidTraits() {delete mRichSpectra;}

StRichPidTraits::StRichPidTraits(const StRichPidTraits& t)
{
    mRichSpectra = 0;
    *this = t;
}

StRichPidTraits&
StRichPidTraits::operator=(const StRichPidTraits& t)
{
    if (this != &t) {
	mThePids           = t.mThePids;
	mProductionVersion = t.mProductionVersion;
	mId                = t.mId;
	mProbability       = t.mProbability;
	mAssociatedMip     = t.mAssociatedMip;     
	mMipResidual       = t.mMipResidual;
	mRefitResidual     = t.mRefitResidual;
	mSigned3dDca       = t.mSigned3dDca;
	mSigned2dDca       = t.mSigned2dDca;
	if (mRichSpectra) delete mRichSpectra;
	if (t.mRichSpectra)
	    mRichSpectra   = new  StRichSpectra(*t.mRichSpectra);
	else
	    mRichSpectra = 0;	
    }
    return *this;
}


StRichPid*
StRichPidTraits::getPid(StParticleDefinition* part) {

    for (size_t index=0;index<mThePids.size();index++) {

        if ( (mThePids[index]->getRingType()==part) ||
             (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
            return mThePids[index];
        }

    }
    return 0;
}


const StRichPid*
StRichPidTraits::getPid(StParticleDefinition* part) const {

    for (size_t index=0;index<mThePids.size();index++) {

        if ( (mThePids[index]->getRingType()==part) ||
             (mThePids[index]->getParticleNumber() == part->pdgEncoding()) ) {
            return mThePids[index];
        }
        
    }
    return 0;
}

ostream&
operator<<(ostream& os, const StRichPidTraits& t)
{
    return (os << "StrichPidTraits::> #Pids= " << t.getAllPids().size()
            << "\n\tProduction Version: " << t.productionVersion()
            << "\n\tAssociated Mip:     " << t.associatedMip()
            << "\n\tMip Residual:       " << t.mipResidual()
            << "\n\tRefit Residual:     " << t.refitResidual()
            << "\n\tid                  " << t.id()
            << "\n\tprobability         " << t.probability());
}

void
StRichPidTraits::Streamer(TBuffer &R__b)
{
    // Stream an object of class .

    if (R__b.IsReading()) {
       UInt_t R__s, R__c;
       Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
       if (R__v > 1) {
          Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
          return;
       }
       //====process old versions before automatic schema evolution
       StTrackPidTraits::Streamer(R__b);
       mThePids.Streamer(R__b);
       R__b >> mProductionVersion;
       R__b >> mId;
       R__b >> mProbability;

//     R__b >> mAssociatedMip;
       R__b >> (StRichHit*&)mAssociatedMip;

       mMipResidual.Streamer(R__b);
       mRefitResidual.Streamer(R__b);
       R__b >> mSigned3dDca;
       R__b >> mSigned2dDca;


       R__b.CheckByteCount(R__s, R__c, Class());
       //====end of old versions
      
    } else {
       Class()->WriteBuffer(R__b,this);
    }
} 

