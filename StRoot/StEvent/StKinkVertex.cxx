/***************************************************************************
 *
 * $Id: StKinkVertex.cxx,v 2.10 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StKinkVertex.cxx,v $
 * Revision 2.10  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.9  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2001/07/13 16:25:20  perev
 * last static array fixed
 *
 * Revision 2.7  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.6  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:34:51  perev
 * clone() -> clone() const
 *
 * Revision 2.4  1999/12/21 15:08:57  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.3  1999/10/28 22:25:53  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.2  1999/10/14 11:07:57  ullrich
 * Fixed geantIdDaughter(). Was returning parent not daughter Id.
 *
 * Revision 2.1  1999/10/13 19:44:51  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "TClass.h"
#include "TFile.h"
#include "StKinkVertex.h"
#include "StParticleTable.hh"
#include "StTrack.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

ClassImp(StKinkVertex)

static const char rcsid[] = "$Id: StKinkVertex.cxx,v 2.10 2009/11/23 16:34:06 fisyak Exp $";

StKinkVertex::StKinkVertex()
{
    mType = kKinkVtxId;
    mDaughter = 0;
    mParentGeantId = 0;
    mDaughterGeantId = 0;
    mDcaParentDaughter = 0;
    mDcaDaughterPrimaryVertex = 0;
    mDcaParentPrimaryVertex = 0;
    mHitDistanceParentDaughter = 0;
    mHitDistanceParentVertex = 0;
    fill_n(mDeltaEnergy, 3, 0);
    mDecayAngle = 0;
    mDecayAngleCM = 0;
}

StKinkVertex::~StKinkVertex() {/* noop */}

StVertexId
StKinkVertex::type() const { return kKinkVtxId; }

unsigned int
StKinkVertex::numberOfDaughters() const { return mDaughter ? 1 : 0; }

StTrack*
StKinkVertex::daughter(unsigned int i)
{
    return i==0 ? (StTrack*)mDaughter : 0;
}

const StTrack*
StKinkVertex::daughter(unsigned int i) const
{
    return i==0 ? (const StTrack*)mDaughter : 0;
}

StPtrVecTrack
StKinkVertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    if (filter(mDaughter)) vec.push_back(mDaughter);
    return vec;
}

StParticleDefinition*
StKinkVertex::pidParent() const
{
    return StParticleTable::instance()->findParticleByGeantId(mParentGeantId);
}

StParticleDefinition*
StKinkVertex::pidDaughter() const
{
    return StParticleTable::instance()->findParticleByGeantId(mDaughterGeantId);
}
    
unsigned short
StKinkVertex::geantIdParent() const { return mParentGeantId; }

unsigned short
StKinkVertex::geantIdDaughter() const { return mDaughterGeantId; }

float
StKinkVertex::dcaParentDaughter() const { return mDcaParentDaughter; }

float
StKinkVertex::dcaDaughterPrimaryVertex() const { return mDcaDaughterPrimaryVertex; }
   
float
StKinkVertex::dcaParentPrimaryVertex() const { return mDcaParentPrimaryVertex; }

float
StKinkVertex::hitDistanceParentDaughter() const { return mHitDistanceParentDaughter; }

float
StKinkVertex::hitDistanceParentVertex() const { return mHitDistanceParentVertex; }

float
StKinkVertex::dE(unsigned int i) const
{
    if (i < 3)
        return mDeltaEnergy[i];
    else
        return 0;
}

float
StKinkVertex::decayAngle() const { return mDecayAngle; }

float
StKinkVertex::decayAngleCM() const { return mDecayAngleCM; }

const StThreeVectorF&
StKinkVertex::parentMomentum() const { return mParentMomentum; }

StThreeVectorF&
StKinkVertex::parentMomentum()  { return mParentMomentum; }

const StThreeVectorF&
StKinkVertex::daughterMomentum() const { return mDaughterMomentum; }

StThreeVectorF&
StKinkVertex::daughterMomentum() { return mDaughterMomentum; }

void
StKinkVertex::setGeantIdParent(unsigned short val) { mParentGeantId = val; }

void
StKinkVertex::setGeantIdDaughter(unsigned short val) { mDaughterGeantId = val; }

void
StKinkVertex::setDcaParentDaughter(float val) { mDcaParentDaughter = val; }

void
StKinkVertex::setDcaDaughterPrimaryVertex(float val) { mDcaDaughterPrimaryVertex = val; }

void
StKinkVertex::setDcaParentPrimaryVertex(float val) { mDcaParentPrimaryVertex = val; }

void
StKinkVertex::setHitDistanceParentDaughter(float val) { mHitDistanceParentDaughter = val; }

void
StKinkVertex::setHitDistanceParentVertex(float val) { mHitDistanceParentVertex = val; }

void
StKinkVertex::setdE(unsigned int i, float val)
{
    if (i < 3) mDeltaEnergy[i] = val;
}

void
StKinkVertex::setDecayAngle(float val) { mDecayAngle = val; }

void
StKinkVertex::setDecayAngleCM(float val) { mDecayAngleCM = val; }

void
StKinkVertex::setParentMomentum(const StThreeVectorF& val) { mParentMomentum = val; }

void
StKinkVertex::setDaughterMomentum(const StThreeVectorF& val) { mDaughterMomentum = val; }

void
StKinkVertex::addDaughter(StTrack* val)
{
    if (val) mDaughter = val;
}

void
StKinkVertex::removeDaughter(StTrack* val)
{
    if (mDaughter == val) mDaughter = 0;
}

void StKinkVertex::Streamer(TBuffer &R__b)
{
    // Stream an object of class StKinkVertex.

    if (R__b.IsReading()) {
       UInt_t R__s, R__c;
       Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
       if (R__v > 1) {
          Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
          return;
       }
       //====process old versions before automatic schema evolution
       StVertex::Streamer(R__b);
//     R__b >> mDaughter;
       R__b >> (StTrack*&)mDaughter;

       R__b >> mParentGeantId;
       R__b >> mDaughterGeantId;
       R__b >> mDcaParentDaughter;
       R__b >> mDcaDaughterPrimaryVertex;
       R__b >> mDcaParentPrimaryVertex;
       R__b >> mHitDistanceParentDaughter;
       R__b >> mHitDistanceParentVertex;

       Int_t dumy;
       if (gFile && gFile->GetVersion() < 30000) {R__b >> dumy;}

       R__b.ReadFastArray(mDeltaEnergy,3);
       R__b >> mDecayAngle;
       R__b >> mDecayAngleCM;
       mParentMomentum.Streamer(R__b);
       mDaughterMomentum.Streamer(R__b);
       R__b.CheckByteCount(R__s, R__c, Class());
       //====end of old versions
      
    } else {
       Class()->WriteBuffer(R__b,this);
    }
} 
