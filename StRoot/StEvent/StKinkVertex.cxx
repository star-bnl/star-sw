/***************************************************************************
 *
 * $Id: StKinkVertex.cxx,v 2.4 1999/12/21 15:08:57 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StKinkVertex.cxx,v $
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
#include "StKinkVertex.h"
#include "StParticleTable.hh"
#include "StTrack.h"
#include "tables/St_dst_tkf_vertex_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

ClassImp(StKinkVertex)

static const char rcsid[] = "$Id: StKinkVertex.cxx,v 2.4 1999/12/21 15:08:57 ullrich Exp $";

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

StKinkVertex::StKinkVertex(const dst_vertex_st& vtx, const dst_tkf_vertex_st& kvtx)
    : StVertex(vtx), mParentMomentum(kvtx.p), mDaughterMomentum(kvtx.pd)
{
    mType = kKinkVtxId;
    mDaughter = 0;
    mParentGeantId = kvtx.pidp;
    mDaughterGeantId = kvtx.pidd;
    mDcaParentDaughter = kvtx.dca;
    mDcaDaughterPrimaryVertex = kvtx.dcad;
    mDcaParentPrimaryVertex = kvtx.dcap;
    mHitDistanceParentDaughter = kvtx.dlf;
    mHitDistanceParentVertex = kvtx.dlv;
    copy(kvtx.dE+0, kvtx.dE+3, mDeltaEnergy);
    mDecayAngle = kvtx.theta;
    mDecayAngleCM = kvtx.theta_cm;
}

StKinkVertex::~StKinkVertex() {/* noop */}

StObject*
StKinkVertex::clone() { return new StKinkVertex(*this); }

StVertexId
StKinkVertex::type() const { return kKinkVtxId; }

UInt_t
StKinkVertex::numberOfDaughters() const { return mDaughter ? 1 : 0; }

StTrack*
StKinkVertex::daughter(UInt_t i)
{
    return i==0 ? mDaughter : 0;
}

const StTrack*
StKinkVertex::daughter(UInt_t i) const
{
    return i==0 ? mDaughter : 0;
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
    
UShort_t
StKinkVertex::geantIdParent() const { return mParentGeantId; }

UShort_t
StKinkVertex::geantIdDaughter() const { return mDaughterGeantId; }

Float_t
StKinkVertex::dcaParentDaughter() const { return mDcaParentDaughter; }

Float_t
StKinkVertex::dcaDaughterPrimaryVertex() const { return mDcaDaughterPrimaryVertex; }
   
Float_t
StKinkVertex::dcaParentPrimaryVertex() const { return mDcaParentPrimaryVertex; }

Float_t
StKinkVertex::hitDistanceParentDaughter() const { return mHitDistanceParentDaughter; }

Float_t
StKinkVertex::hitDistanceParentVertex() const { return mHitDistanceParentVertex; }

Float_t
StKinkVertex::dE(UInt_t i) const
{
    if (i < 3)
        return mDeltaEnergy[i];
    else
        return 0;
}

Float_t
StKinkVertex::decayAngle() const { return mDecayAngle; }

Float_t
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
StKinkVertex::setGeantIdParent(UShort_t val) { mParentGeantId = val; }

void
StKinkVertex::setGeantIdDaughter(UShort_t val) { mDaughterGeantId = val; }

void
StKinkVertex::setDcaParentDaughter(Float_t val) { mDcaParentDaughter = val; }

void
StKinkVertex::setDcaDaughterPrimaryVertex(Float_t val) { mDcaDaughterPrimaryVertex = val; }

void
StKinkVertex::setDcaParentPrimaryVertex(Float_t val) { mDcaParentPrimaryVertex = val; }

void
StKinkVertex::setHitDistanceParentDaughter(Float_t val) { mHitDistanceParentDaughter = val; }

void
StKinkVertex::setHitDistanceParentVertex(Float_t val) { mHitDistanceParentVertex = val; }

void
StKinkVertex::setdE(UInt_t i, Float_t val)
{
    if (i < 3) mDeltaEnergy[i] = val;
}

void
StKinkVertex::setDecayAngle(Float_t val) { mDecayAngle = val; }

void
StKinkVertex::setDecayAngleCM(Float_t val) { mDecayAngleCM = val; }

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
