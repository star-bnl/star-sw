/***************************************************************************
 *
 * $Id: StV0Vertex.cxx,v 2.1 1999/10/28 22:28:01 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StV0Vertex.cxx,v $
 * Revision 2.1  1999/10/28 22:28:01  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/28 22:28:01  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:23  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include "StV0Vertex.h"
#include "StTrack.h"
#include "StTrackGeometry.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_v0_vertex_Table.h"

ClassImp(StV0Vertex)

static const char rcsid[] = "$Id: StV0Vertex.cxx,v 2.1 1999/10/28 22:28:01 ullrich Exp $";

StV0Vertex::StV0Vertex()
{
    mType = kV0VtxId;
    mDaughters.resize(2);
    mDaughters[negative] = 0;
    mDaughters[positive] = 0;
    fill_n(mDcaDaughtersToPrimaryVertex, 2, 0);
    mDcaDaughters             = 0;
    mDcaParentToPrimaryVertex = 0;
}

StV0Vertex::StV0Vertex(const dst_vertex_st& vtx, const dst_v0_vertex_st& v0vtx) : StVertex(vtx)
{
    mType = kV0VtxId;
    mDaughters.resize(2);
    mDaughters[negative] = 0;
    mDaughters[positive] = 0;
    mDcaDaughtersToPrimaryVertex[negative] = v0vtx.dcan;
    mDcaDaughtersToPrimaryVertex[positive] = v0vtx.dcap;
    mMomentumOfDaughters[negative].setX(v0vtx.neg_px);
    mMomentumOfDaughters[negative].setY(v0vtx.neg_py);
    mMomentumOfDaughters[negative].setZ(v0vtx.neg_pz);
    mMomentumOfDaughters[positive].setX(v0vtx.pos_px);
    mMomentumOfDaughters[positive].setY(v0vtx.pos_py);
    mMomentumOfDaughters[positive].setZ(v0vtx.pos_pz);
    mDcaDaughters = v0vtx.dcapn;
    mDcaParentToPrimaryVertex = v0vtx.dcav0;
}

StV0Vertex::~StV0Vertex() { /* noop */ }

StObject*
StV0Vertex::clone() { return new StV0Vertex(*this); }

StVertexId
StV0Vertex::type() const { return kV0VtxId; }

UInt_t
StV0Vertex::numberOfDaughters() const { return 2; }

StTrack*
StV0Vertex::daughter(UInt_t i)
{
    return i < 2 ?  mDaughters[i] : 0;
}

const StTrack*
StV0Vertex::daughter(UInt_t i) const
{
    return i < 2 ?  mDaughters[i] : 0;
}

StPtrVecTrack
StV0Vertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    for (int i=0; i<2; i++)
        if (filter(mDaughters[i])) vec.push_back(mDaughters[i]);
    return vec;
}

void
StV0Vertex::addDaughter(StTrack* track)
{
    if (track) {
        if (track->geometry()->charge() > 0)
            mDaughters[positive] = track;
        else
            mDaughters[negative] = track;
    }
}

void
StV0Vertex::removeDaughter(StTrack* t)
{
    if (t == mDaughters[positive]) mDaughters[positive] = 0;
    if (t == mDaughters[negative]) mDaughters[negative] = 0;
}

StTrack*
StV0Vertex::daughter(StChargeSign sign)
{
    return mDaughters[sign];
}

const StTrack*
StV0Vertex::daughter(StChargeSign sign) const
{
    return mDaughters[sign];
}

Float_t
StV0Vertex::dcaDaughterToPrimaryVertex(StChargeSign sign) const
{
    return mDcaDaughtersToPrimaryVertex[sign];
}

const StThreeVectorF&
StV0Vertex::momentumOfDaughter(StChargeSign sign) const
{
    return mMomentumOfDaughters[sign];
}

StThreeVectorF
StV0Vertex::momentum() const
{
    return (mMomentumOfDaughters[negative] +
            mMomentumOfDaughters[positive]);
}

Float_t
StV0Vertex::dcaDaughters() const { return mDcaDaughters; }

Float_t
StV0Vertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

void
StV0Vertex::setDcaDaughterToPrimaryVertex(StChargeSign sign, Float_t val)
{
    mDcaDaughtersToPrimaryVertex[sign] = val;
}

void
StV0Vertex::setMomentumOfDaughter(StChargeSign sign, const StThreeVectorF& v)
{
    mMomentumOfDaughters[sign] = v;
}

void
StV0Vertex::setDcaDaughters(Float_t val) { mDcaDaughters = val; }

void
StV0Vertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }
