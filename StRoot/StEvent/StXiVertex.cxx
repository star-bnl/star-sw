/***************************************************************************
 *
 * $Id: StXiVertex.cxx,v 2.0 1999/10/12 18:43:34 ullrich Exp $
 *
 * Author: Gene Van Buren, Feb 1999, revised Thomas Ullrich Sep 99
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.cxx,v $
 * Revision 2.0  1999/10/12 18:43:34  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.1  1999/10/28 22:28:15  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:34  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <iostream.h>
#include "StXiVertex.h"
#include "tables/dst_vertex.h"
#include "tables/dst_xi_vertex.h"
#include "StTrackGeometry.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_xi_vertex_Table.h"

static const char rcsid[] = "$Id: StXiVertex.cxx,v 2.0 1999/10/12 18:43:34 ullrich Exp $";

ClassImp(StXiVertex)

StXiVertex::StXiVertex()
{
    mType = kXiVtxId;
    mDaughter = 0;
    mDcaBachelorToPrimaryVertex = 0;
    mDcaDaughters = 0;
    mDcaParentToPrimaryVertex = 0;
    mV0Vertex = 0;
}

StXiVertex::StXiVertex(const dst_xi_vertex_st& xivtx,
                       const dst_vertex_st& vtx) :
    StVertex(vtx)
{
    mType = kXiVtxId;
    mDcaBachelorToPrimaryVertex = xivtx.b_b;
    mMomentumOfBachelor.setX(xivtx.px_b);
    mMomentumOfBachelor.setY(xivtx.py_b);
    mMomentumOfBachelor.setZ(xivtx.pz_b);
    mDcaDaughters = xivtx.dca;
    mDcaParentToPrimaryVertex = xivtx.b_xi;
    mV0Vertex = 0;
    mDaughter = 0;

StObject*
StXiVertex::clone() { return new StXiVertex(*this); }

StVertexId
StXiVertex::type() const { return kXiVtxId; }

UInt_t
StXiVertex::numberOfDaughters() const { return mDaughter ? 1 : 0;}

StTrack*
StXiVertex::daughter(UInt_t i)
{
    return i == 0 ? mDaughter : 0;
}

const StTrack*
StXiVertex::daughter(UInt_t i) const
{
    return i == 0 ? mDaughter : 0;
}

StPtrVecTrack
StXiVertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    if (filter(mDaughter)) vec.push_back(mDaughter);
    return vec;
}

Float_t
StXiVertex::dcaV0ToPrimaryVertex() const
{
    if (mV0Vertex)
        return mV0Vertex->dcaParentToPrimaryVertex();
    else
        return 0;
}

StThreeVectorF
StXiVertex::momentumOfV0() const
{
    if (mV0Vertex) {
        const StThreeVectorF& nMom = mV0Vertex->momentumOfDaughter(negative);
        const StThreeVectorF& pMom = mV0Vertex->momentumOfDaughter(positive);
        return (nMom + pMom);
    }
    else
        return StThreeVectorF();
}

Float_t
StXiVertex::dcaBachelorToPrimaryVertex () const
{
    return mDcaBachelorToPrimaryVertex;
}

const StThreeVectorF&
StXiVertex::momentumOfBachelor() const { return mMomentumOfBachelor; }

StThreeVectorF
StXiVertex::momentum() const
{
    return mMomentumOfBachelor + momentumOfV0();
}

Float_t
StXiVertex::dcaDaughters() const { return mDcaDaughters; }

Float_t
StXiVertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

StV0Vertex*
StXiVertex::v0Vertex() const { return mV0Vertex; }

StTrack*
StXiVertex::bachelor() { return mDaughter; }

void
StXiVertex::setDcaBachelorToPrimaryVertex(Float_t val)
{
    mDcaBachelorToPrimaryVertex = val;
}

Double_t
StXiVertex::chargeOfBachelor()
{
    StTrack* b = bachelor();
    return b ? b->geometry()->charge() : 0;
}

void
StXiVertex::setMomentumOfBachelor(const StThreeVectorF& v)
{
    mMomentumOfBachelor = v;
}

void
StXiVertex::setDcaDaughters(Float_t val) { mDcaDaughters = val; }

void
StXiVertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }

void
StXiVertex::setV0Vertex(StV0Vertex* v0vtx)
{
    mV0Vertex = v0vtx;
}

void
StXiVertex::addDaughter(StTrack* track) { mDaughter = track; }


void
StXiVertex::removeDaughter(StTrack* track)
{
    if (track == mDaughter)  mDaughter = 0;
}
    
