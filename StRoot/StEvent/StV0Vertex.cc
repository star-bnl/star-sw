/***************************************************************************
 *
 * $Id: StV0Vertex.cc,v 1.5 1999/02/21 20:32:48 genevb Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StV0Vertex.cc,v $
 * Revision 1.5  1999/02/21 20:32:48  genevb
 * Improve StV0Vertex code
 *
 * Revision 1.4  1999/02/18 15:42:09  ullrich
 * Momemta of daughter tracks added.
 *
 * Revision 1.3  1999/01/27 13:05:05  ullrich
 * Renamed data member and access functions: xxxToV0 into xxxToPrimaryVertex.
 * This is the right meaning according to P. Jones.
 *
 * Revision 1.2  1999/01/15 22:54:15  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include <iostream.h>
#include "StEvent/StV0Vertex.hh"

static const char rcsid[] = "$Id: StV0Vertex.cc,v 1.5 1999/02/21 20:32:48 genevb Exp $";

StV0Vertex::StV0Vertex() : 
 StVertex()
{
    mType = V0;			// always
    mDcaDaughtersToPrimaryVertex[negativeTrack] = 0;
    mDcaDaughtersToPrimaryVertex[positiveTrack] = 0;
    mMomentumOfDaughters[negativeTrack] = StThreeVector<float>();
    mMomentumOfDaughters[positiveTrack] = StThreeVector<float>();
    mDcaDaughters = 0;
    mDcaParentToPrimaryVertex = 0;
}

StV0Vertex::StV0Vertex(dst_v0_vertex_st* v0vtx, dst_vertex_st* vtx) :
 StVertex(vtx)
{
    mType = V0;			// always
    mDcaDaughtersToPrimaryVertex[negativeTrack] = v0vtx->dcan;
    mDcaDaughtersToPrimaryVertex[positiveTrack] = v0vtx->dcap;
    mMomentumOfDaughters[negativeTrack].setX(v0vtx->neg_px);
    mMomentumOfDaughters[negativeTrack].setY(v0vtx->neg_py);
    mMomentumOfDaughters[negativeTrack].setZ(v0vtx->neg_pz);
    mMomentumOfDaughters[positiveTrack].setX(v0vtx->pos_px);
    mMomentumOfDaughters[positiveTrack].setY(v0vtx->pos_py);
    mMomentumOfDaughters[positiveTrack].setZ(v0vtx->pos_pz);
    mDcaDaughters = v0vtx->dcapn;
    mDcaParentToPrimaryVertex = v0vtx->dcav0;
}

StV0Vertex::~StV0Vertex() { /* noop */ }

void StV0Vertex::setDcaDaughterToPrimaryVertex(StTrackSign sign, float val)
{
    mDcaDaughtersToPrimaryVertex[sign] = val;
}

void StV0Vertex::setMomentumOfDaughter(StTrackSign sign, const StThreeVector<float>& v)
{
    mMomentumOfDaughters[sign] = v;
}

void StV0Vertex::setDcaDaughters(float val) { mDcaDaughters = val; }

void StV0Vertex::setDcaParentToPrimaryVertex(float val) { mDcaParentToPrimaryVertex = val; }

void StV0Vertex::setType(StVertexType)
{
    cerr << "StV0Vertex::setType(): change of type not allowed, class has fixed type." << endl;
}
