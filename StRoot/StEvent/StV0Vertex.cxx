/***************************************************************************
 *
 * $Id: StV0Vertex.cxx,v 1.3 1999/04/27 01:24:29 fisyak Exp $
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
 * $Log: StV0Vertex.cxx,v $
 * Revision 1.3  1999/04/27 01:24:29  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.6  1999/04/09 19:34:03  genevb
 * Added vertex daughter functionality
 *
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
 * Revision 2.1  1999/10/28 22:28:01  ullrich
static const Char_t rcsid[] = "$Id: StV0Vertex.cxx,v 1.3 1999/04/27 01:24:29 fisyak Exp $";
 *
#ifdef __ROOT__
#include <algorithm>
static const Char_t rcsid[] = "$Id: StV0Vertex.cxx,v 1.3 1999/04/27 01:24:29 fisyak Exp $";
#endif
#include "tables/dst_v0_vertex.h"
ClassImp(StV0Vertex)
#include "tables/St_dst_vertex_Table.h"
StV0Vertex::StV0Vertex() : 
 StVertex()

    mType = V0;			// always
    mDcaDaughtersToPrimaryVertex[negativeTrack] = 0;
    mDcaDaughtersToPrimaryVertex[positiveTrack] = 0;
    mMomentumOfDaughters[negativeTrack] = StThreeVectorF();
    mMomentumOfDaughters[positiveTrack] = StThreeVectorF();
    mDcaDaughters = 0;
    mType = kV0VtxId;
    mDaughters.resize(2);
    mDaughters[negative] = 0;
StV0Vertex::StV0Vertex(dst_v0_vertex_st* v0vtx, dst_vertex_st* vtx) :
 StVertex(vtx)
    fill_n(mDcaDaughtersToPrimaryVertex, 2, 0);
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
{
    return 0;
    if (track) {
        if (track->geometry()->charge() > 0)
void StV0Vertex::setDcaDaughterToPrimaryVertex(StTrackSign sign, Float_t val)
            mDaughters[negative] = track;
    mDcaDaughtersToPrimaryVertex[sign] = val;
}

void StV0Vertex::setMomentumOfDaughter(StTrackSign sign, const StThreeVectorF& v)
{
    mMomentumOfDaughters[sign] = v;

StThreeVectorF
void StV0Vertex::setDcaDaughters(Float_t val) { mDcaDaughters = val; }
    return (mMomentumOfDaughters[negative] +
void StV0Vertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }

void StV0Vertex::setType(StVertexType)
{
    cerr << "StV0Vertex::setType(): change of type not allowed, class has fixed type." << endl;
}

void
StV0Vertex::setDcaDaughters(Float_t val) { mDcaDaughters = val; }

void
StV0Vertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }
