/***************************************************************************
 *
 * $Id: StXiVertex.cxx,v 1.1 1999/04/27 01:24:32 fisyak Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 *
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.cxx,v $
 * Revision 1.1  1999/04/27 01:24:32  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.5  1999/04/09 19:34:05  genevb
 * Added vertex daughter functionality
 *
 * Revision 1.4  1999/02/24 01:55:35  genevb
 * Add Xi vertex type
 *
 * Revision 1.3  1999/02/23 16:13:26  genevb
 * Add v0 pointers for xi's outside constructor
 *
 * Revision 1.2  1999/02/23 13:50:59  genevb
 * Fixed some typos
 *
 * Revision 1.1  1999/02/23 13:46:45  genevb
 * Add new StXiVertex
 *
 *
 * Revision 2.1  1999/10/28 22:28:15  ullrich
static const Char_t rcsid[] = "$Id: StXiVertex.cxx,v 1.1 1999/04/27 01:24:32 fisyak Exp $";
 *
#ifdef __ROOT__
#include "StXiVertex.h"
static const Char_t rcsid[] = "$Id: StXiVertex.cxx,v 1.1 1999/04/27 01:24:32 fisyak Exp $";
#endif
#include "tables/dst_xi_vertex.h"
#include "StTrackGeometry.h"
#include "tables/St_dst_vertex_Table.h"
StXiVertex::StXiVertex() : 
 StVertex()

    mType = Xi;			// always
ClassImp(StXiVertex)
    mMomentumOfBachelor = StThreeVectorF();

StXiVertex::StXiVertex()
{
    mType = kXiVtxId;
    mDaughter = 0;
StXiVertex::StXiVertex(dst_xi_vertex_st* xivtx, dst_vertex_st* vtx) :
 StVertex(vtx)
{
    mType = Xi;			// always
    mDcaBachelorToPrimaryVertex = xivtx->b_b;
    mMomentumOfBachelor.setX(xivtx->px_b);
    mMomentumOfBachelor.setY(xivtx->py_b);
    mMomentumOfBachelor.setZ(xivtx->pz_b);
    mDcaDaughters = xivtx->dca;
    mDcaParentToPrimaryVertex = xivtx->b_xi;
    mDcaBachelorToPrimaryVertex = xivtx.b_b;
    mMomentumOfBachelor.setY(xivtx.py_b);
    mMomentumOfBachelor.setZ(xivtx.pz_b);
    mDcaDaughters = xivtx.dca;
    mDcaParentToPrimaryVertex = xivtx.b_xi;
Float_t StXiVertex::dcaV0ToPrimaryVertex() const
    if (filter(mDaughter)) vec.push_back(mDaughter);
    if (mV0Vertex) {
      return mV0Vertex->dcaParentToPrimaryVertex();
    } else {
      return 0;
StThreeVectorF& StXiVertex::momentumOfV0() const
StXiVertex::dcaV0ToPrimaryVertex() const
    StThreeVectorF* v0Mom;
{
StThreeVectorF StXiVertex::momentumOfV0() const
    else
      v0Mom = new StThreeVectorF(nMom + pMom);
    } else {
      v0Mom = new StThreeVectorF();
      const StThreeVectorF& nMom = mV0Vertex->momentumOfDaughter(negativeTrack);
    return *v0Mom;
      return (nMom + pMom);
StXiVertex::momentumOfV0() const
    return StThreeVectorF();
        const StThreeVectorF& nMom = mV0Vertex->momentumOfDaughter(negative);
        const StThreeVectorF& pMom = mV0Vertex->momentumOfDaughter(positive);
void StXiVertex::setDcaBachelorToPrimaryVertex(Float_t val)
    else
    mDcaBachelorToPrimaryVertex = val;
}

void StXiVertex::setMomentumOfBachelor(const StThreeVectorF& v)

    mMomentumOfBachelor = v;
StXiVertex::momentumOfBachelor() const { return mMomentumOfBachelor; }

void StXiVertex::setDcaDaughters(Float_t val) { mDcaDaughters = val; }

void StXiVertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }

void StXiVertex::setType(StVertexType)

    cerr << "StXiVertex::setType(): change of type not allowed, class has fixed type." << endl;
StXiVertex::bachelor() { return mDaughter; }

void
    StGlobalTrack* b = bachelor();
    return ( (b) ? b->helix().charge(B) : 0 ) ;

void
StXiVertex::removeDaughter(StTrack* track)
{
    if (track == mDaughter)  mDaughter = 0;
}
    
