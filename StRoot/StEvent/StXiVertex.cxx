/***************************************************************************
 *
 * $Id: StXiVertex.cxx,v 1.2 1999/04/28 22:27:40 fisyak Exp $
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
 * Revision 1.2  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.8  1999/04/14 22:04:30  genevb
 * Fixed a memory leak
 *
 * Revision 1.7  1999/04/13 23:27:21  genevb
 * Slightly refined vertex code, updated V0, Xi vertex documentation
 *
 * Revision 1.6  1999/04/09 20:02:11  genevb
 * Change constancy of new functions
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
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
#include "StXiVertex.h"
static const Char_t rcsid[] = "$Id: StXiVertex.cxx,v 1.2 1999/04/28 22:27:40 fisyak Exp $";
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
    }
StXiVertex::dcaV0ToPrimaryVertex() const
{
StThreeVectorF StXiVertex::momentumOfV0() const
    else
        return 0;
      const StThreeVectorF& nMom = mV0Vertex->momentumOfDaughter(negativeTrack);
      const StThreeVectorF& pMom = mV0Vertex->momentumOfDaughter(positiveTrack);
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

void StXiVertex::setV0Vertex(StV0Vertex* v0vtx)

void
StXiVertex::setDcaParentToPrimaryVertex(Float_t val) { mDcaParentToPrimaryVertex = val; }

Double_t StXiVertex::chargeOfBachelor(Double_t B)
void
    StGlobalTrack* b = bachelor();
    return ( (b) ? b->helix().charge(B) : 0 ) ;

void
StXiVertex::removeDaughter(StTrack* track)
{
    if (track == mDaughter)  mDaughter = 0;
}
    
