/***************************************************************************
 *
 * $Id: StXiVertex.cc,v 1.6 1999/04/09 20:02:11 genevb Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 *
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.cc,v $
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
 **************************************************************************/
#include <iostream.h>
#include "StEvent/StXiVertex.hh"

static const char rcsid[] = "$Id: StXiVertex.cc,v 1.6 1999/04/09 20:02:11 genevb Exp $";

StXiVertex::StXiVertex() : 
 StVertex()
{
    mType = Xi;			// always
    mDcaBachelorToPrimaryVertex = 0;
    mMomentumOfBachelor = StThreeVector<float>();
    mDcaDaughters = 0;
    mDcaParentToPrimaryVertex = 0;
    mV0Vertex = 0;
}

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
    mV0Vertex = 0;
}

StXiVertex::~StXiVertex() { /* noop */ }

float StXiVertex::dcaV0ToPrimaryVertex() const
{
    if (mV0Vertex) {
      return mV0Vertex->dcaParentToPrimaryVertex();
    } else {
      return 0;
    }
}

StThreeVector<float>& StXiVertex::momentumOfV0() const
{
    StThreeVector<float>* v0Mom;
    if (mV0Vertex) {
      const StThreeVector<float>& nMom = mV0Vertex->momentumOfDaughter(negativeTrack);
      const StThreeVector<float>& pMom = mV0Vertex->momentumOfDaughter(positiveTrack);
      v0Mom = new StThreeVector<float>(nMom + pMom);
    } else {
      v0Mom = new StThreeVector<float>();
    }
    return *v0Mom;
}

void StXiVertex::setDcaBachelorToPrimaryVertex(float val)
{
    mDcaBachelorToPrimaryVertex = val;
}

void StXiVertex::setMomentumOfBachelor(const StThreeVector<float>& v)
{
    mMomentumOfBachelor = v;
}

void StXiVertex::setDcaDaughters(float val) { mDcaDaughters = val; }

void StXiVertex::setDcaParentToPrimaryVertex(float val) { mDcaParentToPrimaryVertex = val; }

void StXiVertex::setType(StVertexType)
{
    cerr << "StXiVertex::setType(): change of type not allowed, class has fixed type." << endl;
}

void StXiVertex::setV0Vertex(StV0Vertex* v0vtx)
{
    mV0Vertex = v0vtx;
}

StGlobalTrack* StXiVertex::bachelor()
{
    if ((numberOfDaughters()))
      return daughters()[0]; 
    else
      return 0;
}

double StXiVertex::chargeOfBachelor(double B)
{
    if ((numberOfDaughters()))
      return bachelor()->helix().charge(B);
    else
      return 0;
}

