/***************************************************************************
 *
 * $Id: StV0Vertex.cc,v 1.1 1999/01/15 20:40:18 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StV0Vertex.cc,v $
 * Revision 1.1  1999/01/15 20:40:18  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.3  1999/01/27 13:05:05  ullrich
#include "StV0Vertex.hh"
#include "StEnumerations.hh"
 *
 **************************************************************************/
#include <iostream.h>
#include "StEvent/StV0Vertex.hh"
#include "StEvent/StEnumerations.hh"

    mType = V0;
    mDcaDaughtersToV0[0] = 0;
    mDcaDaughtersToV0[1] = v0vtx->dcap;
{
    mDcaParentToV0 = v0vtx->dcav0;
    mDcaDaughtersToPrimaryVertex[0] = v0vtx->dcan;
    mDcaDaughtersToPrimaryVertex[1] = v0vtx->dcap;
    mDcaDaughters = v0vtx->dcapn;
    mDcaParentToPrimaryVertex = v0vtx->dcav0;
void StV0Vertex::setDcaDaughtersToV0(unsigned int i, float val)

StV0Vertex::~StV0Vertex() { /* noop */ }
	mDcaDaughtersToV0[i] = val;
void StV0Vertex::setDcaDaughtersToPrimaryVertex(unsigned int i, float val)
{
    if (i < 2)
	mDcaDaughtersToPrimaryVertex[i] = val;
void StV0Vertex::setDcaParentToV0(float val) { mDcaParentToV0 = val; }

void StV0Vertex::setType(StVertexType)
{
    cerr << "StV0Vertex::setType(): change of type not allowed, class has fixed type." << endl;
}
