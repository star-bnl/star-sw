/***************************************************************************
 *
 * $Id: StV0Vertex.cc,v 1.3 1999/01/27 13:05:05 ullrich Exp $
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
 * Revision 1.3  1999/01/27 13:05:05  ullrich
 * Renamed data member and access functions: xxxToV0 into xxxToPrimaryVertex.
 * This is the right meaning according to P. Jones.
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
#include "StEvent/StEnumerations.hh"

static const char rcsid[] = "$Id: StV0Vertex.cc,v 1.3 1999/01/27 13:05:05 ullrich Exp $";

StV0Vertex::StV0Vertex()
{
    mType = V0;			// always
    mDcaDaughtersToPrimaryVertex[0] = 0;
    mDcaDaughtersToPrimaryVertex[1] = 0;
    mDcaDaughters = 0;
    mDcaParentToPrimaryVertex = 0;
}

StV0Vertex::StV0Vertex(dst_v0_vertex_st* v0vtx)
{
    mType = V0;			// always
    mDcaDaughtersToPrimaryVertex[0] = v0vtx->dcan;
    mDcaDaughtersToPrimaryVertex[1] = v0vtx->dcap;
    mDcaDaughters = v0vtx->dcapn;
    mDcaParentToPrimaryVertex = v0vtx->dcav0;
}

StV0Vertex::~StV0Vertex() { /* noop */ }

void StV0Vertex::setDcaDaughtersToPrimaryVertex(unsigned int i, float val)
{
    if (i < 2)
	mDcaDaughtersToPrimaryVertex[i] = val;
}

void StV0Vertex::setDcaDaughters(float val) { mDcaDaughters = val; }

void StV0Vertex::setDcaParentToPrimaryVertex(float val) { mDcaParentToPrimaryVertex = val; }

void StV0Vertex::setType(StVertexType)
{
    cerr << "StV0Vertex::setType(): change of type not allowed, class has fixed type." << endl;
}
