/***************************************************************************
 *
 * $Id: StPrimaryVertex.cxx,v 2.4 1999/11/09 15:44:11 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.cxx,v $
 * Revision 2.4  1999/11/09 15:44:11  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.4  1999/11/09 15:44:11  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.3  1999/11/04 20:36:17  ullrich
 * New method to obtain daughter container directly
 *
 * Revision 2.2  1999/10/28 22:26:16  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <iostream.h>
#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
#include "StTrack.h"
#include "StFunctional.h"
#include "tables/St_dst_vertex_Table.h"

ClassImp(StPrimaryVertex)

static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.4 1999/11/09 15:44:11 ullrich Exp $";

StPrimaryVertex::StPrimaryVertex()
{ mType = kEventVtxId; }

StPrimaryVertex::StPrimaryVertex(const dst_vertex_st& v) : StVertex(v)
{ mType = kEventVtxId; }

StPrimaryVertex::~StPrimaryVertex() {/* noop */};

StObject*
StPrimaryVertex::clone() { return new StPrimaryVertex(*this); }

StVertexId
StPrimaryVertex::type() const { return kEventVtxId; }

UInt_t
StPrimaryVertex::numberOfDaughters() const
{
    return mDaughters.size();
}

StTrack*
StPrimaryVertex::daughter(UInt_t i)
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

const StTrack*
StPrimaryVertex::daughter(UInt_t i) const
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

StPtrVecTrack
StPrimaryVertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    for (unsigned int i=0; i<mDaughters.size(); i++)
        if (filter(mDaughters[i])) vec.push_back(mDaughters[i]);
    return vec;
}

StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters() { return mDaughters; }

const StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters() const { return mDaughters; }

void
StPrimaryVertex::addDaughter(StTrack* t)
{
    StPrimaryTrack* p = dynamic_cast<StPrimaryTrack*>(t);
    if (p) mDaughters.push_back(p);
}

void
StPrimaryVertex::removeDaughter(StTrack* t)
{
    StSPtrVecPrimaryTrackIterator iter;
    for (iter=mDaughters.begin(); iter != mDaughters.end(); iter++)
        if (*iter == t) mDaughters.erase(iter);
}

void
StPrimaryVertex::setParent(StTrack*)
{
    cerr << "StPrimaryVertex::setParent(): StPrimaryVertex cannot have a parent." << endl;
}


