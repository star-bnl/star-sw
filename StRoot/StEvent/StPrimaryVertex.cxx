/***************************************************************************
 *
 * $Id: StPrimaryVertex.cxx,v 2.1 1999/10/13 19:45:02 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.cxx,v $
 * Revision 2.1  1999/10/13 19:45:02  ullrich
 * Initial Revision
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
#include "tables/dst_vertex.h"
#include <iostream.h>
#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.1 1999/10/13 19:45:02 ullrich Exp $";
#include "StFunctional.h"
#include "tables/St_dst_vertex_Table.h"

ClassImp(StPrimaryVertex)

static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.1 1999/10/13 19:45:02 ullrich Exp $";

StPrimaryVertex::StPrimaryVertex()
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
    for (int i=0; i<mDaughters.size(); i++)

StPtrVecTrack
StPrimaryVertex::daughters(StTrackFilter& filter)

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
StPrimaryVertex::unlink(StTrack* t)
{
    //
    //  Remove a track from the daughter list but do not delete it.
    //  This is needed in case a track gets deleted elsewhere but
    //  is referenced/owned by the vertex.
    //
    StSPtrVecPrimaryTrackIterator iter;
    for (iter=mDaughters.begin(); iter != mDaughters.end(); iter++)
        if (*iter == t) mDaughters.clean(iter);
}

void
}

void
StPrimaryVertex::setParent(StTrack*)
{
    cerr << "StPrimaryVertex::setParent(): StPrimaryVertex cannot have a parent." << endl;
}


