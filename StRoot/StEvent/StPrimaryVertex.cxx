/***************************************************************************
 *
 * $Id: StPrimaryVertex.cxx,v 2.8 2002/04/18 23:38:21 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.cxx,v $
 * Revision 2.8  2002/04/18 23:38:21  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.7  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2001/03/24 03:34:53  perev
 * clone() -> clone() const
 *
 * Revision 2.5  2000/04/03 15:30:23  ullrich
 * addDaughter() now assigns the right vertex, i.e. this, to
 * the primary tracks stored within this primary vertex.
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

static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.8 2002/04/18 23:38:21 jeromel Exp $";

StPrimaryVertex::StPrimaryVertex()
{ mType = kEventVtxId; }

StPrimaryVertex::StPrimaryVertex(const dst_vertex_st& v) : StVertex(v)
{ mType = kEventVtxId; }

StPrimaryVertex::~StPrimaryVertex() {/* noop */};

StObject*
StPrimaryVertex::clone() const { return new StPrimaryVertex(*this); }

StVertexId
StPrimaryVertex::type() const { return kEventVtxId; }

unsigned int
StPrimaryVertex::numberOfDaughters() const
{
    return mDaughters.size();
}

unsigned int
StPrimaryVertex::numberOfDaughters(StTrackType type) const
{
    if (type == primary)
	return mDaughters.size();
    else if (type == estPrimary)
	return mEstDaughters.size();
    else
	return 0;
}

StTrack*
StPrimaryVertex::daughter(unsigned int i)
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

StTrack*
StPrimaryVertex::daughter(unsigned int i, StTrackType type)
{
    if (type == primary)
	return i < mDaughters.size() ? mDaughters[i] : 0;
    else if (type == estPrimary)
	return i < mEstDaughters.size() ? mEstDaughters[i] : 0;	
    else
	return 0;
}

const StTrack*
StPrimaryVertex::daughter(unsigned int i) const
{
    return i < mDaughters.size() ? mDaughters[i] : 0;
}

const StTrack*
StPrimaryVertex::daughter(unsigned int i, StTrackType type) const
{
    if (type == primary)
	return i < mDaughters.size() ? mDaughters[i] : 0;
    else if (type == estPrimary)
	return i < mEstDaughters.size() ? mEstDaughters[i] : 0;	
    else
	return 0;
}

StPtrVecTrack
StPrimaryVertex::daughters(StTrackFilter& filter)
{
    StPtrVecTrack vec;
    for (unsigned int i=0; i<mDaughters.size(); i++)
	if (filter(mDaughters[i])) vec.push_back(mDaughters[i]);
    return vec;
}

StPtrVecTrack
StPrimaryVertex::daughters(StTrackFilter& filter, StTrackType type)
{
    StPtrVecTrack vec;
    if (type == primary) {
	for (unsigned int i=0; i<mDaughters.size(); i++)
	    if (filter(mDaughters[i])) vec.push_back(mDaughters[i]);
    }
    else if (type == estPrimary) {
	for (unsigned int i=0; i<mEstDaughters.size(); i++)
	    if (filter(mEstDaughters[i])) vec.push_back(mEstDaughters[i]);
    }
    return vec;
}

StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters(StTrackType type)
{
    if (type == estPrimary)
	return mEstDaughters;
    else 
	return mDaughters;	     
}

const StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters(StTrackType type) const
{
    if (type == estPrimary)
	return mEstDaughters;
    else 
	return mDaughters;	     
}

void
StPrimaryVertex::addDaughter(StTrack* t)
{
    StPrimaryTrack* p = dynamic_cast<StPrimaryTrack*>(t);
    if (p) {
	if (p->type() == primary) {
	    mDaughters.push_back(p);
	    p->setVertex(this);
	}
	else if (p->type() == estPrimary) {
	    mEstDaughters.push_back(p);
	    p->setVertex(this);
	}
    }
}

void
StPrimaryVertex::removeDaughter(StTrack* t)
{
    StPrimaryTrack* p = dynamic_cast<StPrimaryTrack*>(t);
    if (!p) return;
    StSPtrVecPrimaryTrackIterator iter;
    if (p->type() == primary) {
	for (iter=mDaughters.begin(); iter != mDaughters.end(); iter++)
	    if (*iter == t) {
		mDaughters.erase(iter);
		p->setVertex(0);
	    }
    }
    else if (p->type() == estPrimary) {
	for (iter=mEstDaughters.begin(); iter != mEstDaughters.end(); iter++)
	    if (*iter == t) {
		mEstDaughters.erase(iter);
		p->setVertex(0);
	    }
    }
}

void
StPrimaryVertex::setParent(StTrack*)
{
    cerr << "StPrimaryVertex::setParent(): StPrimaryVertex cannot have a parent." << endl;
}


