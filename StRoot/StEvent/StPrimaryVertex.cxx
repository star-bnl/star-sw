/***************************************************************************
 *
 * $Id: StPrimaryVertex.cxx,v 2.14 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.cxx,v $
 * Revision 2.14  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.13  2006/04/07 18:21:28  ullrich
 * Added data member mMeanDip incl. access functions (Marco).
 *
 * Revision 2.12  2005/07/15 20:17:35  ullrich
 * Corrected spelling in membrane
 *
 * Revision 2.11  2005/06/15 21:50:32  ullrich
 * Added members and methods to identify used vertex finder and store vertex quality.
 *
 * Revision 2.10  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.9  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
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
#include <Stiostream.h>
#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
#include "StTrack.h"
#include "StFunctional.h"

ClassImp(StPrimaryVertex)

static const char rcsid[] = "$Id: StPrimaryVertex.cxx,v 2.14 2009/11/23 16:34:06 fisyak Exp $";

StPrimaryVertex::StPrimaryVertex()
{init();}

void StPrimaryVertex::init()
{
    mType = kEventVtxId;
    mVertexFinderId = undefinedVertexFinder; 
    mNumTracksUsedInFinder = 0;
    mNumMatchesWithCTB = 0; 
    mNumMatchesWithBEMC = 0;
    mNumMatchesWithEEMC = 0;
    mNumTracksCrossingCentralMembrane = 0; 
    mMeanDip = 0;
    mSumOfTrackPt = 0;
    mRanking = 0;    
}

StPrimaryVertex::~StPrimaryVertex() {/* noop */};

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
    return vec;
}

StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters(StTrackType type)
{
	return mDaughters;	     
}

const StSPtrVecPrimaryTrack&
StPrimaryVertex::daughters(StTrackType type) const
{
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
}

void
StPrimaryVertex::setParent(StTrack*)
{
    cerr << "StPrimaryVertex::setParent(): StPrimaryVertex cannot have a parent." << endl;
}


