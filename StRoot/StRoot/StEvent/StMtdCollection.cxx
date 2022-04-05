/***************************************************************************
 *
 * $Id: StMtdCollection.cxx,v 2.2 2012/02/28 01:25:32 perev Exp $ 
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *
 * Persistent data which is written into StEvent
 * directly from the reco chain.
 *
 ***************************************************************************
 *
 * $Log: StMtdCollection.cxx,v $
 * Revision 2.2  2012/02/28 01:25:32  perev
 * Browse(...) added
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "TBrowser.h"
#include "StMtdCollection.h"

static const char rcsid[] = "$Id: StMtdCollection.cxx,v 2.2 2012/02/28 01:25:32 perev Exp $";

ClassImp(StMtdCollection)
    
StMtdCollection::StMtdCollection()
{
    mMtdHeader = 0;
}

StMtdCollection::~StMtdCollection()
{
    if(mMtdHeader) delete mMtdHeader;
}

StMtdHeader*
StMtdCollection::mtdHeader() { return mMtdHeader; }

const StMtdHeader*
StMtdCollection::mtdHeader() const { return mMtdHeader; }

const StSPtrVecMtdHit&
StMtdCollection::mtdHits() const { return mMtdHits; }

StSPtrVecMtdHit&
StMtdCollection::mtdHits() { return mMtdHits; }

const StSPtrVecMtdRawHit&
StMtdCollection::mtdRawHits() const { return mMtdRawHits; }

StSPtrVecMtdRawHit&
StMtdCollection::mtdRawHits() { return mMtdRawHits; }

void
StMtdCollection::setHeader(StMtdHeader* val) { mMtdHeader = val; }

void
StMtdCollection::addHit(const StMtdHit* aHit)
{
    if (aHit) mMtdHits.push_back(aHit);
}

void
StMtdCollection::addRawHit(const StMtdRawHit* aRawHit)
{
    if (aRawHit) mMtdRawHits.push_back(aRawHit);
}

bool
StMtdCollection::hitsPresent() const { return mMtdHits.size(); }

bool
StMtdCollection::rawHitsPresent() const { return mMtdRawHits.size(); }
//_____________________________________________________________________________
void StMtdCollection::Browse(TBrowser *b)
{
  mMtdHits.Browse(b);
  mMtdRawHits.Browse(b);
//   // Browse this event (called by TBrowser).
//    for (int i=0; i<(int)mMtdHits.size(); i++) {
//      TObject *obj = mMtdHits[i]; if (!obj) continue;
//      TString ts(obj->GetName()); ts+="#"; ts+=i;
//      b->Add(obj,ts.Data());
//    }
//    for (int i=0; i<(int)mMtdRawHits.size(); i++) {
//      TObject *obj = mMtdRawHits[i]; if (!obj) continue;
//      TString ts(obj->GetName()); ts+="#"; ts+=i;
//      b->Add(obj,ts.Data());
//    }
}
