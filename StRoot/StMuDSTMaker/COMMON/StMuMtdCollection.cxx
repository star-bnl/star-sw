#ifdef __APPLE__
#include <sys/types.h>
#endif
#include "StMuMtdCollection.h"
#include "StMuMtdRawHit.h"
#include "StMuMtdHit.h"
#include "StMuMtdHeader.h"

#include "StEvent/StEvent.h"
#include "StEvent/StMtdCollection.h"
#include "StEvent/StMtdHeader.h"
#include "StEvent/StMtdHit.h"
#include "StEvent/StMtdRawHit.h"

ClassImp(StMuMtdCollection)

StMuMtdCollection::StMuMtdCollection() {;}
StMuMtdCollection::~StMuMtdCollection() {;}

StMuMtdCollection::StMuMtdCollection(const StMtdCollection & mtd){

	const StMtdCollection *mtdp = &mtd;

	//Fill Header	
	mMtdHeader.push_back(StMuMtdHeader(mtdp->mtdHeader()));
	
	//Fill Hits
	const StSPtrVecMtdHit& VecHit = mtdp->mtdHits();
	for(u_int i=0; i< VecHit.size(); i++){
		StMtdHit *pHit = (StMtdHit*)VecHit.at(i);		
		mMtdHits.push_back(StMuMtdHit(pHit));
	}
	
	//Fill Raw Hits	
	const StSPtrVecMtdRawHit& VecRawHit = mtdp->mtdRawHits();
	for(u_int i=0; i< VecRawHit.size(); i++){
		StMtdRawHit *pRawHit = (StMtdRawHit*)VecRawHit.at(i);
		mMtdRawHits.push_back(StMuMtdRawHit(pRawHit));
	}

}

StMuMtdHeader* StMuMtdCollection::mtdHeader() { return &mMtdHeader[0]; }
const StMuMtdHeader* StMuMtdCollection::mtdHeader() const { return &mMtdHeader[0]; }

int StMuMtdCollection::hitsPresent() { return mMtdHits.size(); }
int StMuMtdCollection::rawHitsPresent() { return mMtdRawHits.size(); }

StMuMtdRawHit* StMuMtdCollection::RawMtdHit(int i) { return &mMtdRawHits[i]; }
StMuMtdHit* StMuMtdCollection::MtdHit(int i) { return &mMtdHits[i]; }
