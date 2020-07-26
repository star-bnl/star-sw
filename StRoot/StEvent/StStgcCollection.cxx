#include "StEvent/StStgcCollection.h"

#include "StEvent/StFtsStgcHit.h"

ClassImp(StStgcCollection)

StStgcCollection::StStgcCollection() {}
StStgcCollection::~StStgcCollection() {}

void StStgcCollection::addHit(unsigned int det, StFtsStgcHit* hit){mHits[det%(kStgcNDet+1)].push_back(hit);}
StSPtrVecFtsStgcHit& StStgcCollection::hits(unsigned int det) {return mHits[det%(kStgcNDet+1)];}
const StSPtrVecFtsStgcHit& StStgcCollection::hits(unsigned int det) const {return mHits[det%(kStgcNDet+1)];}
unsigned int StStgcCollection::numberOfHits(unsigned int det) const { return mHits[det%(kStgcNDet+1)].size(); }

void StStgcCollection::print(int option) {
	cout << Form("  *** Print Stgc collection ***") << endl;
	for(unsigned int det=0; det<kStgcNDet+1; det++){
	cout << Form("  *** Stgc Det=%1d *** NHit=%3d",
			 det,numberOfHits(det)) << endl;
	}
}

