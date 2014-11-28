#ifndef __StMuMTDCollection_hh__
#define __StMuMTDCollection_hh__

#include "TObject.h"
#include <vector>

using namespace std;

class StMtdCollection;
class StMtdHeader;
class StMuMtdHeader;
class StMuMtdHit;
class StMuMtdRawHit;

class StMuMtdCollection : public TObject {

public:

	StMuMtdCollection();
	StMuMtdCollection(const StMtdCollection& vertex);
	~StMuMtdCollection();

	const StMuMtdHeader*         mtdHeader() const;
    StMuMtdHeader*               mtdHeader();

	StMuMtdRawHit* RawMtdHit(int i);
	StMuMtdHit*  MtdHit(int i);
	
	int hitsPresent();
	int rawHitsPresent();
	
protected:

    vector<StMuMtdHeader> mMtdHeader;
	vector<StMuMtdHit> mMtdHits;	
	vector<StMuMtdRawHit> mMtdRawHits;	
	
ClassDef(StMuMtdCollection,1)

};

#endif
