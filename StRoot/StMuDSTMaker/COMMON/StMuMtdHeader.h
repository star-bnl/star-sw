#ifndef __StMuMtdHeader_hh__
#define __StMuMtdHeader_hh__

#include "TObject.h"

using namespace std;

class StMtdHeader;

class StMuMtdHeader : public TObject {

public:

	StMuMtdHeader() {; }
	StMuMtdHeader(const StMtdHeader* header);
	~StMuMtdHeader() {; }

	short          fiberHeader(int fiberId) const;
    unsigned int   fiberTriggerWord(int fiberId) const;
    unsigned int   triggerTime(int fiberId) const;
    
protected:
    short mFiberHeader1;
    short mFiberHeader2;
	unsigned int mFiberTriggerWord1;
	unsigned int mFiberTriggerWord2;
	unsigned int mTriggerTime1;
	unsigned int mTriggerTime2;
    
ClassDef(StMuMtdHeader,1)

};

#endif
