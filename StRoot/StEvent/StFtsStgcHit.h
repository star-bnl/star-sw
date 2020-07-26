
#ifndef StFtsStgcHit_hh
#define StFtsStgcHit_hh

#include "Stiostream.h"
#include "StObject.h"
#include "TArrayS.h"

class StFtsStgcHit : public StObject {
public:
	StFtsStgcHit();
	StFtsStgcHit(unsigned short rdo, unsigned short sec, unsigned short altro, unsigned short ch, unsigned ntbs, unsigned short *adcs, unsigned short *tbs);
	~StFtsStgcHit() {}


	
	unsigned short rdo() const { return mRdo; }
	unsigned short sec() const { return mSec; }
	unsigned short fee() const { return mFEE; }
	unsigned short altro() const { return mAltro; }
	unsigned short ch() const { return mCH; };
	
	unsigned int   nTimebins() const { return mNTimebins; };
	
	// definition in .cxx
	void setStgcHit( unsigned short rdo, unsigned short sec, unsigned short altro, unsigned short ch, unsigned ntbs, unsigned short *adcs, unsigned short *tbs );
	unsigned short timebin(int i) const;
	unsigned short adc(int i) const;
	void setData(int ntimebin, const unsigned short* adcs, const unsigned short *tbs);
	
	int   adcSum( int tb0, int tb1 ) const;
	int   adcSum() const;
	
	void print(Option_t *option="") const;

protected:
	UShort_t mRdo=0;
	UShort_t mSec=0;
	UShort_t mAltro=0;
	UShort_t mFEE=0;
	UShort_t mCH=0;
	UShort_t mNTimebins=0;
	TArrayS* mAdcs=0;
	TArrayS* mTimebins=0;

	ClassDef(StFtsStgcHit,0)
};

#endif
