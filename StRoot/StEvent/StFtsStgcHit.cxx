
#include "StFtsStgcHit.h"

ClassImp(StFtsStgcHit)

StFtsStgcHit::StFtsStgcHit() {}
StFtsStgcHit::StFtsStgcHit(unsigned short rdo, unsigned short sec, unsigned short altro, unsigned short ch, unsigned ntbs, unsigned short *adcs, unsigned short *tbs) {
	setStgcHit( rdo, sec, altro, ch, ntbs, adcs, tbs );
}

void StFtsStgcHit::setData(int ntimebin, const unsigned short* adcs, const unsigned short *tbs){
	
	mNTimebins = ntimebin;
	if ( adcs != nullptr  ){
		if(!mAdcs){
			mAdcs = new TArrayS(mNTimebins,(const short*)adcs);
		}else{
			mAdcs->Set(mNTimebins,(const short*)adcs);
		}
	}

	if ( tbs != nullptr ){
		if(!mTimebins){
			mTimebins = new TArrayS(mNTimebins,(const short*)tbs);
		}else{
			mTimebins->Set(mNTimebins,(const short*)tbs);
		}
	}
	
}


void StFtsStgcHit::print(Option_t *option) const {

	cout << Form("StFtsStgcHit: rdo=%2d sec=%2d altro=%2d CH=%2d | Ntb=%d sumADC=%d", rdo(), sec(), altro(), ch(), nTimebins(), adcSum() ) << endl;
	cout << ":::::";
	for(int i=0; i<nTimebins(); i++) {
		cout << Form("%4d (%3d) ",adc(i),timebin(i));
	}
	cout << endl;
}

void StFtsStgcHit::setStgcHit( unsigned short rdo, unsigned short sec, unsigned short altro, unsigned short ch, unsigned ntbs, unsigned short *adcs, unsigned short *tbs ){

	mRdo = rdo;
	mSec = sec;
	mAltro = altro;
	mFEE = altro / 2; // implicit floor
	mCH = ch;

	mNTimebins = ntbs;
	setData( ntbs, adcs, tbs );
}

unsigned short StFtsStgcHit::timebin(int i) const{
	return mTimebins->At(i);
}

unsigned short StFtsStgcHit::adc(int i) const{
	return mAdcs->At(i);
}
	
int StFtsStgcHit::adcSum() const{
	unsigned int sum = 0;
	for ( size_t i = 0; i < mNTimebins; i++ ){
		sum+=adc(i);
	}
	return sum;
}


int StFtsStgcHit::adcSum( int tb0, int tb1 ) const{
	unsigned int sum = 0;
	if ( tb0 >= mNTimebins || tb1 >= mNTimebins )
		return 0;


	for ( size_t i = tb0; i < tb1; i++ ){
		sum+=adc(i);
	}
	return sum;
}