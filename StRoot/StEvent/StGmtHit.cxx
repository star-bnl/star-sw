/**
 * \class StGmtHit
 * \brief Holds data for the hit in GMT
 * 
 * Data for an individual ``hit'' in GMT, i.e. a 1D cluster (based on StFgtHit).
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// C++ headers
#include <cmath>

// StEvent headers
#include "StGmtHit.h"

//________________
StGmtHit::StGmtHit( Int_t key, Int_t module, Float_t adcX, 
	                Float_t adcY, Float_t dadcX, Float_t dadcY,
	                Float_t localX, Float_t localY,
	                Float_t localXErr, Float_t localYErr,
	                Float_t sigmaX, Float_t sigmaY,
	                Float_t sigmaXErr, Float_t sigmaYErr) : 
  StHit( StThreeVectorF(localX,localY,0), StThreeVectorF(localXErr,localYErr,0), 
  	     module+1, 0.), mKey(key), mAdcX(adcX), mAdcY(adcY), mdAdcX(dadcX), 
         mdAdcY(dadcY), mSigmaX(sigmaX), mErrSigmaX(sigmaXErr), mSigmaY(sigmaY), 
         mErrSigmaY(sigmaYErr) {
  /* empty */
}

//________________
StGmtHit::~StGmtHit() {
	/* empty */
}

//________________
ostream& operator<<(ostream& os, const StGmtHit& v) {
  return os << Form("Gmt m %3i ",v.getModule())
	        << *((StHit *)&v)
	    	<< Form(" Adc X/Y  %5.1f/%5.1f locX = %8.3f +/- %7.3f locY = %8.3f +/- %7.3f",
		    		v.getAdcX(), v.getAdcY(), v.getLocalX(), v.getErrorLocalX(), 
                    v.getLocalY(), v.getErrorLocalY()); 
}

//________________
void StGmtHit::Print(Option_t *option) const {
	std::cout << *this << std::endl;
}


