#ifndef __StMuMdtHit_hh__
#define __StMuMdtHit_hh__

#include "TObject.h"

using namespace std;

class StMtdHit;
class StMuMtdHit : public TObject {

public:

	StMuMtdHit() {; }
	StMuMtdHit(const StMtdHit* hit);
	~StMuMtdHit() {; }

    int             backleg() const;
    int             module() const;
    int             cell() const;
    pair<double,double>   leadingEdgeTime() const;
    pair<double,double>   trailingEdgeTime() const;
    pair<double,double>   tot() const;
    double          tof() const;
	
    short associatedTrackKey() const;
    int             idTruth() const;
    int             qaTruth() const;
	
private:

	UChar_t   mBackLeg;
    UChar_t   mModule;
    UChar_t   mCell;
    pair<Double_t,Double_t>  mLeadingEdgeTime;
    pair<Double_t,Double_t>  mTrailingEdgeTime;
	
    UShort_t  mIdTruth;  // simulation associated track id
    UShort_t  mQuality;  // quality of this information (percentage of charge produced by mIdTruth)
	UShort_t  mTrackKey;

ClassDef(StMuMtdHit,1)

};

#endif
