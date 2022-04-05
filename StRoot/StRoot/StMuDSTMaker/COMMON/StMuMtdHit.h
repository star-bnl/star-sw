#ifndef __StMuMdtHit_hh__
#define __StMuMdtHit_hh__

#include "TObject.h"

using namespace std;

class StMtdHit;
class StMuMtdHit : public TObject {

public:

  StMuMtdHit();
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
  int       index2Primary() const;
  int        index2Global() const;


  void setAssociatedTrackKey(short);
  void setIndex2Primary(int);
  void setIndex2Global(int);
	
 private:

  UChar_t   mBackLeg;
  UChar_t   mModule;
  UChar_t   mCell;
  pair<Double_t,Double_t>  mLeadingEdgeTime;
  pair<Double_t,Double_t>  mTrailingEdgeTime;
	
  UShort_t  mIdTruth;  // simulation associated track id
  UShort_t  mQuality;  // quality of this information (percentage of charge produced by mIdTruth)
  UShort_t  mTrackKey;
  Int_t     mIndex2Primary;
  Int_t     mIndex2Global;


ClassDef(StMuMtdHit,2)

};

inline void StMuMtdHit::setAssociatedTrackKey(short id) { mTrackKey=id; }
inline void StMuMtdHit::setIndex2Primary(int index)     { mIndex2Primary=index; }
inline void StMuMtdHit::setIndex2Global(int index)      { mIndex2Global=index; }

#endif
