#ifndef StPicoMtdHit_hh
#define StPicoMtdHit_hh

class PicoDst;
class StMuMtdHit;
#include "TObject.h"

using namespace std;

class StPicoMtdHit : public TObject {
 public:
  StPicoMtdHit();
  StPicoMtdHit(const StMuMtdHit* hit);
  ~StPicoMtdHit();

  virtual void Print(const Char_t *option = "") const; 
  void   setTriggerFlag(const Int_t flag)  { mTriggerFlag = (UChar_t)flag; }

  Int_t  gChannel()    const { return (Int_t)mgChannel;             }
  Int_t  backleg()     const { return (Int_t)mgChannel/60 + 1;      }
  Int_t  module()      const { return ((Int_t)mgChannel%60)/12 + 1; }
  Int_t  cell()        const { return (Int_t)mgChannel%12;          }
  Int_t  triggerFlag() const { return (Int_t) mTriggerFlag;         }   
  
  pair<Float_t,Float_t> leadingEdgeTime()  const { return mLeadingEdgeTime;  }
  pair<Float_t,Float_t> trailingEdgeTime() const { return mTrailingEdgeTime; }
  pair<Float_t,Float_t> tot() const 
  { return pair<Float_t,Float_t>(mTrailingEdgeTime.first - mLeadingEdgeTime.first, mTrailingEdgeTime.second - mLeadingEdgeTime.second); }
  
 protected:
  Short_t mgChannel; // mgChannel = (backleg-1) * 60 + (module-1) * 12 + cell
  UChar_t mTriggerFlag; // # of hits in the corresponding trigger unit that fired the trigger
  pair<Float_t,Float_t>  mLeadingEdgeTime;
  pair<Float_t,Float_t>  mTrailingEdgeTime;
	
  ClassDef(StPicoMtdHit,1)

};

#endif
