#ifndef StPicoBTOWHit_hh
#define StPicoBTOWHit_hh

class StPicoDst;

#include "TObject.h"
#include "stdio.h"

class StPicoBTOWHit : public TObject {
 public:
  StPicoBTOWHit();
  ~StPicoBTOWHit();
  StPicoBTOWHit(int, int, float);
  void    Clear(const Option_t *opt="");
  virtual void Print(const Char_t *option = "") const;  ///< Print trigger info
 
  Int_t   id() const             { return (Int_t)mId; }
  Int_t   adc() const            { return (Int_t)mAdc; }
  Float_t energy() const         { return (Float_t)mE/1000.; }

 protected:
  UShort_t mId;    // towerId 1-4800
  UShort_t mAdc;   // adc
  Short_t  mE;     // Energy * 1000;
  

  friend class StPicoDst;

  ClassDef(StPicoBTOWHit, 1)
};

#endif
