#ifndef StPicoBTowHit_h
#define StPicoBTowHit_h

#include "TObject.h"

//_________________
class StPicoBTowHit : public TObject {

 public:
  //Default constructor
  StPicoBTowHit();
  //Constructor that takes id, ADC and energy
  StPicoBTowHit(int id, int adc, float e);
  //Copy constructor
  StPicoBTowHit(const StPicoBTowHit &hit);
  //Destructor
  virtual ~StPicoBTowHit();
  //Print tower information
  virtual void Print(const Char_t* option = "") const;

  Int_t   id() const;
  Int_t   adc() const;
  Float_t energy() const;

 protected:

  UShort_t mId;    // towerId 1-4800
  UShort_t mAdc;   // adc
  Short_t  mE;     // Energy * 1000;

  ClassDef(StPicoBTowHit, 1)
};

inline Int_t   StPicoBTowHit::id() const { return (Int_t)mId; }
inline Int_t   StPicoBTowHit::adc() const { return (Int_t)mAdc; }
inline Float_t StPicoBTowHit::energy() const { return (Float_t)mE / 1000.; }
#endif
