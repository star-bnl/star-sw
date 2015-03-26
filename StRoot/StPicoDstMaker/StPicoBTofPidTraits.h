#ifndef StPicoBTofPidTraits_hh
#define StPicoBTofPidTraits_hh

#include "TObject.h"
#include "StThreeVectorF.hh"
class StMuTrack;

class StPicoBTofPidTraits : public TObject{
 public:
  StPicoBTofPidTraits();
  StPicoBTofPidTraits(const StMuTrack *, const StMuTrack*, const Int_t);
  ~StPicoBTofPidTraits();
  virtual void Print(const Char_t *option = "") const;

  Int_t   btofCellId() const     { return (Int_t)mBTofCellId; }
  Int_t   btofMatchFlag() const  { return (Int_t)mBTofMatchFlag; }
  Float_t btof() const           { return (Float_t)mBTof/1000.; }
  Float_t btofBeta() const       { return (Float_t)mBTofBeta/20000.; }
  Float_t btofYLocal() const     { return (Float_t)mBTofYLocal/1000.; }
  Float_t btofZLocal() const     { return (Float_t)mBTofZLocal/1000.; }  
  StThreeVectorF btofHitPos() const { return StThreeVectorF(mBTofHitPosX/100., mBTofHitPosY/100., mBTofHitPosZ/100.); }  

 private:
  Short_t  mTrackIndex;       // Index to the associated track in the event
  Short_t  mBTofCellId;       // (tray-1)*192+(module-1)*6+(cell-1): -1 - no match
  UChar_t  mBTofMatchFlag;    // 0 - no match, 1 - one-to-one, 2 - one-to-multiple
  UShort_t mBTof;             // time-Of-Flight * 1000 in ns
  UShort_t mBTofBeta;         // beta * 20000
  Short_t  mBTofYLocal;       // ylocal * 1000
  Short_t  mBTofZLocal;       // zlocal * 1000
  Short_t  mBTofHitPosX;      // projected hit position X * 100
  Short_t  mBTofHitPosY;      // projected hit position Y * 100
  Short_t  mBTofHitPosZ;      // projected hit position Z * 100

  ClassDef(StPicoBTofPidTraits,1);
};

#endif
