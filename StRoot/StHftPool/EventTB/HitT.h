#ifndef __HIT__
#define __HIT__
#define __USE_GLOBAL__
#include <string.h>
#include "TObject.h"
class StHit;
class HitT : public TObject {
 private:
  Char_t start;
  Int_t Id;
  UInt_t nRawHits;  // cluseter size
  Double32_t xG, yG, zG;    // hit Global from StEvent
  Double32_t xL, yL, zL;    // hit in Ladder CS
  UInt_t     index2Track;
  Int_t ladder;
  Int_t wafer;
  Char_t end;
  Int_t adcP;
  Int_t adcN;
 public:
  HitT(Int_t Id = 0,
      Double32_t X = 0, Double32_t Y = 0, Double32_t Z = 0,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    memset(&start, 0, &end - &start);
    SetId(Id); Set(X,Y,Z,XL,YL,ZL);
  }
  
  virtual ~HitT() {}
  void Clear(Option_t *option="") {if (option); memset(&start, 0, &end - &start);}
  void Set(Double32_t X, Double32_t Y, Double32_t Z,
      Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
    xG = X; yG = Y; zG = Z; 
    xL = XL; yL = YL; zL = ZL;
  }
  void SetL(Double32_t X, Double32_t Y, Double32_t Z) {xL = X; yL = Y; zL = Z;}  // set coordinates in Ladder frame
  void SetId(Int_t id) {Id=id;}
  void SetNRawHits(UInt_t n) {nRawHits=n;}
  void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0],xyzG[1],xyzG[2],xyzL[0],xyzL[1],xyzL[2]);}

  void SetIndex2Track(Int_t index) {index2Track = index;}

  void SetLadderWafer(Int_t lad, Int_t waf){ladder = lad; wafer=waf;}
  void SetADC(Int_t pulseP, Int_t pulseN){adcP = pulseP; adcN=pulseN;}
  
  virtual void Print(Option_t *opt="") const;
  ClassDef(HitT,1)
};
#endif
