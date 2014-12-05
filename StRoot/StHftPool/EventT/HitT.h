#ifndef HitT_h
#define HitT_h

#include <string.h>
#include "TObject.h"


class HitT : public TObject
{
public:

   Char_t start;
   Int_t Id;
   UInt_t nRawHits;  // cluseter size
   Double32_t xG, yG, zG;    // hit Global from StEvent
   Double32_t xL, yL, zL;    // hit in Ladder CS
   UInt_t     index2Track;
   Char_t end;

   HitT(Int_t Id = 0,
        Double32_t X = 0, Double32_t Y = 0, Double32_t Z = 0,
        Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
      memset(&start, 0, &end - &start);
      SetId(Id); Set(X, Y, Z, XL, YL, ZL);
   }

   virtual ~HitT() {}
   void Clear(Option_t *option = "") {if (option); memset(&start, 0, &end - &start);}
   void Set(Double32_t X, Double32_t Y, Double32_t Z,
            Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0)
   {
      xG = X; yG = Y; zG = Z;
      xL = XL; yL = YL; zL = ZL;
   }
   void SetL(Double32_t X, Double32_t Y, Double32_t Z) {xL = X; yL = Y; zL = Z;}  // set coordinates in Ladder frame
   void SetId(Int_t id) {Id = id;}
   void SetNRawHits(UInt_t n) {nRawHits = n;}
   void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0], xyzG[1], xyzG[2], xyzL[0], xyzL[1], xyzL[2]);}

   void SetIndex2Track(Int_t index) {index2Track = index;}

   ClassDef(HitT, 1)
};

#endif
