#ifndef HitMatchT_h
#define HitMatchT_h

#include <string.h>
#include "TObject.h"

class HitMatchT : public TObject
{
private:
   Char_t start;
   UInt_t index2Track;
   UInt_t index2Hit;
   UInt_t detId;  // hit detector Id
   UInt_t nRawHits;
   Double32_t xGP, yGP, zGP;
   Double32_t xLP, yLP, zLP;  // aka (u,w,v)
   Double32_t tuP, tvP;
   Double32_t xG, yG, zG;    // hit Global from StEvent
   Double32_t xL, yL, zL;    // hit in Ladder CS
   Double32_t pT, eta, phi;  // track mom at origin (dcaGeometry)
   Double32_t ox, oy, oz;    // origin (dcaGeometry)
   Int_t      npoint;        // npoint
   Double32_t firstPointR;   // first measured point R
   Double32_t firstPointZ;   // first measured point Z
   Char_t end;

public:

   HitMatchT() {
      memset(&start, 0, &end - &start);
   }
   virtual ~HitMatchT() {}

   void Clear(Option_t *option = "") {if (option); memset(&start, 0, &end - &start);}
   void Set(Double32_t X, Double32_t Y, Double32_t Z,
            Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
      xG = X; yG = Y; zG = Z;
      xL = XL; yL = YL; zL = ZL;
   }
   void SetPred(Double32_t X, Double32_t Y, Double32_t Z,
                Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
      xGP = X; yGP = Y; zGP = Z;
      xLP = XL; yLP = YL; zLP = ZL;
   }
   void SetDetId(UInt_t id) {detId = id;}
   void SetNRawHits(UInt_t n) {nRawHits = n;}
   void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0], xyzG[1], xyzG[2], xyzL[0], xyzL[1], xyzL[2]);}
   void SetPred(Double32_t *xyzG, Double32_t *xyzL) {SetPred(xyzG[0], xyzG[1], xyzG[2], xyzL[0], xyzL[1], xyzL[2]);}
   void SettuvPred(Double32_t tu, Double32_t tv) {tuP = tu; tvP = tv;}

   void SetIndex2Track(UInt_t index) {index2Track = index;}
   void SetIndex2Hit(UInt_t index) {index2Hit = index;}

   void SetTrackMom(Double32_t t_pT, Double32_t t_eta, Double32_t t_phi) {pT = t_pT; eta = t_eta; phi = t_phi;}
   void SetTrackOrigin(Double32_t x, Double32_t y, Double32_t z) {ox = x; oy = y; oz = z;}
   void SetTrackNpoint(Int_t n) {npoint = n;}
   void SetTrackFirstPointR(Double32_t r) {firstPointR = r;}
   void SetTrackFirstPointZ(Double32_t z) {firstPointZ = z;}

   ClassDef(HitMatchT, 1)
};

#endif
