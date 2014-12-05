#ifndef TrackT_h
#define TrackT_h

#include <string.h>
#include "TObject.h"
#include "TMath.h"


class TrackT : public TObject
{
public:
   Char_t          beg;
   Double32_t      fPxDca;        //signed - global
   Double32_t      fPyDca;
   Double32_t      fPzDca;

   Double32_t      fPPx;        //signed - primary
   Double32_t      fPPy;
   Double32_t      fPPz;

   Double32_t      fOriginXDca;
   Double32_t      fOriginYDca;
   Double32_t      fOriginZDca;

   Double32_t      fPx;
   Double32_t      fPy;
   Double32_t      fPz;

   Double32_t      fOriginX;
   Double32_t      fOriginY;
   Double32_t      fOriginZ;

   Int_t           fNpoint;       //Number of fitted points for this track * q

   Double32_t      fTpcHitX[45];
   Double32_t      fTpcHitY[45];
   Double32_t      fTpcHitZ[45];

   Double32_t      fNsigmaPi;
   Double32_t      fDca2D;
   Double32_t      fDca3D;

   UInt_t          fPxlHitPattern; // sensorId-hit3 * 1000000 + sensorId-hit2 * 1000 + sensorId-hit1
   UInt_t          fIstHitPattern; // sensorId-hit2 * 1000 + sensorId-hit1
   UInt_t          fSsdHitPattern; // sensorId-hit2 * 1000 + sensorId-hit1

   Double32_t      fPxlHitX[3];
   Double32_t      fPxlHitY[3];
   Double32_t      fPxlHitZ[3];

   Double32_t      fIstHitX[2];
   Double32_t      fIstHitY[2];
   Double32_t      fIstHitZ[2];

   Double32_t      fSsdHitX[2];
   Double32_t      fSsdHitY[2];
   Double32_t      fSsdHitZ[2];

   Char_t          end;

public:
   TrackT() { Clear(); }
   virtual ~TrackT() {Clear();}
   void          Clear(Option_t *option = "") {if (option); memset(&beg, 0, &end - &beg);}
   Double32_t    GetPx()      { return fPx;}
   Double32_t    GetPy()      { return fPy;}
   Double32_t    GetPz()      { return fPz;}
   Double32_t    GetPxDca()   { return fPxDca;}
   Double32_t    GetPyDca()   { return fPyDca;}
   Double32_t    GetPzDca()   { return fPzDca;}
   Double32_t    GetPPx()     { return fPPx;}
   Double32_t    GetPPy()     { return fPPy;}
   Double32_t    GetPPz()     { return fPPz;}
   Double32_t    GetOriginX() { return fOriginX; }
   Double32_t    GetOriginY() { return fOriginY; }
   Double32_t    GetOriginZ() { return fOriginZ; }
   Double32_t    GetOriginXDca() { return fOriginXDca; }
   Double32_t    GetOriginYDca() { return fOriginYDca; }
   Double32_t    GetOriginZDca() { return fOriginZDca; }
   UInt_t        GetNpoint()  { return TMath::Abs(fNpoint); }
   Short_t       GetCharge()  { return (Short_t) TMath::Sign(1, fNpoint); }
   Double32_t    GetNsigmaPi() { return fNsigmaPi; }
   Double32_t    GetDca2D()   { return fDca2D; }
   Double32_t    GetDca3D()   { return fDca3D; }
   UInt_t        GetPxlHitPattern() { return fPxlHitPattern; }
   UInt_t        GetIstHitPattern() { return fIstHitPattern; }
   UInt_t        GetSsdHitPattern() { return fSsdHitPattern; }

   virtual void SetPxPyPz(Double32_t px, Double32_t py, Double32_t pz)  { fPx = px; fPy = py; fPz = pz; }
   virtual void SetDcaPxPyPz(Double32_t px, Double32_t py, Double32_t pz)  { fPxDca = px; fPyDca = py; fPzDca = pz; }
   virtual void SetPPxPyPz(Double32_t px, Double32_t py, Double32_t pz) { fPPx = px; fPPy = py; fPPz = pz; }
   virtual void SetOxOyOz(Double32_t ox, Double32_t oy, Double32_t oz)  { fOriginX = ox; fOriginY = oy; fOriginZ = oz; }
   virtual void SetDcaOxOyOz(Double32_t ox, Double32_t oy, Double32_t oz)  { fOriginXDca = ox; fOriginYDca = oy; fOriginZDca = oz; }

   virtual void SetNpoint(UInt_t p, Int_t q)     { fNpoint = p * q; }
   virtual void SetNsigmaPi(Double32_t ns) { fNsigmaPi = ns; }
   virtual void SetDca2D(Double32_t dca) { fDca2D = dca; }
   virtual void SetDca3D(Double32_t dca) { fDca3D = dca; }
   virtual void SetPxlHitPattern(UInt_t n) { fPxlHitPattern = n; }
   virtual void SetIstHitPattern(UInt_t n) { fIstHitPattern = n; }
   virtual void SetSsdHitPattern(UInt_t n) { fSsdHitPattern = n; }

   virtual void SetPxlHit(UInt_t i, Double32_t *xyz) { fPxlHitX[i] = xyz[0]; fPxlHitY[i] = xyz[1]; fPxlHitZ[i] = xyz[2]; }
   virtual void SetIstHit(UInt_t i, Double32_t *xyz) { fIstHitX[i] = xyz[0]; fIstHitY[i] = xyz[1]; fIstHitZ[i] = xyz[2]; }
   virtual void SetSsdHit(UInt_t i, Double32_t *xyz) { fSsdHitX[i] = xyz[0]; fSsdHitY[i] = xyz[1]; fSsdHitZ[i] = xyz[2]; }
   virtual void SetTpcHit(UInt_t i, Double32_t *xyz) { fTpcHitX[i] = xyz[0]; fTpcHitY[i] = xyz[1]; fTpcHitZ[i] = xyz[2]; }

   ClassDef(TrackT, 1)
};

#endif
