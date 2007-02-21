#ifndef TrackParameterDeriv_h
#define TrackParameterDeriv_h
#include "TNumDeriv.h"
#include "TGeoMatrix.h"
#include "StarRoot/THelixTrack.h"

class TrackParameterDeriv : public TNumDeriv {
 public: 
  TrackParameterDeriv(const THelixTrack *HelixI = 0, const TGeoHMatrix* Matrix = 0) : 
  TNumDeriv("TrackParameters"), fHelixI(HelixI), fMatrix(Matrix), fI(0), fJ(0) {}
  ~TrackParameterDeriv() {}
  void SetIJ(Int_t I, Int_t J) {
    if (I >=0 && I < 2) fI = I; 
    if (J >=0 && J < 6) fJ = J;
  }
  virtual Double_t Fcn(Double_t  add=0.);
  Double_t GetU()  const {return fLocalXYZ[0];}
  Double_t GetV()  const {return fLocalXYZ[1];}
  Double_t GettU() const {return fLocalDir[0]/fLocalDir[2];}
  Double_t GettV() const {return fLocalDir[1]/fLocalDir[2];}
  const Double_t *GetXYZ() const {return fGlobalXYZ;}
  const Double_t *GetXYZL() const {return fLocalXYZ;}
  const Double_t *GetDir() const {return fGlobalDir;}
  const THelixTrack *GetHelix()   const {return fHelixI;}
  const TGeoHMatrix *GetHMatrix() const {return fMatrix;}
 private:
  const THelixTrack *fHelixI;
  const TGeoHMatrix *fMatrix;
  Int_t        fI;   // uv[2]
  Int_t        fJ;   // xyz[3], TanL, Phi, Rho [6]
  Double_t fLocalXYZ[3];
  Double_t fLocalDir[3];
  Double_t fGlobalXYZ[3];
  Double_t fGlobalDir[3];
};
#endif
