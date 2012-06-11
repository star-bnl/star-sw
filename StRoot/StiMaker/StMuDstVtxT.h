#ifndef __StMuDstVtxT_h__
#define __StMuDstVtxT_h__
#include "Riostream.h"
#include "TObject.h"
#include "TVector3.h"
#include "StKFTrack.h"
class StMuDstVtxT;
std::ostream&  operator<<(std::ostream& os,  const StMuDstVtxT& v);
class StMuDstVtxT : public TObject {
public:
  StMuDstVtxT(Double_t x = 0, Double_t y = 0, Double_t z = 0, 
	    Double_t sigma_x = 0, Double_t sigma_y = 0, Double_t sigma_z = 0,
	    Int_t multU = 0, Int_t mult = 0, Int_t multC = 0, Int_t multW = 0, 
	    Int_t multE = 0, Int_t q = 0, Int_t r = 0,
	    Short_t idTruth = 0, Short_t Qual = 0, Int_t idParentTk = 0) :
    fMultU(multU), fMult(mult), fMultC(multC), fMultW(multW), fMultE(multE), fQ(q), fRank(r),
    fXyz(x,y,z), fSigmaXyz(sigma_x,sigma_y,sigma_z),
    fIdTruth(idTruth), fQuality(Qual), fIdParentTk(idParentTk), fTimeMc(0), fNoDaughtersMc(0), fgePidMc(0)  {}
  virtual ~StMuDstVtxT() {}
  TVector3 &Xyz()            {return *&fXyz;}
  TVector3 &SigmaXyz()       {return *&fSigmaXyz;}
  TVector3  Xyz() const      {return fXyz;}
  TVector3  SigmaXyz() const {return fSigmaXyz;}
  TVector3 &XyzMc()          {return *&fXyzMc;}
  TVector3  XyzMc() const    {return fXyzMc;}
  Int_t     Mult()     const {return fMult;}
  Int_t     MultU()    const {return fMultU;}
  Int_t     MultC()    const {return fMultC;}
  Int_t     MultW()    const {return fMultW;}
  Int_t     MultE()    const {return fMultE;}
  Int_t     Q()        const {return fQ;}
  Int_t     Rank()     const {return fRank;}
  Short_t   IdTruth()  const {return fIdTruth;}
  Short_t   QaTruth()  const {return fQuality;}
  Int_t     IdParentTk() const {return fIdParentTk;}
  Float_t   TimeMc()   const {return fTimeMc;}
  Int_t     NoDaughtersMc() const {return fNoDaughtersMc;}
  Int_t     gePidMc()  const {return fgePidMc;}
  void Print(Option_t *option="") const {if (option) {}; std::cout << *this << std::endl;}
  void SetMc(Int_t NoMuMcVertex = 0, Int_t NoMuMcTrack = 0, const Float_t *time = 0,
	     const Float_t *x = 0,const Float_t *y = 0,const Float_t *z = 0,
	     const Int_t *NoDaughters = 0,const Int_t *IdParTrk = 0,const Int_t *gePid = 0) {
      Int_t kv = IdTruth();
      if (kv > 0 && kv <= NoMuMcVertex) {
	if (time && x && y && z && NoDaughters && IdParTrk) {
	  fTimeMc = 1e9*time[kv-1];
	  fXyzMc = TVector3(x[kv-1],y[kv-1],z[kv-1]);
	  fNoDaughtersMc = NoDaughters[kv-1];
	  Int_t kvp = IdParTrk[kv-1];
	  if (kvp > 0 && kvp <= NoMuMcTrack) {
	    fgePidMc = StKFTrack::CorrectGePid(gePid[kvp-1]);
	  }
	}
      }
  }
protected:
  Int_t fMultU, fMult, fMultC, fMultW, fMultE;
  Int_t fQ; // charge
  Int_t fRank; // MC=>0
  TVector3 fXyz;
  TVector3 fSigmaXyz;
  Short_t  fIdTruth;
  Short_t  fQuality;
  Int_t    fIdParentTk;
  Float_t  fTimeMc;
  TVector3 fXyzMc;
  Int_t    fNoDaughtersMc;
  Int_t    fgePidMc;
  ClassDef(StMuDstVtxT,1)
};
#endif
