#ifndef __StVertexT_h__
#define __StVertexT_h__
#include "StMuDstVtxT.h"
class StVertexT : public StMuDstVtxT {
public:
  StVertexT(Double_t x = 0, Double_t y = 0, Double_t z = 0, 
	  Double_t sigma_x = 0, Double_t sigma_y = 0, Double_t sigma_z = 0,
	  Int_t multU = 0, Int_t mult = 0, Int_t multW = 0, Int_t multE = 0, Int_t q = 0, Int_t r = 0,
	  Int_t ndf = 0, Double_t prob = 0, Double_t chi2 = 0,
	  Short_t idTruth = 0, Short_t Qual = 0, Int_t idParentTk = 0) : 
    StMuDstVtxT(x,y,z,sigma_x,sigma_y,sigma_z,multU,mult,multW,multE,q,r,idTruth,Qual,idParentTk),
    fNDF(ndf), fProb(prob), fChi2(chi2),
    fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {}
  StVertexT(StMuDstVtxT &dst, Int_t ndf = 0, Double_t prob = 0, Double_t chi2 = 0) : 
    StMuDstVtxT(dst), fNDF(ndf), fProb(prob), fChi2(chi2),
    fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {} 
  StVertexT(StKFVertex &v) : StMuDstVtxT(v.Vertex().GetX(),
				     v.Vertex().GetY(),
				     v.Vertex().GetZ(),
				     TMath::Sqrt(v.Vertex().GetCovariance(0,0)),
				     TMath::Sqrt(v.Vertex().GetCovariance(1,1)),
				     TMath::Sqrt(v.Vertex().GetCovariance(2,2)),
				     -1, v.NoTracks(), -1, v.MultW(), v.MultE(), v.Q(), 0,
				     v.IdTruth(), v.QaTruth(), v.IdParentTk()), 
			   fNDF(v.Vertex().GetNDF()),fProb(TMath::Prob(v.Vertex().GetChi2(),v.Vertex().GetNDF())),
			   fChi2(v.Vertex().GetChi2()),
			   fiMuDst(0), fChi2MuDst(0), fiKF(0), fChi2KF(0) {
    fTimeMc = v.TimeMc(); 
    fXyzMc = v.XyzMc(); 
    fNoDaughtersMc = v.NoDaughtersMc(); 
    fgePidMc = v.gePidMc();
  }
  virtual ~StVertexT() {}
  void SetMuDst(Int_t i, Double_t p) {fiMuDst = i; fChi2MuDst = p;}
  void SetKF(Int_t i, Double_t p) {fiKF = i; fChi2KF = p;}
private:
  Int_t    fNDF;
  Double_t fProb;
  Double_t fChi2;
  Int_t    fiMuDst; // KFVertex best match with MuDst
  Double_t fChi2MuDst; // chi2 for the above match
  Int_t    fiKF;    // KFVertex best match with KFVertex in (x,y)
  Double_t fChi2KF; // chi2 for the above match
  ClassDef(StVertexT,1)
};
#endif
