#ifndef __StKFTrack_h__
#define __StKFTrack_h__
// $Id: StKFTrack.h,v 2.2 2012/06/11 15:33:41 fisyak Exp $
#include "Riostream.h"
#include "TObject.h"
#include "KFParticle.h"
#include "TRSymMatrix.h"
#include "StAnneling.h"
#include "TString.h"
class StKFTrack;
std::ostream&  operator<<(std::ostream& os,  const StKFTrack& p);
class StKFTrack : public TObject {
public:
  StKFTrack(Int_t k = -1, KFParticle *particle = 0, Double_t chi2=-1, Int_t iWE = 0);
  virtual     ~StKFTrack() {}
  void         SetChi2(Double_t chi2=-1);
  void Reset() {fParticle = KFParticle(*fOrigKFParticle);}
  Int_t        K()       const {return fK;}       // index in particle array 
  Double_t     Weight()  const {return fWeight;}  // adaptive weight 
  Double_t     W()       const {return fW;}       // adaptive weigth for multi vertices
  Double_t     Chi2()    const {return fChi2;}
  KFParticle   Particle()const {return fParticle;}// particle with modified covariance matrix accourdingly to weight
  KFParticle  &Particle(){return *&fParticle;}// particle with modified covariance matrix accourdingly to weight
  
  Double_t    &Weight()  {return *&fWeight;}  // adaptive weight 
  Double_t    &W()       {return *&fW;}       // adaptive weigth for multi vertices
  Double_t    &Chi2()    {return *&fChi2;}
  KFParticle *OrigParticle() const {return fOrigKFParticle;} // 
  Bool_t      IsWest()   {return fWestOrEast > 0;}
  Bool_t      IsEast()   {return fWestOrEast < 0;}
  void Print(Option_t *option="") const {if (option) {}; std::cout << *this << std::endl;}
  static Int_t CorrectGePid(Int_t gePid);
private:
  const Int_t fK;       // index in particle array 
  Double_t    fWeight;  // adaptive weight 
  Double_t    fW;       // adaptive weigth for multi vertices
  Double_t    fChi2;
  KFParticle  fParticle;// particle with modified covariance matrix accourdingly to weight
  KFParticle *fOrigKFParticle; // 
  Int_t       fWestOrEast;  
  ClassDef(StKFTrack,0)
};
// $Log: StKFTrack.h,v $
// Revision 2.2  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
#endif
