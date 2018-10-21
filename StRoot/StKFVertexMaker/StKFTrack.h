#ifndef __StKFTrack_h__
#define __StKFTrack_h__
// $Id: StKFTrack.h,v 2.3 2015/12/20 01:06:39 fisyak Exp $
#include "Riostream.h"
#include "TObject.h"
#include "KFParticle/KFParticle.h"
#include "TRSymMatrix.h"
#include "StAnneling.h"
#include "TString.h"
class StKFTrack;
std::ostream&  operator<<(std::ostream& os,  const StKFTrack& p);
class StKFTrack : public TObject {
public:
  StKFTrack(const KFParticle *particle, Double_t chi2=-1, Int_t iWE = 0);
  StKFTrack(const StKFTrack& track);
  virtual          ~StKFTrack() {}												              
  void              SetChi2(Double_t chi2=-1);										      
  void Reset()	    {fParticle = KFParticle(*fOrigKFParticle);}								      
  Int_t       	    K()       const {return fOrigKFParticle ? fOrigKFParticle->Id() : -1;}       // index in particle array     
  Double_t    	    Weight()  const {return fWeight;}  // adaptive weight 							      
  Double_t    	    W()       const {return fW;}       // adaptive weigth for multi vertices					      
  Double_t    	    Chi2()    const {return fChi2;}										      
  const KFParticle &Particle()const {return *&fParticle;}// particle with modified covariance matrix accourdingly to weight	      
  KFParticle       &Particle()      {return *&fParticle;}// particle with modified covariance matrix accourdingly to weight	      
  const KFParticle *OrigParticle() const {return fOrigKFParticle;} // 
  Int_t             Id()   const {return fOrigKFParticle->Id();}
  Bool_t            IsWest()   const {return fWestOrEast > 0;}	
  Bool_t      	    IsEast()   const {return fWestOrEast < 0;}	
  Int_t       	    WestOrEast()const {return fWestOrEast;}		
  void        	    NormW(Double_t Norm = -1);			
  void        	    ResetParticle();                                  
  void              Print(Option_t *option="") const {if (option) {}; std::cout << *this << std::endl;}
  void              SetParent(Int_t Id) {((KFParticle *) fOrigKFParticle)->SetParentID(Id);}
  static Int_t      CorrectGePid(Int_t gePid);
private:
  Double_t          fWeight;  // adaptive weight 
  Double_t          fW;       // adaptive weigth for multi vertices
  Double_t          fChi2;
  KFParticle        fParticle;// particle with modified covariance matrix accourdingly to weight
  const KFParticle *fOrigKFParticle; // 
  Int_t             fWestOrEast;  
  ClassDef(StKFTrack,0)
};
// $Log: StKFTrack.h,v $
// Revision 2.3  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.3  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
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
