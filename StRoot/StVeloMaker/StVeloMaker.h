// $Id: StVeloMaker.h,v 1.3 2003/09/10 19:47:42 perev Exp $
//
#ifndef StVeloMaker_H
#define StVeloMaker_H



//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVeloMaker: Drift velosity and TZero                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
class St_tcl_tphit;

class TH1F;
class TH2F;
class TH2C;
class TCanvas;
class MyHit;
class MyTrk;
class StHitChair;
class StVeloMaker : public StMaker {

 public: 

  StVeloMaker(const char *name="velo");
  virtual       ~StVeloMaker(); 
  
  virtual Int_t  Init();
  virtual Int_t  Make();
          Int_t  Make00();
          Int_t  Make01();
          Int_t  Make02();
          double MakeTrack(int* tr);
          void   FitVtx(int is); 
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt="");
  virtual void   PrintInfo();
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StVeloMaker.h,v 1.3 2003/09/10 19:47:42 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
 private:

// 	Hists
  enum {NTHIST=7};
  TH1          **hAll;	//!
  TH1 		*hZHit; 	//! 
  TH1 		*hZBadHit; 	//! 
  TH1 		*hZVtx[8]; 	//! 
  TH2C          *hPlot;         //!
  TH1 		*hDZ[4]; 	//! 
  TH2C          *hZdZPlot;      //!
  TCanvas       *fC1;		//!
  TCanvas       *fC2;		//!
  TCanvas       *fC3;		//!
  TCanvas       *fC4;		//!

//	real members
  StHitChair    *fHitChair;	//!
  Float_t       *fGate;		//!
  MyHit         *fHits;		//!
  Int_t		 fNHits;	//!
  MyTrk      	*fTrks;		//!
  Int_t      	 fNTrks;	//!
  double         fZLim[2][2];	//!
  double         fV[4][3];	//!
  double         fG[4][9];	//!
  ClassDef(StVeloMaker,0)       
};

#endif
