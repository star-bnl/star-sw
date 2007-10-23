//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Mon Feb  6 14:07:01 2006 by ROOT version 5.09/01
// from TTree T/TTree with SVT + SSD hits and tracks
// found on file: /star/data07/calib/fisyak/SvtSsdAlignment/TpcSvtSsd.dev/EventAll1.root
//////////////////////////////////////////////////////////

#ifndef T_h
#define T_h

#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TRef.h"
#include "Riostream.h"
#include "TBase.h"
class HybridFit_t;
class TT : public TBase {
 public :
  TString         fOutFileName;
  Double_t        uMin, uMax;
  Double_t        vMin, vMax;
  Double_t        DipCut;
  Double_t        VertexZCut;
  Double_t        rCut;
  Bool_t          AllWafers;
  Bool_t          LaddersInGlobal;
  Int_t           minNoFitPoints;
  Bool_t          UseSsd;
  Bool_t          UseSvt;
  Int_t           EastWest; // 1 => East, 2 => West; 0 => Both
  Bool_t          fGlobal;
  Bool_t          fLocal;
  Double_t        dEdxCut;
  Double_t        TpcLengthCut;
  TT(TFile *f) {
    fOutFileName = "Out.root";
    if (f) {
      cout << f->GetName() << " file hase been found" << endl;
      fOutFileName = f->GetName();
      fOutFileName.ReplaceAll("/","_");
      fOutFileName.ReplaceAll(".root","");
      fOutFileName += "Out.root";
      TTree *tree = (TTree*) f->Get("T");
      if (tree) Init(tree);
    else cout << "no TTree found" << endl;
    }
  }
  TT(TTree *tree=0) : TBase(tree)  {
    // if parameter tree is not specified (or zero), connect the file
    // used to generate this class and read the Tree.
    if (tree == 0) {
      TCollection *files = gROOT->GetListOfFiles();
      if (! files) {
	cout << "no root files" << endl;
      } else {
	TFile *f = 0;
	TIter next(files);
	while ( (f = (TFile *) next()) ) {   
	  tree = (TTree*) f->Get("T");
	  if (tree) {
	    cout << f->GetName() << " file hase been found" << endl;
	    fOutFileName = f->GetName();
	    fOutFileName.ReplaceAll("/","_");
	    fOutFileName.ReplaceAll(".root","");
	    fOutFileName += "Out.root";
	    break;
	  }
	}
      }
    }
    if (tree) Init(tree);
  }

  virtual void     Loop() {Loop(0);}
  virtual void     Loop(Int_t Nevents);
  virtual void     Loop4BadAnodes() {Loop4BadAnodes(0);}
  virtual void     Loop4BadAnodes(Int_t Nevents);
  virtual void     MakeNt();
  static  Int_t    IsNotValidHybrid(Int_t barrel, Int_t ladder, Int_t wafer, Int_t hybrid, Int_t run, Double_t anode);
  virtual void     SetOutFileName(const Char_t *name="Out.root") {fOutFileName = name;}
  virtual void     SetuMinMax(Double_t min, Double_t max) {uMin = min; uMax = max;}
  virtual void     SetvMinMax(Double_t min, Double_t max) {vMin = min; vMax = max;}
  virtual void     SetDipCut(Double_t cut) {DipCut = cut;}
  virtual void     SetVertexZCut(Double_t cut) {VertexZCut = cut;}
  virtual void     SetNoWafers() {AllWafers = kFALSE;}
  virtual void     SetRCut(Double_t r=0.5) {rCut = r;}
  virtual void     SetLaddersInGlobal(Bool_t p=kTRUE) {LaddersInGlobal = p;}
  virtual void     SetMinNoFitPoints(Int_t k = 25) {minNoFitPoints = k;}
  virtual void     SetSsd(Bool_t k = kTRUE) {UseSsd = k;}
  virtual void     SetSvt(Bool_t k = kTRUE) {UseSvt = k;}
  virtual void     SetEastWest(Int_t k = 0) {EastWest = k;}
  virtual void     SetdEdxCut(Double_t dEdx=4e-6, Double_t length=40) {dEdxCut = dEdx; TpcLengthCut = length;}
  virtual void     UseGlobal() {fGlobal = kTRUE; fLocal = kFALSE;}
  virtual void     UseLocal() {fLocal = kTRUE; fGlobal = kFALSE;}
  Double_t         GetRCut() {return rCut;}
  virtual void     Init(TTree *tree) {
    uMin = uMax = vMin = vMax = DipCut = VertexZCut = 0; rCut = 0.5;
    minNoFitPoints = 0;
    AllWafers = kTRUE;
    UseSsd = kFALSE;
    UseSvt = kFALSE;
    LaddersInGlobal = kFALSE;
    EastWest = 0;
    dEdxCut = 0;
    TpcLengthCut = 0;
    fGlobal  = kTRUE;
    fLocal  = kTRUE;
    TBase::Init(tree);
  }
  //  ClassDef(TT,1)
};
void TBase::Loop(){}
void TBase::MakeNt(){}
#endif
