//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Mon Feb  6 14:07:01 2006 by ROOT version 5.09/01
// from TTree T/TTree with SVT + SSD hits and tracks
// found on file: /star/data07/calib/fisyak/SvtSsdAlignment/TpcSvtSsd.dev/EventAll1.root
//////////////////////////////////////////////////////////

#ifndef TT_h
#define TT_h

#include "TROOT.h"
#include "TChain.h"
#include "TFile.h"
#include "TRef.h"
#include "Riostream.h"
#include "TBaseK.h"
#include "Xdcor.h"

class TreeClass: public TBase {
private:

 TreeClass(const TreeClass &f):TBase() {}

 public :
  Xdcor           X1;
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
  
  TreeClass();
  TreeClass(TFile *f);
  TreeClass(TTree *tree);
  virtual ~TreeClass(){}
  virtual void     Loop() {Loop(0);}
  virtual void     Loop(Int_t Nevents);
  virtual void     SetOutFileName(const Char_t *name="Out.root") {fOutFileName = name;}
  virtual void     SetuMinMax(Double_t min, Double_t max) {uMin = min; uMax = max;}
  virtual void     SetvMinMax(Double_t min, Double_t max) {vMin = min; vMax = max;}
  virtual void     SetDipCut(Double_t cut)                {DipCut = cut;}
  virtual void     SetVertexZCut(Double_t cut)            {VertexZCut = cut;}
  virtual void     SetNoWafers()                          {AllWafers = kFALSE;}
  virtual void     SetRCut(Double_t r=0.5)                {rCut = r;}
  virtual void     SetLaddersInGlobal(Bool_t p=kTRUE)     {LaddersInGlobal = p;}
  virtual void     SetMinNoFitPoints(Int_t k = 25)        {minNoFitPoints = k;}
  virtual void     SetSsd(Bool_t k = kTRUE)               {UseSsd = k;}
  virtual void     SetSvt(Bool_t k = kTRUE)               {UseSvt = k;}
  Double_t         GetRCut()                              {return rCut;}
  
  static TreeClass *Create();
  
  virtual void     MyInit(TTree *my_tree) {
    uMin = uMax = vMin = vMax = DipCut = VertexZCut = 0; 
    rCut = 0.5;
    minNoFitPoints = 0;
    AllWafers = kTRUE;
    UseSsd = kFALSE;
    UseSvt = kFALSE;
    LaddersInGlobal = kFALSE;
    Init(my_tree);
  }
//  ClassDef(TreeClass,1);
};
//------------------------------------------------------------------------------

#endif


