// $Id: StarMCPrimaryGenerator.h,v 1.3 2011/02/11 16:12:52 fisyak Exp $
// $Log: StarMCPrimaryGenerator.h,v $
// Revision 1.3  2011/02/11 16:12:52  fisyak
// Fixes for gcc451
//
// Revision 1.2  2009/03/31 22:43:11  fisyak
// comment out not used  variables
//
// Revision 1.1.1.1  2008/12/10 20:45:53  fisyak
// Merge with macos version
//
// Revision 1.3  2005/06/09 20:13:47  fisyak
// It looks like that all hits in place (calorimeters have to be check for volumeid)
//
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef Star_PRIMARY_GENERATOR_H
#define Star_PRIMARY_GENERATOR_H
#include <assert.h>
#include <stdio.h> 
#include "Stiostream.h"
#include "TString.h"
#include "TMath.h"
#include "TVector3.h"
#include "StarStack.h"
#include "TRandom.h"
#include "TPDGCode.h"
#include "TDatabasePDG.h"

class StarMCPrimaryGenerator : public TObject {
 public:
  StarMCPrimaryGenerator(StarStack* stack = 0) : TObject(), fStarStack(stack), fIsRandom(false), fNofPrimaries(0), 
    fOption(""), fDebug(0), fId(0), fOrigin(), fSigmasOrigin() {fgInstance = this;}
  virtual ~StarMCPrimaryGenerator() {}
  static StarMCPrimaryGenerator* Instance() {return fgInstance;}
  void  SetIsRandom(Bool_t isRandomGenerator) { fIsRandom = isRandomGenerator; }
  void  SetNofPrimaries(Int_t nofPrimaries)   { fNofPrimaries = nofPrimaries; }
  void  SetStack(StarStack *stack)      { fStarStack = stack;}
  void  SetOption(const Char_t *opt)          { fOption = opt;}
  void  SetDebug(Int_t m)                     { fDebug = m;}
  void  SetOrigin(Double_t x, Double_t y, Double_t z) {fOrigin = TVector3(x,y,z);}
  void  SetOrigin(const TVector3 &xyz)        { fOrigin = xyz;}
  void  SetSigmasOrigin(Double_t sigma_x, Double_t sigma_y, Double_t sigma_z) {fSigmasOrigin = TVector3(sigma_x,sigma_y,sigma_z);}
  void  SetSigmasOrigin(const TVector3 &xyz)  { fSigmasOrigin = xyz;}
  Int_t GetNofPrimaries()                     { return fNofPrimaries;}
  const Option_t* GetOption() const           { return fOption.Data();}
  StarStack *GetStack()                 { return fStarStack;}
  Int_t Debug()                               { return fDebug;}
  TVector3 &GetOrigin()                       { return fOrigin;}
  TVector3 &GetSigmasOrigin()                 { return fSigmasOrigin;}
  virtual void GeneratePrimaries() {}
  virtual void GeneratePrimaries(const TVector3& /* origin */) {}
  virtual void Print(Option_t *option="") const;
 protected:
   
  static StarMCPrimaryGenerator *fgInstance;
  StarStack       *fStarStack;    
  Bool_t            fIsRandom;
  Int_t             fNofPrimaries;
  TString           fOption;  
  Int_t             fDebug;
  Int_t             fId;
  TVector3          fOrigin;
  TVector3          fSigmasOrigin;
  ClassDef(StarMCPrimaryGenerator,1)  //StarMCPrimaryGenerator
};
#endif //Star_PRIMARY_GENERATOR_H

