// $Id: StarMCPrimaryGenerator.h,v 1.2 2005/05/03 15:42:14 fisyak Exp $
// $Log: StarMCPrimaryGenerator.h,v $
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef Star_PRIMARY_GENERATOR_H
#define Star_PRIMARY_GENERATOR_H
#include "TString.h"
#include "TMath.h"
#include "TVirtualMCApplication.h"
#include "TVector3.h"
class TVirtualMCStack;

class StarDetectorConstruction;

class StarMCPrimaryGenerator : public TObject {
 public:
  static StarMCPrimaryGenerator* Instance() {return fgInstance;}
    virtual void GeneratePrimaries(const TVector3& v);
    virtual void GeneratePrimaries();
    virtual void SetGenerator(Int_t nprim=1, Int_t Id=13, 
			      Double_t pT_min = 0,Double_t pT_max = 1000,
			      Double_t Eta_min=-10, Double_t Eta_max=10, 
			      Double_t Phi_min = 0, Double_t Phi_max= 2*TMath::Pi(), 
			      Double_t Z_min=0, Double_t Z_max=0, const Char_t *option = "G");
    void  SetIsRandom(Bool_t isRandomGenerator) { fIsRandom = isRandomGenerator; }
    void  SetNofPrimaries(Int_t nofPrimaries)   { fNofPrimaries = nofPrimaries; }
    void  SetStack(TVirtualMCStack *stack)      { fStack = stack;}
    Int_t GetNofPrimaries()                     { return fNofPrimaries;}
    TVirtualMCStack *GetStack()                 { return fStack;}
    TVector3 &GetOrigin()                       { return fOrigin;}
    StarMCPrimaryGenerator(TVirtualMCStack* stack); 
    StarMCPrimaryGenerator(Int_t nprim=1,       Int_t Id=6, 
			   Double_t pT_min = 0,  Double_t pT_max = 10,
			   Double_t Eta_min=-10,   Double_t Eta_max=10, 
			   Double_t Phi_min = 0, Double_t Phi_max= 2*TMath::Pi(), 
			   Double_t Z_min=0,     Double_t Z_max=0, 
			   const Char_t *option = "G"): 
    TObject(), fStack(0), fIsRandom(false) {
    SetGenerator(nprim, Id, pT_min, pT_max, Eta_min, Eta_max, Phi_min, Phi_max, 
		 Z_min, Z_max, option);}
    virtual ~StarMCPrimaryGenerator() {}
  private:
    static StarMCPrimaryGenerator *fgInstance;
    void GeneratePrimary(const TVector3& origin);
    TVirtualMCStack  *fStack;    
    Bool_t            fIsRandom;
    Int_t             fNofPrimaries;
    Int_t             fId;
    Double_t          fpT_min, fpT_max, fEta_min, fEta_max, fPhi_min, fPhi_max, fZ_min, fZ_max;
    TString           fOption;  
    TVector3          fOrigin;
  ClassDef(StarMCPrimaryGenerator,1)  //StarMCPrimaryGenerator
};
#endif //Star_PRIMARY_GENERATOR_H

