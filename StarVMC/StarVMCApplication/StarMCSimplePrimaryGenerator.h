// $Id: StarMCSimplePrimaryGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarMCSimplePrimaryGenerator.h,v $
// Revision 1.1.1.1  2008/12/10 20:45:52  fisyak
// Merge with macos version
//
// Revision 1.1  2005/06/09 20:13:47  fisyak
// It looks like that all hits in place (calorimeters have to be check for volumeid)
//
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef StarMCSimplePrimaryGenerator_h
#define StarMCSimplePrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"

class StarMCSimplePrimaryGenerator : public StarMCPrimaryGenerator  {
 public:
  StarMCSimplePrimaryGenerator(StarMCStack* stack) : StarMCPrimaryGenerator() { PreSet(); fStarMcStack = stack; }
  StarMCSimplePrimaryGenerator(Int_t    nprim=1,     Int_t    Id=6, 
			       Double_t pT_min =  0, Double_t pT_max = 10,
			       Double_t Eta_min=-10, Double_t Eta_max=10, 
			       Double_t Phi_min = 0, Double_t Phi_max= 2*TMath::Pi(), 
			       Double_t Z_min=0,     Double_t Z_max=0, 
			       const Char_t *option = "G");
  virtual ~StarMCSimplePrimaryGenerator() {}
  
  static StarMCSimplePrimaryGenerator* Instance() {return (StarMCSimplePrimaryGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void GeneratePrimaries();
  virtual void GeneratePrimaries(const TVector3& v);
  virtual void SetGenerator(Int_t nprim=1, Int_t Id=13, 
			    Double_t pT_min = 0,Double_t pT_max = 1000,
			    Double_t Eta_min=-10, Double_t Eta_max=10, 
			    Double_t Phi_min = 0, Double_t Phi_max= 2*TMath::Pi(), 
			    Double_t Z_min=0, Double_t Z_max=0, const Char_t *option = "G");
 private:
  void GeneratePrimary();
  void PreSet();
  Double_t fpT_min, fpT_max, fEta_min, fEta_max, fPhi_min, fPhi_max, fZ_min, fZ_max;
  ClassDef(StarMCSimplePrimaryGenerator,1)  //StarMCSimplePrimaryGenerator
};
#endif //StarMCSimplePrimaryGenerator_h

