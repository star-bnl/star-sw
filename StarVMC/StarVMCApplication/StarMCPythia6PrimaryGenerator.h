// $Id: StarMCPythia6PrimaryGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarMCPythia6PrimaryGenerator.h,v $
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

#ifndef StarMCPythia6PrimaryGenerator_h
#define StarMCPythia6PrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TH1.h"
#include "TPythia6.h"
#include "TString.h"
#include <vector>
class StarMCPythia6PrimaryGenerator : public StarMCPrimaryGenerator  {
 public:
  StarMCPythia6PrimaryGenerator(TString mode="pp:W:510", Int_t tune=320);
  virtual ~StarMCPythia6PrimaryGenerator() {}
  
  static StarMCPythia6PrimaryGenerator* Instance() {return (StarMCPythia6PrimaryGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void GeneratePrimaries();
  virtual void SetGenerator(TString mode="pp:W:510", Int_t tune=320);
  void SetSpread(Double_t xs = 0.15, Double_t ys = 0.15, Double_t zs = 42.0) { gSpreadX = xs; gSpreadY = ys; gSpreadZ = zs;}
 private:
  TPythia6 *fPythia6;
  void GeneratePrimary();
  void PreSet();
  TH1 *fPVX, *fPVY, *fPVZ, *fPVxyError; 
  Double_t gSpreadX, gSpreadY, gSpreadZ;
  ClassDef(StarMCPythia6PrimaryGenerator,1)  //StarMCPythia6PrimaryGenerator
};
#endif //StarMCPythia6PrimaryGenerator_h

