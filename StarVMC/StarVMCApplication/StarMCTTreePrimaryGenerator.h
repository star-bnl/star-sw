// $Id: StarMCTTreePrimaryGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarMCTTreePrimaryGenerator.h,v $
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

#ifndef StarMCTTreePrimaryGenerator_h
#define StarMCTTreePrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TH1.h"
#include "TTree.h"
#include "TString.h"
#include "TTreeIter.h"
class StarMCTTreePrimaryGenerator : public StarMCPrimaryGenerator  {
 public:
  StarMCTTreePrimaryGenerator(TString mode="pp:W:510", Int_t tune=320);
  virtual ~StarMCTTreePrimaryGenerator() {}
  
  static StarMCTTreePrimaryGenerator* Instance() {return (StarMCTTreePrimaryGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void GeneratePrimaries();
  virtual void GeneratePrimaries(const TVector3& v);
  virtual void SetGenerator(TString mode="pp:W:510", Int_t tune=320);
  void SetSpread(Double_t xs = 0.15, Double_t ys = 0.15, Double_t zs = 42.0) { gSpreadX = xs; gSpreadY = ys; gSpreadZ = zs;}
 private:
  TTreeIter *fTreeIter;
  TTree     *fTree;
  void GeneratePrimary();
  void PreSet();
  TH1 *fPVX, *fPVY, *fPVZ, *fPVxyError; 
  Double_t gSpreadX, gSpreadY, gSpreadZ;
  ClassDef(StarMCTTreePrimaryGenerator,1)  //StarMCTTreePrimaryGenerator
};
#endif //StarMCTTreePrimaryGenerator_h

