// $Id: StarMCMuPrimaryGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarMCMuPrimaryGenerator.h,v $
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

#ifndef StarMCMuPrimaryGenerator_h
#define StarMCMuPrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TDataSet.h"
#include "TFile.h"
#include "TTree.h"
#include "TTreeIter.h"

class StarMCMuPrimaryGenerator : public StarMCPrimaryGenerator {
 public:
 StarMCMuPrimaryGenerator(TTreeIter *MuDstIter, TDataSet *set=0) : fMuDstIter(MuDstIter), fData(set) {}
  virtual ~StarMCMuPrimaryGenerator() {}
  
  static StarMCMuPrimaryGenerator* Instance() {return (StarMCMuPrimaryGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void  GeneratePrimaries();
  virtual void  Skip(Int_t nskip);
  //  virtual void GeneratePrimaries(const TVector3& origin) {}
 private:
  virtual void GeneratePrimaries(const TVector3& /* origin */) {}
  
  //  void GeneratePrimary(const TVector3& origin);
  TTreeIter   *fMuDstIter;
  TDataSet         *fData;
  ClassDef(StarMCMuPrimaryGenerator,1)  //StarMCMuPrimaryGenerator
};
#endif //StarMCMuPrimaryGenerator_h

