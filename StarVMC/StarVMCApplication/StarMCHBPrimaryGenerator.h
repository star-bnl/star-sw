// $Id: StarMCHBPrimaryGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarMCHBPrimaryGenerator.h,v $
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

#ifndef StarMCHBPrimaryGenerator_h
#define StarMCHBPrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TDataSet.h"
#include "TFile.h"
#include "TTree.h"

class StarMCHBPrimaryGenerator : public StarMCPrimaryGenerator {
 public:
  StarMCHBPrimaryGenerator(const Char_t *rzFile, TDataSet *set=0);
  virtual ~StarMCHBPrimaryGenerator() {SafeDelete(fHBTree); SafeDelete(fHBFile);}
  
  static StarMCHBPrimaryGenerator* Instance() {return (StarMCHBPrimaryGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void  GeneratePrimaries();
  virtual void GeneratePrimaries(const TVector3& origin) {}
  void  SetHbtId(Int_t m = 999)               { fHbtId = m;}
  void  SetHBFileName(const Char_t *name)     { fHBFileName = name;}
  TString *HBFileName()                       { return &fHBFileName;}
  TTree *HBTree()                             { return fHBTree;}
  TFile *HBFile()                             { return fHBFile;}
 private:
  void GeneratePrimary(const TVector3& origin);
  TString           fHBFileName;
  TTree       *fHBTree;
  TFile       *fHBFile;
  Long64_t          fEntry;
  Long64_t          fnEntries;
  Int_t             fHbtId;
  TDataSet         *fData;
  ClassDef(StarMCHBPrimaryGenerator,1)  //StarMCHBPrimaryGenerator
};
#endif //StarMCHBPrimaryGenerator_h

