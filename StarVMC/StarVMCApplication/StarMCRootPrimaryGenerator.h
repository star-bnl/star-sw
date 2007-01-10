// $Id: StarMCRootPrimaryGenerator.h,v 1.2 2007/01/10 16:37:44 potekhin Exp $
// $Log: StarMCRootPrimaryGenerator.h,v $
// Revision 1.2  2007/01/10 16:37:44  potekhin
// Removed an extraneous header file reference
//
// Revision 1.1  2007/01/09 18:57:15  potekhin
// Header file for the ROOT-format event reader
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

#ifndef StarMCRootPrimaryGenerator_h
#define StarMCRootPrimaryGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TDataSet.h"
#include "TFile.h"
#include "TTree.h"

class StarMCRootPrimaryGenerator : public StarMCPrimaryGenerator {
 public:

  StarMCRootPrimaryGenerator(const Char_t *rootFile, TDataSet *set=0);
  virtual ~StarMCRootPrimaryGenerator() {SafeDelete(fRootTree); SafeDelete(fRootFile);}
  
  static StarMCRootPrimaryGenerator* Instance()           {return (StarMCRootPrimaryGenerator*) StarMCPrimaryGenerator::Instance();}

  virtual void GeneratePrimaries();

  virtual void GeneratePrimaries(const TVector3& origin)  {}
  void    SetHbtId(Int_t m = 999)                         { fHbtId = m;}
  void    SetRootFileName(const Char_t *name)             { fRootFileName = name;}
  TString *RootFileName()                                 { return &fRootFileName;}
  TTree   *RootTree()                                     { return fRootTree;}
  TFile   *RootFile()                                     { return fRootFile;}

 private:
  void GeneratePrimary(const TVector3& origin);
  TString            fRootFileName;
  TTree             *fRootTree;
  TFile             *fRootFile;
  Long64_t           fEntry;
  Long64_t           fnEntries;
  Int_t              fHbtId;
  TDataSet          *fData;

  //  StarVMCEventInput *eInput;

  ClassDef(StarMCRootPrimaryGenerator,1)  //StarMCRootPrimaryGenerator
};
#endif //StarMCRootPrimaryGenerator_h

