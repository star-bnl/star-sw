// $Id: StMCRootManager.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 novice ExampleN01 adapted to Virtual Monte Carlo 
//
// Class StMCRootManager
// ------------------
// Class that takes care of Root IO.
//
// by Ivana Hrivnacova, 5.4.2002

#include "StMCRootManager.h"

#include <TParticle.h>
#include <TObjArray.h>

ClassImp(StMCRootManager)

StMCRootManager* StMCRootManager::fgInstance = 0;

//_____________________________________________________________________________
StMCRootManager::StMCRootManager(const char* projectName, FileMode fileMode)
  : TObject()
{
//
  if (fgInstance) {
    Fatal("StMCRootManager", "Singleton instance already exists.");
    return;
  }  

  TString fileName(projectName);
  fileName += ".root";

  TString treeTitle(projectName);
  treeTitle += " tree";

  switch (fileMode) {
    case kRead:
      fFile = new TFile(fileName);
      fTree = (TTree*) fFile->Get(projectName);
      break;
      
    case kWrite:  
      fFile = new TFile(fileName, "recreate");
      fTree = new TTree(projectName, treeTitle);
      ;;  
  }

  fgInstance = this;
}

//_____________________________________________________________________________
StMCRootManager::StMCRootManager()
  : TObject(),
    fFile(0),
    fTree(0) 
{
//   

  if (fgInstance) {
    Fatal("StMCRootManager", "Singleton instance already exists.");
    return;
  }  

  fgInstance = this;
}

//_____________________________________________________________________________
StMCRootManager::~StMCRootManager() 
{
//
  delete fFile;
  fgInstance = 0;
}

//
// static methods
//

//_____________________________________________________________________________
StMCRootManager* StMCRootManager::Instance()
{
// Returns singleton instance.
// ---

  return fgInstance;
}  

//
// public methods
//


//_____________________________________________________________________________
void  StMCRootManager::Register(const char* name,  void* clonesAddress)
{
// Creates a branch of the given name and associates it with
// the given address.
// ---

  if (!fTree->GetBranch(name)) 
    fTree->Branch(name, clonesAddress, 32000, 99);
  else  
    fTree->GetBranch(name)->SetAddress(clonesAddress);
}

//_____________________________________________________________________________
void  StMCRootManager::Register(const char* name, const char* className, 
                                void* objAddress)
{
// Creates a branch of the given name and associates it with
// the given address.
// ---

  if (!fTree->GetBranch(name)) 
    fTree->Branch(name, className, objAddress, 32000, 99);
  else  
    fTree->GetBranch(name)->SetAddress(objAddress);
}

//_____________________________________________________________________________
void  StMCRootManager::Fill()
{
// Fills the tree.
// ---

  fTree->Fill();
}  

//_____________________________________________________________________________
Int_t StMCRootManager:: Write()
{
// Erites the tree in the file.
// ---

  fTree->Write();
  return 0;
}  

//_____________________________________________________________________________
void  StMCRootManager::ReadEvent(Int_t i)
{
// Reads the event data for i-th event for all connected branches.
// ---

  fTree->GetEntry(i);
}
