// $Id: StarRootManager.cxx,v 1.1 2004/07/12 20:36:39 potekhin Exp $
//

#include <iostream.h>
#include "StarRootManager.h"

#include <TParticle.h>
#include <TObjArray.h>

ClassImp(StarRootManager)

StarRootManager* StarRootManager::fgInstance = 0;

//_____________________________________________________________________________
StarRootManager::StarRootManager(const char* projectName, FileMode fileMode)
  : TObject()
{
//
  cout<<"StarRootManager ctor called"<<endl;
  if (fgInstance) {
    Fatal("StarRootManager", "Singleton instance already exists.");
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
  cout<<"StarRootManager ctor done"<<endl;
}

//_____________________________________________________________________________
StarRootManager::StarRootManager()
  : TObject(),
    fFile(0),
    fTree(0) 
{
//   

  if (fgInstance) {
    Fatal("StarRootManager", "Singleton instance already exists.");
    return;
  }  

  fgInstance = this;
}

//_____________________________________________________________________________
StarRootManager::~StarRootManager() 
{
//
  delete fFile;
  fgInstance = 0;
}

//
// static methods
//

//_____________________________________________________________________________
StarRootManager* StarRootManager::Instance()
{
// Returns singleton instance.
// ---

  return fgInstance;
}  

//
// public methods
//


//_____________________________________________________________________________
void  StarRootManager::Register(const char* name,  void* clonesAddress)
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
void  StarRootManager::Register(const char* name, const char* className, 
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
void  StarRootManager::Fill()
{
// Fills the tree.
// ---

  fTree->Fill();
}  

//_____________________________________________________________________________
void StarRootManager:: Write()
{
// Erites the tree in the file.
// ---

  fTree->Write();
}  

//_____________________________________________________________________________
void  StarRootManager::ReadEvent(Int_t i)
{
// Reads the event data for i-th event for all connected branches.
// ---

  fTree->GetEntry(i);
}
