// $Id: StMCRootManager.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 novice ExampleN02 adapted to Virtual Monte Carlo 
//
// Class StMCRootManager
// ---------------------
// Class that takes care of Root IO.
//
// by Ivana Hrivnacova, 5.4.2002


#ifndef STMC_ROOT_MANAGER_H
#define STMC_ROOT_MANAGER_H

#include <TObject.h>
#include <TTree.h>
#include <TFile.h>

class TParticle;

enum FileMode { kRead, kWrite};

class StMCRootManager : public TObject
{
  public:
    StMCRootManager(const char* projectName, FileMode fileMode);
    StMCRootManager();
    virtual ~StMCRootManager();     
  
    // static access method
    static StMCRootManager* Instance(); 

    // methods
    void  Register(const char* name, void* clonesAddress);
    void  Register(const char* name, const char* className, void* objAddress);
    void  Fill();
    Int_t Write(const char* name,Int_t option,Int_t bufsize){return 0;}
    Int_t Write();
    void  ReadEvent(Int_t i);
    
  private:
    // data members
    static  StMCRootManager* fgInstance; //Singleton instance

    // data members
    TFile*  fFile;
    TTree*  fTree;
    
    ClassDef(StMCRootManager,0) // Root IO manager
};

#endif //STMC_ROOT_MANAGER_H   
   

