// $Id: StarRootManager.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $
//

#ifndef STARROOTMANAGER_H
#define STARROOTMANAGER_H

#include <TObject.h>
#include <TTree.h>
#include <TFile.h>

class TParticle;

enum FileMode { kRead, kWrite};

class StarRootManager : public TObject
{
  public:
    StarRootManager(const char* projectName, FileMode fileMode);
    StarRootManager();
    virtual ~StarRootManager();     
  
    // static access method
    static StarRootManager* Instance(); 

    // methods
    void  Register(const char* name, void* clonesAddress);
    void  Register(const char* name, const char* className, void* objAddress);
    void  Fill();
    void  Write();
    void  ReadEvent(Int_t i);
    
  private:
    // data members
    static  StarRootManager* fgInstance; //Singleton instance

    // data members
    TFile*  fFile;
    TTree*  fTree;
    
    ClassDef(StarRootManager,0) // Root IO manager
};

#endif //STARROOTMANAGER_H   
   

