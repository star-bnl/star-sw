//StJetSimuReader.h
//M.L. Miller
//MIT Software
//6/04

#ifndef STJETSIMUREADER_H
#define STJETSIMUREADER_H

#include "StMaker.h"
#include "StJetMaker/StJetMaker.h"

class TTree;
class TFile;
class StMuDstMaker;
class StJetSkimEvent;

class StJetSimuReader : public StMaker{
  
  
 public:
  
  ///A useful typedef for the StJets map
  typedef map<string, StJets*, less<string> > JetBranchesMap;
  
  ///The constructor requires a valid instance of StMuDstMaker
  StJetSimuReader(const char* name, StMuDstMaker* uDstMaker);
  virtual ~StJetSimuReader();
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  ///Recover the TTree from file and prepare for reading
  virtual void InitFile(const char* file);
  
  ///Recover the "fast" tree of StJetSkimEvent
  virtual void InitJetSkimFile(const char* file);
  
  ///Prepare for reading the TTree, but as a "friend" of the StMuDst tree
  ///BUG: this doesn't work with MuDst since 2005 and on...
  virtual void InitTree(TTree* tree);
  
  ///Check if we are all ready to read the Skim and Jet trees together
  int preparedForDualRead();
  
  ///Access to the StJetsMap
  JetBranchesMap& jetsMap();
  
  ///An example analysis method, look here for a demonstration of jet + event info
  void exampleSimuAna();
  
  ///Access to the StJets tree
  TTree* tree() {return mTree;}
  
  ///Access to the StJetSkimEvent tree
  TTree* skimTree() {return mSkimTree;}
  
  ///Acces to the StJetSkimEvent
  StJetSkimEvent* skimEvent() {return mSkimEvent;}

 private:
  JetBranchesMap mStJetsMap;
  TFile *mFile;
  TTree *mTree;
  TFile* mSkimFile;
  TTree* mSkimTree;
  StMuDstMaker *mDstMaker;
  int mCounter;
  bool mValid;
  StJetSkimEvent* mSkimEvent;
  ofstream* mOfstream; 

  
  ClassDef(StJetSimuReader,1)
    };    
    
    //inlines ---
    
    inline StJetSimuReader::JetBranchesMap& StJetSimuReader::jetsMap(){
      return mStJetsMap;
    }

#endif // STJETSIMUREADER_H
