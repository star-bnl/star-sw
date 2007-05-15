/*
 *  StSpinTreeReader.h
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#if !defined(ST_SPIN_TREE_READER)
#define ST_SPIN_TREE_READER

#include <set>

#include "TChain.h"
#include "TEventList.h"

#include "StJetMaker/StJetSkimEvent.h"
#include "StJetMaker/StJet.h"
#include "StSpinPool/StChargedPionAnalysisMaker/StChargedPionTrack.h"
//#include "TPi0Event.h"
#include "StEEmcPool/StEEmcPi0Mixer/StEEmcPair.h"

class StSpinTreeReader {
public:
    StSpinTreeReader(const char *treeName = "spinTree");
    virtual ~StSpinTreeReader();
    StSpinTreeReader(const StSpinTreeReader &other);
    StSpinTreeReader& operator=(const StSpinTreeReader &rhs);
    
    long GetEntries();
    void GetEntry(long i);
    
    //all branches are turned on by default; use these flags to turn them off
    bool connectJets;
    bool connectNeutralJets;
    bool connectChargedPions;
    bool connectBemcNeutralPions;
    bool connectEemcNeutralPions;
    bool connectBemcElectrons;
    
    //setters
    void selectRunList(const char *path);
    void selectRun(int runnumber);
    void removeRun(int runnumber);
    
    void selectTrigger(int trigger);
    
    int addFile(const char *path);
    int addFileList(const char *path);
    
    //accessors
    StJetSkimEvent* event() {return mEvent;}
    
    int nJets() {return mConeJets->GetEntries();}
    TClonesArray* jets() {return mConeJets;}
    StJet* jet(int i) {return (StJet*)mConeJets->At(i);}
    
    int nNeutralJets() {return mConeJetsEMC->GetEntries();}
    TClonesArray* neutralJets() {return mConeJetsEMC;}
    StJet* neutralJet(int i) {return (StJet*)mConeJetsEMC->At(i);}
    
    int nChargedPions() {return mChargedPions->GetEntries();}
    TClonesArray* chargedPions() {return mChargedPions;}
    StChargedPionTrack* chargedPion(int i) {return (StChargedPionTrack*)mChargedPions->At(i);}
    
    //int nBemcNeutralPions() {return mBemcNeutralPions->GetEntries();}
    //TClonesArray* bemcNeutralPions() {return mBemcNeutralPions;}
    //TPi0Candidate* bemcNeutralPion(int i) {return (TPi0Candidate*)mBemcNeutralPions->At(i);}
    
    int nEemcNeutralPions() {return mEemcNeutralPions->GetEntries();}
    TClonesArray* eemcNeutralPions() {return mEemcNeutralPions;}
    StEEmcPair* eemcNeutralPion(int i) {return (StEEmcPair*)mEemcNeutralPions->At(i);}
    
    int nBemcElectrons() {return 0;}
    TClonesArray* bemcElectrons() {return NULL;}
    
private:
    TChain *mChain;                     //!
    StJetSkimEvent *mEvent;             //!
    TClonesArray *mConeJets;            //!
    TClonesArray *mConeJetsEMC;         //!
    TClonesArray *mChargedPions;        //!
    TClonesArray *mBemcNeutralPions;    //!
    TClonesArray *mEemcNeutralPions;    //!
        
    std::set<int> mRunList;             //!
    std::set<int> mTriggerList;         //!
    TEventList *mEventList;             //!
    
    void connect();
    bool mIsConnected;                  //!
    
    ClassDef(StSpinTreeReader,1)
};

#endif
