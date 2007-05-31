/*
 *  StSpinTreeReader.h
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
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
#include "TPi0Event.h"
//#include "StEEmcPool/StEEmcPi0Mixer/StEEmcPair.h"

class StSpinTreeReader {
public:
    StSpinTreeReader(const char *treeName = "spinTree");
    virtual ~StSpinTreeReader();
    StSpinTreeReader(const StSpinTreeReader &other);
    StSpinTreeReader& operator=(const StSpinTreeReader &rhs);
    
    void selectDataset(const char *path);    
    void selectFile(const char *path);
    void selectFile(std::string & path);
    
    long GetEntries();
    void GetEntry(long i);
    
    //all branches are turned on by default; use these flags to turn them off
    bool connectJets;
    bool connectNeutralJets;
    bool connectChargedPions;
    bool connectBemcPions;
    bool connectEemcPions;
    bool connectBemcElectrons;
    
    //setters
    void selectRunlist(const char *path);
    void selectRun(int runnumber);
    void removeRun(int runnumber);
    
    void selectTrigger(int trigger);
    bool requireDidFire;
    bool requireShouldFire;
    
    const TEventList* eventList() const {return mEventList;}
    void setEventList(TEventList *elist) {mEventList = elist;}
    
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
    
    int nBemcPions() {return mBemcPions->GetEntries();}
    TClonesArray* bemcPions() {return mBemcPions;}
    TPi0Candidate* bemcPion(int i) {return (TPi0Candidate*)mBemcPions->At(i);}
    
    //int nEemcPions() {return mEemcNeutralPions->GetEntries();}
    //TClonesArray* eemcPions() {return mEemcNeutralPions;}
    //StEEmcPair* eemcPion(int i) {return (StEEmcPair*)mEemcNeutralPions->At(i);}
    
    int nBemcElectrons() {return 0;}
    TClonesArray* bemcElectrons() {return NULL;}
    
private:
    StJetSkimEvent *mEvent;             //!
    TClonesArray *mConeJets;            //!
    TClonesArray *mConeJetsEMC;         //!
    TClonesArray *mChargedPions;        //!
    TClonesArray *mBemcPions;           //!
    TClonesArray *mEemcPions;           //!
        
    std::map<int,std::string> mFileList;//!
    std::set<int> mRunList;             //!
    std::set<int> mTriggerList;         //!
    
    TChain *mChain;                     //!
    TEventList *mEventList;             //!
    
    //these chains are friends of the parent chain
    TChain *mChainConeJets;             //!
    TChain *mChainConeJetsEMC;          //!
    TChain *mChainChargedPions;         //!
    TChain *mChainBemcPions;            //!
    TChain *mChainEemcPions;            //!
    
    void connect();
    bool mIsConnected;                  //!
    
    ClassDef(StSpinTreeReader,1)
};

#endif
