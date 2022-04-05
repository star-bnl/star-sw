/*
 *  StSpinTreeReader.h
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
 *  Last updated 7/4/07.
 *
 */

#if !defined(ST_SPIN_TREE_READER)
#define ST_SPIN_TREE_READER

#include <set>

#include "TChain.h"
#include "TEventList.h"

#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StChargedPionAnalysisMaker/StChargedPionTrack.h"
#include "TPi0Event.h"
#include "StPrimaryElectron.h"
#include "StGlobalElectron.h"
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
    bool connectBemcElectrons;
    bool connectEemcPions;
    
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
    const TChain* chain() const {return mChain;}
    
    const StJetSkimEvent* event() const {return mEvent;}
    
    int nJets() {return mConeJets->GetEntries();}
    const TClonesArray* jets() const {return mConeJets;}
    const StJet* jet(int i) const {return (StJet*)mConeJets->At(i);}
    
    int nNeutralJets() {return mConeJetsEMC->GetEntries();}
    const TClonesArray* neutralJets() const {return mConeJetsEMC;}
    const StJet* neutralJet(int i) const {return (StJet*)mConeJetsEMC->At(i);}
    
    int nChargedPions() {return mChargedPions->GetEntries();}
    const TClonesArray* chargedPions() const {return mChargedPions;}
    const StChargedPionTrack* chargedPion(int i) const {return (StChargedPionTrack*)mChargedPions->At(i);}
    
    int nBemcPions() {return mBemcPions->GetEntries();}
    const TClonesArray* bemcPions() const {return mBemcPions;}
    const TPi0Candidate* bemcPion(int i) const {return (TPi0Candidate*)mBemcPions->At(i);}
    
    int nBemcElectrons() {return mBemcElectrons->GetEntries();}
    const TClonesArray* bemcElectrons() const {return mBemcElectrons;}
    const StPrimaryElectron* bemcElectron(int i) const {return (StPrimaryElectron*)mBemcElectrons->At(i);}
    
    int nBemcGlobalElectrons() {return mBemcGlobalElectrons->GetEntries();}
    const TClonesArray* bemcGlobalElectrons() const {return mBemcGlobalElectrons;}
    const StGlobalElectron* bemcGlobalElectron(int i) const {return (StGlobalElectron*)mBemcGlobalElectrons->At(i);}
    
    //int nEemcPions() {return mEemcNeutralPions->GetEntries();}
    //TClonesArray* eemcPions() {return mEemcNeutralPions;}
    //StEEmcPair* eemcPion(int i) {return (StEEmcPair*)mEemcNeutralPions->At(i);}
    
private:
    StJetSkimEvent *mEvent;             //!
    TClonesArray *mConeJets;            //!
    TClonesArray *mConeJetsEMC;         //!
    TClonesArray *mChargedPions;        //!
    TClonesArray *mBemcPions;           //!
    TClonesArray *mBemcElectrons;       //!
    TClonesArray *mBemcGlobalElectrons; //!
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
    TChain *mChainBemcElectrons;        //!
    TChain *mChainEemcPions;            //!
    
    std::string mCurrentFileName;       //!
    
    void connect();
    bool mIsConnected;                  //!
    
    ClassDef(StSpinTreeReader,1)
};

#endif
