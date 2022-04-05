/*
 *  StSpinTreeReader.cpp
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
 *
 */

#include "StSpinTreeReader.h"

#include "TDirectory.h"
#include "TFile.h"
#include "TStopwatch.h"

ClassImp(StSpinTreeReader)

#include <fstream>

StSpinTreeReader::StSpinTreeReader(const char *treeName) : connectJets(true), 
    connectNeutralJets(true), connectChargedPions(true), connectBemcPions(true),
    connectEemcPions(true), connectBemcElectrons(true), requireDidFire(false), requireShouldFire(false),
    mEemcPions(NULL), mEventList(NULL), mIsConnected(false)
{
    mChain              = new TChain(treeName);
    mChainConeJets      = new TChain("ConeJets");
    mChainConeJetsEMC   = new TChain("ConeJetsEMC");
    mChainChargedPions  = new TChain("chargedPions");
    mChainBemcPions     = new TChain("bemcPions");
    mChainBemcElectrons = new TChain("bemcElectrons");
    
    mEvent = new StJetSkimEvent();
    mConeJets = new TClonesArray("StJet",100);
    mConeJetsEMC = new TClonesArray("StJet",100);
    mChargedPions = new TClonesArray("StChargedPionTrack",100);
    mBemcPions = new TClonesArray("TPi0Candidate",100);
    mBemcElectrons = new TClonesArray("StPrimaryElectron",100);
    mBemcGlobalElectrons = new TClonesArray("StGlobalElectron",500);
}

StSpinTreeReader::~StSpinTreeReader() { 
    std::cout << "StSpinTreeReader::~StSpinTreeReader()" << std::endl;
    delete mChainConeJets;
    delete mChainConeJetsEMC;
    delete mChainChargedPions;
    delete mChainBemcPions;
    delete mChainBemcElectrons;
    delete mChain;
    
    delete mConeJets;
    delete mConeJetsEMC;
    delete mChargedPions;
    delete mBemcPions;
    delete mBemcElectrons;
    delete mBemcGlobalElectrons;
    
    if(mEventList){
        delete mEventList;
        mEventList = NULL;
    }
}

StSpinTreeReader::StSpinTreeReader(const StSpinTreeReader & t) {
    
}

StSpinTreeReader& StSpinTreeReader::operator=(const StSpinTreeReader &rhs) {
    return *this;
}

void StSpinTreeReader::selectDataset(const char *path) {
    TString fullPath = path;
    fullPath.ReplaceAll("$STAR",getenv("STAR"));
    std::ifstream filelist(fullPath.Data());
    std::string currentFile;
    while(filelist.good()) {
        getline(filelist,currentFile);
        if(currentFile.size() == 0) continue;
        //std::cout << "adding current file = " << currentFile << std::endl;
        selectFile(currentFile);
    }
}

void StSpinTreeReader::selectFile(const char *path) {
    std::string theFile(path);
    selectFile(theFile);
}

void StSpinTreeReader::selectFile(std::string & path) {
    int run = atoi(path.substr(path.length()-17,7).c_str());
    mFileList[run] = path;
}

long StSpinTreeReader::GetEntries() {
    connect();
    if(mEventList) return mEventList->GetN();
    return mChain->GetEntries();
}

void StSpinTreeReader::GetEntry(long i) {
    connect();
    if(mEventList) {
        long n = mEventList->GetEntry(i);
        mChain->GetEntry(n);
    }
    else mChain->GetEntry(i);
    
    if(mCurrentFileName != mChain->GetFile()->GetName()) { 
        mCurrentFileName = mChain->GetFile()->GetName();
        std::cout << "now analyzing " << mCurrentFileName << std::endl;
    }
}

void StSpinTreeReader::connect() {
    if(!mIsConnected) {
        //only use files in the filelist if the run is selected or runlist.empty()
        if(mFileList.empty()) std::cout << "no files to analyze!  check your macro" << std::endl;
        for(map<int,std::string>::iterator it=mFileList.begin(); it!=mFileList.end(); it++) {
            if(mRunList.empty() || mRunList.count(it->first)) { 
                std::cout << "adding " << it->second << std::endl;
                mChain->AddFile(it->second.c_str());
                if(connectJets)         mChainConeJets->AddFile(it->second.c_str());
                if(connectNeutralJets)  mChainConeJetsEMC->AddFile(it->second.c_str());
                if(connectChargedPions) mChainChargedPions->AddFile(it->second.c_str());
                if(connectBemcPions)    mChainBemcPions->AddFile(it->second.c_str());
                if(connectBemcElectrons)mChainBemcElectrons->AddFile(it->second.c_str());
            }
        }
                
        mChain->SetBranchAddress("skimEventBranch",&mEvent);
        
        if(connectJets) { 
            mChain->AddFriend("ConeJets");
            mChain->SetBranchAddress("ConeJets", &mConeJets);
        }
        
        if(connectNeutralJets) {
            mChain->AddFriend("ConeJetsEMC");
            mChain->SetBranchAddress("ConeJetsEMC",&mConeJetsEMC);
        }
        
        if(connectChargedPions) {
            mChain->AddFriend("chargedPions");
            mChain->SetBranchAddress("chargedPions",&mChargedPions);
        }
        
        if(connectBemcPions) {
            mChain->AddFriend("bemcPions");
            mChain->SetBranchAddress("bemcPions",&mBemcPions);
        }
        if(connectBemcElectrons) {
            mChain->AddFriend("bemcElectrons");
            mChain->SetBranchAddress("PrimaryElectrons",&mBemcElectrons);
            mChain->SetBranchAddress("GlobalElectrons",&mBemcGlobalElectrons);
        }
        if(connectEemcPions) {
            //mEemcPions = new TClonesArray("StEEmcPair",100);
            //mChain->SetBranchAddress("eemcNeutralPions",&mEemcPions);
        }
        
        //now do the event list selection by building a TString from the sets of triggers
        if(mEventList == NULL) {
            TString s = "";
            bool atStart = true;
            for(std::set<int>::const_iterator it = mTriggerList.begin(); it != mTriggerList.end(); it++) {
                if(atStart) {
                    s += "( mTriggers.mTrigId==";
                    atStart = false;
                }
                else { s += " || mTriggers.mTrigId=="; }
                s += *it;
            }
            if(mTriggerList.size()) s += " )";
            if(requireDidFire || requireShouldFire) {
                s += " && ( ";
                if(requireDidFire && requireShouldFire) s += "mTriggers.mDidFire==1 && mTriggers.mShouldFire==1 )";
                else if(requireDidFire) s += "mTriggers.mDidFire==1 )";
                else s+= "mTriggers.mShouldFire==1 )";
            }
            if(s.Length()) {
                std::cout << "begin generation of TEventList with contents \n" << s << std::endl;
                TStopwatch timer;
                mChain->Draw(">>elist_spinTreeReader",s.Data(),"entrylist");
                mEventList = (TEventList*)gDirectory->Get("elist_spinTreeReader");
                mChain->SetEventList(mEventList);
                std::cout << "TEventList generated and stored in " << timer.CpuTime() 
                    << " CPU / " << timer.RealTime() << " real seconds" << std::endl;
            }
        }
        
        mIsConnected = true;
    }
}

void StSpinTreeReader::selectRunlist(const char *path) {
    TString fullPath = path;
    fullPath.ReplaceAll("$STAR",getenv("STAR"));
    std::ifstream list(fullPath.Data());
    int currentRun;
    while(!list.eof()) {
        list >> currentRun;
        if(currentRun == 0) continue;
        //std::cout << "adding current run = " << currentRun << std::endl;
        mRunList.insert(currentRun);
    }
}

void StSpinTreeReader::selectRun(int runnumber) {
    mRunList.insert(runnumber);
}

void StSpinTreeReader::removeRun(int runnumber) {
    mRunList.erase(runnumber);
}

void StSpinTreeReader::selectTrigger(int trigger) {
    mTriggerList.insert(trigger);
}
