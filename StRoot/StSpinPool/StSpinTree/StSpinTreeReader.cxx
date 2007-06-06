/*
 *  StSpinTreeReader.cpp
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
 *
 */

#include "StSpinTreeReader.h"

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
    
    mEvent = new StJetSkimEvent();
    mConeJets = new TClonesArray("StJet",100);
    mConeJetsEMC = new TClonesArray("StJet",100);
    mChargedPions = new TClonesArray("StChargedPionTrack",100);
    mBemcPions = new TClonesArray("TPi0Candidate",100);
}

StSpinTreeReader::~StSpinTreeReader() { 
    std::cout << "StSpinTreeReader::~StSpinTreeReader()" << std::endl;
    delete mChainConeJets;
    delete mChainConeJetsEMC;
    delete mChainChargedPions;
    delete mChainBemcPions;
    delete mChain;
    
    delete mConeJets;
    delete mConeJetsEMC;
    delete mChargedPions;
    delete mBemcPions;
}

StSpinTreeReader::StSpinTreeReader(const StSpinTreeReader & t) {
    
}

StSpinTreeReader& StSpinTreeReader::operator=(const StSpinTreeReader &rhs) {
    return *this;
}

/*int StSpinTreeReader::addFile(const char *path) {
    return mChain->AddFile(path);
}

int StSpinTreeReader::addFileList(const char *path) {
    int ret = 0;
    std::ifstream filelist(path);
    std::string currentFile;
    while(!filelist.eof()) {
        getline(filelist,currentFile);
        if(currentFile.size() == 0) continue;
        std::cout << "adding current file = " << currentFile << std::endl;
        if(ret < addFile(currentFile.c_str())) ret++;
    }
    return ret;
}*/

void StSpinTreeReader::selectDataset(const char *path) {
    std::ifstream filelist(path);
    std::string currentFile;
    while(!filelist.eof()) {
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
}

void StSpinTreeReader::connect() {
    if(!mIsConnected) {
        //only use files in the filelist if the run is selected or runlist.empty()
        for(map<int,std::string>::iterator it=mFileList.begin(); it!=mFileList.end(); it++) {
            if(mRunList.empty() || mRunList.count(it->first)) { 
                cout << "adding " << it->second << endl;
                mChain->AddFile(it->second.c_str());
                if(connectJets)         mChainConeJets->AddFile(it->second.c_str());
                if(connectNeutralJets)  mChainConeJetsEMC->AddFile(it->second.c_str());
                if(connectChargedPions) mChainChargedPions->AddFile(it->second.c_str());
                if(connectBemcPions)    mChainBemcPions->AddFile(it->second.c_str());
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
                std::cout << "Processing the chain using a TEventList that looks like \n" << s << std::endl;
                mChain->Draw(">>elist",s.Data(),"entrylist");
                mEventList = (TEventList*)gDirectory->Get("elist");
                mChain->SetEventList(mEventList);
                std::cout << "Eventlist stored" << std::endl;
            }
        }
        
        mIsConnected = true;
    }
}

void StSpinTreeReader::selectRunlist(const char *path) {
    std::ifstream list(path);
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
