/*
 *  StSpinTreeReader.cpp
 *  StarSpinLibraries
 *
 *  Created by Adam Kocoloski on 5/7/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "StSpinTreeReader.h"

ClassImp(StSpinTreeReader)

#include <fstream>

StSpinTreeReader::StSpinTreeReader(const char *treeName) : connectJets(true), 
    connectNeutralJets(true), connectChargedPions(true), connectBemcNeutralPions(true),
    connectEemcNeutralPions(true), connectBemcElectrons(true), mIsConnected(false), mEvent(NULL), 
    mConeJets(NULL), mConeJetsEMC(NULL), mChargedPions(NULL), mBemcNeutralPions(NULL), 
    mEemcNeutralPions(NULL), mEventList(NULL)
{
    mChain = new TChain(treeName);
}

StSpinTreeReader::~StSpinTreeReader() { 
    std::cout << "StSpinTreeReader::~StSpinTreeReader()" << std::endl;
    delete mChain;
}

StSpinTreeReader::StSpinTreeReader(const StSpinTreeReader & t) {
    
}

StSpinTreeReader& StSpinTreeReader::operator=(const StSpinTreeReader &rhs) {
    return *this;
}

int StSpinTreeReader::addFile(const char *path) {
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
}

long StSpinTreeReader::GetEntries() {
    return mChain->GetEntries();
}

void StSpinTreeReader::GetEntry(long i) {
    connect();
    long n = mEventList->GetEntry(i);
    std::cout<<"next entry number in chain is " << n << endl;
    mChain->GetEntry(n);
}

void StSpinTreeReader::connect() {
    if(!mIsConnected) {
        mChain->SetBranchAddress("skimEventBranch",&mEvent);
        
        if(connectJets) { 
            mConeJets = new TClonesArray("StJet",100);
            if(mChain->GetBranch("ConeJets12") != NULL) { //Run 6
                mChain->SetBranchAddress("ConeJets12",&mConeJets);
            }
            else { //Run 5
                mChain->SetBranchAddress("ConeJets", &mConeJets);
            }
        }
        
        if(connectNeutralJets) {
            mConeJetsEMC = new TClonesArray("StJet",100);
            mChain->SetBranchAddress("ConeJetsEMC",&mConeJetsEMC);
        }
        
        if(connectChargedPions) {
            mChargedPions = new TClonesArray("StChargedPionTrack",100);
            mChain->SetBranchAddress("chargedPions",&mChargedPions);
        }
        
        if(connectBemcNeutralPions) {
            mBemcNeutralPions = new TClonesArray("TPi0Candidate",100);
            mChain->SetBranchAddress("bemcNeutralPions",&mBemcNeutralPions);
        }
        
        if(connectEemcNeutralPions) {
            mEemcNeutralPions = new TClonesArray("StEEmcPair",100);
            mChain->SetBranchAddress("eemcNeutralPions",&mEemcNeutralPions);
        }
        
        //now do the event list selection by building a TString from the sets of runs/triggers
        TString s = "( ";
        bool atStart = true;
        std::cout << s << std::endl;
        for(std::set<int>::const_iterator it = mRunList.begin(); it != mRunList.end(); it++) {
            if(!atStart) s += " || ";
            s += "mRunId==";
            s += *it;
            atStart = false;
        }
        s += " )";
        atStart = true;
        for(std::set<int>::const_iterator it = mTriggerList.begin(); it != mTriggerList.end(); it++) {
            if(atStart) {
                s += " && ( mTriggers.mTrigId==";
                atStart = false;
            }
            else { s += " || mTriggers.mTrigId=="; }
            s += *it;
        }
        if(mTriggerList.size()) s += " )";
        
        if(s.Length()) {
            std::cout << "Processing the chain using a TEntryList that looks like \n" << s << std::endl;
            mChain->Draw(">>elist",s.Data(),"entrylist");
            mEventList = (TEventList*)gDirectory->Get("elist");
            mChain->SetEventList(mEventList);
            //mEventList->Print("all");
        }
        
        mIsConnected = true;
    }
}

void StSpinTreeReader::selectRunList(const char *path) {
    std::ifstream list(path);
    int currentRun;
    while(!list.eof()) {
        list >> currentRun;
        if(currentRun == 0) continue;
        std::cout << "adding current run = " << currentRun << std::endl;
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
