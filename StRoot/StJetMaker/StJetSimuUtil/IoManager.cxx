//IoManager.cxx

#include <iostream>
using namespace std;

#include "TFile.h"
#include "TTree.h"

#include "StJetMaker/StJetSimuUtil/IoManager.h"

//#include "StStripEvent.h"

ClassImp(IoManager)
    ClassImp(HasArray)
    ClassImp(JetPair)

    IoManager::IoManager()
{
    mAssocArray = new HasPairArray();
    mRecoArray = new HasArray();
    mPythiaArray = new HasArray();
}

IoManager::~IoManager()
{
}

void IoManager::write()
{
    mFile->cd();
    mAssocTree->Write();
    mPythiaTree->Write();
    mRecoTree->Write();
    
    mFile->Close();
}

void IoManager::getEvent(int i)
{
    cout <<"read RecoTree"<<endl;
    mRecoTree->GetEntry(i);

    cout <<"read pythiaTree"<<endl;
    mPythiaTree->GetEntry(i);

    cout <<"read assocTree"<<endl;
    mAssocTree->GetEntry(i);
}

void IoManager::openRead(const char* fname)
{
    mFile = new TFile(fname,"READ");

    cout <<"Recover TTree from file"<<endl;
    mAssocTree = dynamic_cast<TTree*>(mFile->Get("assocTree"));
    assert(mAssocTree);
    mAssocTree->SetBranchStatus("assocBranch", 1);
    mAssocTree->SetBranchAddress("assocBranch", &mAssocArray);
    
    cout <<"Recover TTree from file"<<endl;
    mRecoTree = dynamic_cast<TTree*>( mFile->Get("recoTree") );
    assert(mRecoTree);
    mRecoTree->SetBranchStatus("recoBranch", 1);
    mRecoTree->SetBranchAddress("recoBranch", &mRecoArray);

    cout <<"Recover TTree from file"<<endl;
    mPythiaTree = dynamic_cast<TTree*>(mFile->Get("pythiaTree"));;
    assert(mPythiaTree);
    mPythiaTree->SetBranchStatus("pythiaBranch", 1);
    mPythiaTree->SetBranchAddress("pythiaBranch", &mPythiaArray);
}

void IoManager::openWrite(const char* fname)
{
    mFile = new TFile(fname,"RECREATE");
    mFile->SetCompressionLevel(1);
    
    
    //create udst & its branches
    mAssocTree  = new TTree("assocTree","AssocTree",99);
    mAssocTree->Branch("assocBranch", "HasPairArray", &mAssocArray, 64000, 99);

    mRecoTree  = new TTree("recoTree","RecoTree",99);
    mRecoTree->Branch("recoBranch", "HasArray", &mRecoArray, 64000, 99);
    
    mPythiaTree  = new TTree("pythiaTree","PythiaTree",99);
    mPythiaTree->Branch("pythiaBranch", "HasArray", &mPythiaArray, 64000, 99);
    

}
