//M.L. Miller
//MIT Software
//6/04

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//StJetMaker
#include "StJetMaker/StJet.h"
#include "StJetMaker/StJets.h"
#include "StJetMaker/StJetReader.h"

ClassImp(StJetReader)

double gDeltaPhi(double p1, double p2)
{
    float dp = p1 - p2;
    while(dp >  M_PI) {dp -= 2.0 * M_PI;}
    while(dp < -1.*M_PI) {dp += 2.0 * M_PI;}
    return dp;
};

double gDeltaR(const TLorentzVector* jet, const StThreeVectorF& track)
{
    double dEta = jet->Eta() - track.pseudoRapidity();
    double dPhi = gDeltaPhi(jet->Phi(), track.phi() );
    return sqrt(dEta*dEta + dPhi*dPhi);
}

StJetReader::StJetReader(const char* name, StMuDstMaker* uDstMaker)
    : StMaker(name), mFile(0), mTree(0), mDstMaker(uDstMaker), mCounter(0)
{
    cout <<"StJetReader::StJetReader()"<<endl;
}

StJetReader::~StJetReader()
{
    cout <<"StJetReader::~StJetReader()"<<endl;
}

Int_t StJetReader::Init()
{
    return kStOk;
}

void StJetReader::InitFile(const char* file)
{
    cout <<"StJetReader::InitFile()"<<endl;

    cout <<"open file:\t"<<file<<"\tfor reading"<<endl;
    mFile = new TFile(file,"READ");
    assert(mFile);

    cout <<"recover tree"<<endl;
    TObject* tree = mFile->Get("jet");
    TTree* t = dynamic_cast<TTree*>(tree);
    assert(t);
    mTree = t;

    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in tree:\t"<<t->GetEntries();
    
    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StJetReader::InitFile().  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;
    
    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StJetReader::InitFile().  Null branch"<<endl; abort();}
	string bname( branch->GetName() );
	cout <<"\t--- Found branch:\t"<<bname<<endl;
	
	if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
	    cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    cout <<"\tfinished!"<<endl;

    return ;
}

void StJetReader::InitTree(TTree* tree)
{
    cout <<"StJetReader::InitTree()"<<endl;

    cout <<"\tset tree pointer"<<endl;
    cout <<"Number of entries in tree:\t"<<tree->GetEntries();

    TList* friendList = tree->GetListOfFriends();
    TTree* t=0;
    for (int j=0; j<friendList->GetSize()+1; ++j) {
	TFriendElement* fr = static_cast<TFriendElement*>( friendList->At(j) );
	string tree_name( fr->GetTreeName() );
	if (tree_name == "jet") {
	    t = fr->GetTree();
	    break;
	}
    }
    assert(t);
    
    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StJetReader::InitFile().  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;

    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StJetReader::InitFile().  Null branch"<<endl; abort();}
	string bname( branch->GetName() );
	cout <<"\t--- Found branch:\t"<<bname<<endl;
	
	if ( (bname.find("jet")!=bname.npos) || (bname.find("Jet")!=bname.npos) ) {
	    cout <<"\t\tcreate StJets object for branch:\t"<<bname<<endl;

	    //create StJets object here, put in map:
	    StJets* jets = new StJets();
	    jets->Clear();
	    mStJetsMap[bname] = jets;
	    cout <<"\t\tset branch address for branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress(bname.c_str(), &jets);
	}
    }

    cout <<"\tfinished!"<<endl;

    return ;
}

Int_t StJetReader::Make()
{
    if (mTree) { //handle the reading ourselves...
	int status = mTree->GetEntry(mCounter++);
	if (status<0) {
	    cout <<"StJetReader::getEvent(). ERROR:\tstatus<0.  return null"<<endl;
	}
    }
    else {
	//the MuDst did the reading...
    }
    if (mDstMaker) {//double check consistency:
	StMuDst* mudst = mDstMaker->muDst();
	for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
	    StJets* j = (*it).second;
	    
	    if ( !j->isSameEvent(mudst) ) {
		cout <<"StJetReader::Maker() ERROR:\tisSameEvent()==false.  abort"<<endl;
		abort();
	    }
	}
    }
    return kStOk;
}

Int_t StJetReader::Finish()
{
    return kStOk;
}

void StJetReader::exampleEventAna()
{
    cout <<"StJetReader::exampleEventAna()"<<endl;

    StMuDst* muDst = 0;
    if (mDstMaker!=0) {
	muDst = mDstMaker->muDst();
	cout <<"nPrimary:\t"<<muDst->primaryTracks()->GetLast()+1;
    }
    
    for (JetBranchesMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {

	StJets* stjets = (*it).second;
	int nJets = stjets->nJets();
	cout <<"Found\t"<<nJets<<"\tjets from:\t"<<(*it).first<<endl;
	
	TClonesArray* jets = stjets->jets();
	for(int i=0; i<nJets; ++i){ 
	    
	    //loop on jets
	    StJet* j = static_cast<StJet*>( (*jets)[i] );
	    cout <<"Ejet:\t"<<j->E()<<"\tPhi:\t"<<j->Phi()<<"\tEta:\t"<<j->Eta()<<endl;
	    
	    if (muDst) {
		typedef StJets::TrackVec TrackVec;
		TrackVec tracks = stjets->jetParticles(muDst, i);
		
		int itrack = 0;
		for (TrackVec::iterator tit = tracks.begin(); tit!=tracks.end(); ++tit) {
		    StMuTrack *muTrack = *tit;
		    
		    //cout <<"\t--track "<<itrack<<endl;
		    const StThreeVectorF& mom = muTrack->momentum();
		    double dR = gDeltaR(j, mom);
		    cout <<"\tPt_track:\t"<<mom.perp()
			 <<"\tEta_track:\t"<<mom.pseudoRapidity()
			 <<"\tPhi_track:\t"<<mom.phi()
			 <<"\tdR:\t"<<dR<<endl;
		    
		    ++itrack;
		}
	    }
	}
    }
}
