
//StppuDstReader.cxx
//M.L. Miller (Yale Software)
//10/02

//std
#include "Stiostream.h"

//root
#include "TObjString.h"
#include "TChain.h"
#include "TString.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//StSpin
#include "StppEvent.h"

#include "StppuDstReader.h"

ClassImp(StppuDstReader)

StppuDstReader::StppuDstReader(TTree* t) : mEvent(0), mTree(0)
{
    cout <<"StppuDstReader::StppuDstReader(TTree* t)"<<endl;
    TChain* dum = dynamic_cast<TChain*>(t);
    if (dum) { //it's a TChain
	cout <<"\tRead from TChain"<<endl;
	t = dum;
    }
    else {
	cout <<"\tRead from TTree"<<endl;
    }

    if (!t || t->GetEntries()==0) {
	cout <<"StppuDstReader::StppuDstReader(TTree* t). ERROR:\t"
	     <<"t==0.  abort()"<<endl;
	abort();
    }

    cout <<"\tcreate StppEvent"<<endl;
    mEvent = new StppEvent();
    mEvent->setInfoLevel(0);

    cout <<"\tGet Branches"<<endl;
    TObjArray* branches = t->GetListOfBranches();
    if (!branches) {cout <<"StppuDstReader::StppuDstReader(TTree*).  Null branches"<<endl; abort();}

    cout <<"\tLoop on branches"<<endl;
    
    for (int i=0; i<branches->GetLast()+1; ++i) {
	TBranch* branch = dynamic_cast<TBranch*>((*branches)[i]);
	if (!branch) {cout <<"StppuDstReader::StppuDstReader(TTree*).  Null branch"<<endl; abort();}
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
	else if (bname.find("Event")!=bname.npos || bname.find("event")!=bname.npos ){
	    cout <<"\t\tSet Event Branch Address"<<endl;
	    t->SetBranchStatus(bname.c_str(), 1);
	    t->SetBranchAddress("Event",&mEvent);
	}
	//would have to put a hook here to read the BBC branch
	else {
	    cout <<"\tDeactivating branch:\t"<<bname.c_str()<<endl;
	    t->SetBranchStatus(bname.c_str(), 0);
	}
    }

    cout <<"\tset tree pointer"<<endl;
    mTree = t;
    
    cout <<"\tSet StJetsMap in StppEvent"<<endl;
    mEvent->setStJetsMap(&mStJetsMap);
    
    cout <<"\tfinished!"<<endl;
}

int StppuDstReader::numberOfEvents() const
{
    if (!mTree) {
	cout <<"StppuDstReader::numberOfEvents(). ERROR:\t"
	     <<"mTree==0. no action"<<endl;
	return 0;
    }
    return static_cast<int>( mTree->GetEntries() );
}

void gStatus(int status, const string name)
{
    if (status==0) {
	cout <<"gStatus("<<name<<"). ERROR:\t event does not exist"<<endl;
    }
    else if (status==-1) {
	cout <<"gStatus("<<name<<"). ERROR:\tI/O Error, status=-1"<<endl;
    }
    else {
	cout <<"gStatus("<<name<<"). read:\t"<<status<<"\tbytes"<<endl;
    }
}

StppEvent* StppuDstReader::getEvent(int i)
{
    mEvent->clear();
    for (StJetsMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
	(*it).second->Clear();
    }
    
    if (i<0 || i>numberOfEvents()) {
	cout <<"StppuDstReader::getEvent(int i). ERROR:\t"
	     <<"index out of bounds. return null"<<endl;
	return 0;
    }
    //cout <<"GetEntry("<<i<<")"<<endl;
    int status = mTree->GetEntry(i); if(status){/*nothing*/};
    //gStatus(status,"Entire Tree");
    
    /*
      int status = mEventBranch->GetEntry(i);
      gStatus(status, "EventBranch");
      
      status = mBranch1->GetEntry(i);
      gStatus(status, "MkConeJets");
      
      status = mBranch2->GetEntry(i);
      gStatus(status, "MkKtJets");
    */
    
    return mEvent;
}

TObjArray StppuDstReader::branchNames()
{
    TObjArray vec;
    for (StJetsMap::iterator it=mStJetsMap.begin(); it!=mStJetsMap.end(); ++it) {
	string bname = (*it).first;
	TObjString* temp = new TObjString(bname.c_str());
	vec.Add(temp);
    }
    return vec;
}
