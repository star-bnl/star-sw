
//std
#include "Stiostream.h"
#include <cmath>
#include <vector>
using namespace std;

//ROOT
#include "TFile.h"
#include "TTree.h"

//UpsilonAna
#include "StJetMuEvent.h"
#include "StJetFilter.h"
    
ClassImp(StJetFilter)
    
    StJetFilter::StJetFilter(ioType type, const char* file)
	: mIoType(type), mEventCounter(0), mFile(0), mTree(0), mEvent(new StJetMuEvent)
{
    cout <<"StJetFilter::StJetFilter()"<<endl;

    if (type==kWrite) {
	cout <<"\tOpen file in write mode"<<endl;
	mFile = new TFile(file,"RECREATE");
	mFile->SetCompressionLevel(9);
	
	//Stole these lines from Jamie/Gans
	mTree = new TTree("mTree","StUpsilon Tree");
	mTree->SetBranchStyle(0);
	
	Int_t bufsize = 20000;
	Int_t split = 1;
	
	mTree->Branch("StJetMuEvent","StJetMuEvent",&mEvent,bufsize,split);
    }
    else if (type==kRead) {
	cout <<"\tOpen file in read mode"<<endl;
	mFile = new TFile(file,"READ");
	
	//Retrieve the TTree from the file
	TTree* temp = dynamic_cast<TTree*>(mFile->Get("mTree"));
	if (!temp) {
	    cout <<"Could not recover TTree from file:\t"<<file<<"  Prepare to crash"<<endl;
	}
	mTree=temp;
	mTree->SetBranchAddress("StJetMuEvent",&mEvent);
	
    }
    else {
	cout <<"StJetFilter::StJetFilter(). ERROR:\t"
	     <<"Undefined ioType.  Abort"<<endl;
    }
}

StJetFilter::~StJetFilter()
{
    cout <<"StJetFilter::~StJetFilter()"<<endl;

    if (mIoType==kWrite) {
	mFile->Write();
    }
    mFile->Close();

    /*
      delete mFile;
      mFile=0;
      delete mTree;
      mTree=0;
    */
    delete mEvent;
    mEvent=0;
}

int StJetFilter::nEvents() const
{
    return (mTree) ? static_cast<int>(mTree->GetEntries()) : 0;
}

void StJetFilter::setCuts(const AnaCuts& c)
{
    mEvent->setCuts(c);
}

void StJetFilter::fill(StMuDstMaker* maker)
{
    //cout <<"StJetFilter::fill(StMuDstMaker*)"<<endl;
    if (mIoType==kWrite) {
	if (mEvent->fill(maker)) {
	    cout <<"Event Accepted. Fill TTree"<<endl;
	    mTree->Fill();
	}
	else {
	    cout <<"Event Rejected, write log"<<endl;
	}
    }
    else if (mIoType==kRead) {
	//cout <<"\tRead event:\t"<<mEventCounter<<endl;
	if (mEventCounter<mTree->GetEntries()) {
	    mTree->GetEntry(mEventCounter++);
	}
	else {
	    cout <<"StJetFilter::fill(StMuDstMaker*). Error:\t"
		 <<"No more entries to get.  No action taken"<<endl;
	}
    }
    else {
	cout <<"StJetFilter::fill(StMuDstMaker*).  ERROR:\t"
	     <<"Undefined ioType.  No action taken"<<endl;
    }
}
