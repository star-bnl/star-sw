//ChainMerger.cxx

//std
#include "Stiostream.h"
#include <string>
#include <ctime>
#include <stdlib.h>

//ROOT
#include "TSystem.h"
#include "TROOT.h"
#include "TChain.h"
#include "TTree.h"
#include "TFile.h"

//UpsilonAna
#include "StJetMuEvent.h"
#include "ChainMerger.h"

ClassImp(ChainMerger)
    
ChainMerger::ChainMerger(const char* dir, const char* outfile)
    : mChain(new TChain("mTree")), mEvent(new StJetMuEvent())
{
    cout<<"ChainMerger::ChainMerger()"<<endl;
    buildChain(string(dir), string(outfile));
}

ChainMerger::~ChainMerger()
{
    cout<<"ChainMerger::~ChainMerger()"<<endl;
}

void ChainMerger::buildChain(string dir, string outfile)
{
    cout <<"ChainMerger::buildChain(string, string)"<<endl;

    cout <<"SetBranchAddress"<<endl;
    mChain->SetBranchAddress("StJetMuEvent",&mEvent);
    
    clock_t begin = clock();
    
    cout <<"MakeChain()"<<endl;
    void *pDir = gSystem->OpenDirectory(dir.c_str());
    
    // now find the files that end in the specified extention
    const char* fileName(0);
    int fileCount=0;
    int total=0;
    
    while((fileName = gSystem->GetDirEntry(pDir))) { //loop on files

	string file(fileName);
	if ( (file.find(".MuDst.root")!=file.npos) && (file.find("_has_")!=file.npos) ) {
	    int n=findEntries(file);
	    cout <<"file number "<<fileCount<<":\t"<<fileName<<"\twith nEntries:\t"<<n<<endl;
	    ++fileCount;
	    mChain->AddFile(file.c_str(), n);
	    total+=n;
	}
    }
    cout <<"\n TChain::GetEntries():\t"<<mChain->GetEntries()<<"\t total:\t"<<total<<endl;

    cout <<"Merge chain"<<endl;
    mChain->Merge(outfile.c_str());

    clock_t end = clock();
    cout <<"\n Elapsed Time:\t"<<static_cast<double>(end-begin)/static_cast<double>(CLOCKS_PER_SEC)<<endl;
}

int ChainMerger::findEntries(string infile)
{
    //cout <<"infile:\t"<<infile<<endl;
    string begin = "_has_";
    string end = "_events";
    int where1 = infile.find(begin);
    int where2 = infile.find(end);
    //int npos = infile.npos; //for compiled code
    //int npos=-1 //for interpreted code

    //cout <<"npos:\t"<<npos<<"\twhere1:\t"<<where1<<"\twhere2:\t"<<where2<<endl;
    
    int start=where1+begin.size();
    int stop=where2;

    //cout <<"numbers of events is between indices: ["<<start<<","<<stop<<")"<<endl;

    //cout <<"Get number of events"<<endl;

    string number;
    for (int i=start; i<stop; ++i) {
	number += infile[i];
    }
    
    int nevents = atoi(number.c_str());

    //cout <<"Number of events as string:\t"<<number<<endl;
    //cout <<"Number of events as int:   \t"<<nevents<<endl;
    
    //cout <<"Done"<<endl;
    
    return nevents;
}
