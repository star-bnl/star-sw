#include <string>
#include "TROOT.h"
#include "TChain.h"
#include "TSystem.h"
#include "time.h"

//Note, if you run this compiled, then you're fine.  If not, you have to manually set npos to 1

int findEntries(string infile);

void MakeChain()
{
    clock_t begin = clock();

    cout <<"MakeChain()"<<endl;
    TChain* chain = new TChain("mTree");

    void *pDir = gSystem->OpenDirectory("./"); //open this directory
    
    // now find the files that end in the specified extention
    const char* fileName(0);
    int fileCount=0;
    int total=0;
    
    while((fileName = gSystem->GetDirEntry(pDir))) { //loop on files

	string file(fileName);
	if ( (file.find(".MuDst.root")!=-1) && (file.find("_has_")!=-1) ) {
	    int n=findEntries(file);
	    cout <<"file number "<<fileCount<<":\t"<<fileName<<"\twith nEntries:\t"<<n<<endl;
	    ++fileCount;
	    chain->AddFile(file.c_str(), n);
	    total+=n;
	}
    }
    cout <<"\n TChain::GetEntries():\t"<<chain->GetEntries()<<"\t total:\t"<<total<<endl;

    cout <<"Merge chain"<<endl;
    chain->Merge("Chain.root");

    clock_t end = clock();
    cout <<"\n Elapsed Time:\t"<<static_cast<double>(end-begin)/static_cast<double>(CLOCKS_PER_SEC)<<endl;
}

int findEntries(string infile)
{
    //cout <<"infile:\t"<<infile<<endl;
    string begin = "_has_";
    string end = "_events";
    int where1 = infile.find(begin);
    int where2 = infile.find(end);
    int npos = infile.npos; //for compiled code
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
