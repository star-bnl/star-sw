#include <string>
#include "TROOT.h"
#include "TChain.h"
#include "TSystem.h"
#include "time.h"

//Note, if you run this compiled, then you're fine.  If not, you have to manually set npos to 1

void MakeChain2()
{
    clock_t begin = clock();
    
    cout <<"MakeChain2()"<<endl;
    TChain* chain = new TChain("mTree");
    chain->Add("*.MuDst.root*");
    cout <<"TChain::GetEntries():\t"<<chain->GetEntries()<<endl;

    cout <<"Merge chain"<<endl;
    chain->Merge("Chain2.root");
    
    clock_t end = clock();
    cout <<"\n Elapsed Time:\t"<<static_cast<double>(end-begin)/static_cast<double>(CLOCKS_PER_SEC)<<endl;
}
