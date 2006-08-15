///////////////////////////////////////////////////////////////////////////////
//
// $Id: doHists.C,v 3.4 2006/08/15 21:42:44 jeromel Exp $
//
// Description: 
// Chain to read production histgrams from files or database and acculuate it across all files read
// what it does: reads .hist.root then runs StHistCollectorMaker
//          to merge the valus if all similar histograms
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and the first 'nevents' events in the file set will be
// analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.hist.root', ROOT histBranch 'es of the STAR bfc.C production 
// are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example ROOT file invocation:
// .x doHists.C(10,"-","some_directory/some_hist_file.root")
//
// example multi-ROOT file invocation:
// .x doHists.C(9999,"some_directory","*.hist.root")
//
//////////////////////////////////////////////////////////////////////////////
//
// Author: Valeri Fine, BNL  11/2000
// (This macro based on doEvents.C by Torre Wenaus and Victor Perevoztchikov)
//  
//  inputs:
//      nevents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the 1 particular full file name (with directory) you want
//      qaflag = "off"  - doesn't do anything now
//      wrStEOut = flag to turn on=1, off=0 writing of output test.event.root
//                 file --- set to off by default 
//      
///////////////////////////////////////////////////////////////////////////////

Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;
class StEventDisplayMaker;
StEventDisplayMaker *dsMaker = 0;
TBrowser *b=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void Help()
{
    cout << "Usage: doHists.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doHists.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doHists.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}


void doHists(Int_t, const Char_t **, const Char_t *qaflag = "");

void doHists(Int_t nevents=2, 
              const Char_t *path="/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
              const Char_t *file="",
              const Char_t *qaflag = "off", 
              const Int_t wrStEOut = 0);

// ------------------ Here is the actual method -----------------------------------------
void doHists(Int_t nevents, const Char_t **fileList, const Char_t *qaflag, const Int_t wrStEOut)
{

  cout <<  endl << endl <<" doHists -  input # events = " << nevents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doHists -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
  cout << " doHists -  input qaflag   = " << qaflag << endl;
  cout << " doHists -  input wrStEOut = " << wrStEOut << endl << endl << endl;
 
    //
    // First load some shared libraries we need
    //

    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");

    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StAnalysisUtilities");

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");
    StFileI *setFiles =0;
    if (fileList) {	//Normal case
      setFiles= new StFile(fileList);
    } else        {	//Grand Challenge
      gSystem->Load("StChallenger");
      setFiles = StChallenger::Challenge();
      setFiles->SetDebug();
      const char *Argv[]= {
	    "-s","dst runco",                           // list of components needed
	    "-q","numberOfPrimaryTracks>1500",   // example of user query
	    "-c","/afs/rhic.bnl.gov/star/incoming/GCA/daq/stacs.rc"  // pointer to GC servers for daq
        };
      Int_t Argc=sizeof(Argv)/4;
      setFiles->Init(Argc,Argv);
    }
     StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");                 //deactivate all branches
     IOMk->SetBranch("histBranch",0,"r");
     IOMk->SetMaxEvent(1);
     IOMk->SetDebug();

    //
    // Maker to read events from file or database into StEvent
    //


    //

    //
    //  Sample analysis maker
    //
    StHistCollectorMaker *histsMaker = new StHistCollectorMaker("merged");

    // WriteOut Hist Merged
   if (wrStEOut) {
       cout << "!!!! doHists: will write out .event.root file !!" << endl << endl;
       StTreeMaker *outMk = new StTreeMaker("mergeOut","","mergeTree");
         outMk->SetIOMode ("w");
         outMk->SetBranch ("mergeBranch","test.merge.root","w","const");
         outMk->IntoBranch("mergeBranch","Merged");
     }
    //
    // Initialize chain
    //
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();


    //
    // Event loop
    //
    int istat=0,i=1;
 EventLoop: if (i <= nevents && istat!=2) {

     cout << endl << "============================ Event " << i
	  << " start ============================" << endl;

     chain->Clear();
     istat = chain->Make(i);

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}
     i++;
     goto EventLoop;
 }

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;
    new TBrowser("AccumulatedHistograms",histsMaker->FindByName(".const"));

}

//--------------------------------------------------------------------------

void doHists(const Int_t nevents, const Char_t *path, const Char_t *file,
              const Char_t *qaflag, const Int_t wrStEOut)
{
    if (nevents==-1) { Help(); return;}
    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
      fileListQQ=0;
    } else if (path[0]=='-') {
	fileListQQ[0]=file;
    } else if (!file[0]) {
	fileListQQ[0]=path;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    doHists(nevents,fileListQQ,qaflag,wrStEOut);
}

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doHists.C,v $
// Revision 3.4  2006/08/15 21:42:44  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 3.3  2001/02/14 18:19:36  perev
// add write mode
//
// Revision 3.2  2000/12/12 23:11:05  perev
// Add write of merged histos
//
// Revision 3.1  2000/11/30 19:35:13  fine
// New analysis utility to collect all histogram from all histBranh production branches
//
//////////////////////////////////////////////////////////////////////////






