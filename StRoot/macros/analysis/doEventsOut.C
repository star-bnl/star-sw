///////////////////////////////////////////////////////////////////////////////
//
// $Id: doEventsOut.C,v 1.2 2000/04/28 03:47:42 perev Exp $
//
// Description: 
// Chain to read events from files or database into StEvent and analyze.
// what it does: reads .dst.root or .xdf files and then runs StEventMaker
//          to fill StEvent and StAnalysisMaker to show example of analysis
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and the first 'nevents' events in the file set will be
// analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
// If 'file ends in '.xdf', XDF DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEventsOut.C(10,"-","some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x doEventsOut.C(10,"-","some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x doEventsOut.C(9999,"some_directory","*.dst.root")
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
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
    cout << "Usage: doEventsOut.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doEventsOut.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doEventsOut.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}
void doEventsOut(Int_t,const Char_t **,const char *qaflag = "");

void doEventsOut(Int_t nevents=-1
                ,const Char_t *path="/star/data03/reco/auau200/mevsim/vcascade/resonance/year_1h/hadronic_on/tfs_6/rcf0097_106_50evts.dst.root"
                ,const Char_t *file="",const char *qaflag="off");


void doEventsOut(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
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
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StMagF");
    gSystem->Load("StEventMaker");
    gSystem->Load("StAnalysisMaker");

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");
    StFileI *setFiles =0;
    if (fileList) {	//Normal case
      setFiles= new StFile(fileList);
    } else        {	//Grand Chalenge
      gSystem->Load("StChallenger");
      setFiles = StChallenger::Challenge();
      setFiles->SetDebug();
      Int_t Argc=4;
      const char *Argv[4]= {
        "-s","dst;hist;runco",
        "-q","-5<=qxa_3<0.3 && 22>qxc_1>18"
        };
      setFiles->Init(Argc,Argv);
    }
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");                 //deactivate all branches
     IOMk->SetBranch("dstBranch",0,"r");
     IOMk->SetBranch("runcoBranch",0,"r");
     IOMk->SetDebug();


    //
    // Maker to read events from file or database into StEvent
    //
    StEventMaker *readerMaker =  new StEventMaker("events","title");


    //
    //  Sample analysis maker
    //
//    StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");

    // WriteOut StEvent
    StTreeMaker *outMk = new StTreeMaker("EvOut","","bfcTree");
    outMk->SetIOMode("w");
    outMk->SetBranch("eventBranch","test.event.root","w");
    outMk->IntoBranch("eventBranch","StEvent");
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
     cout << "============================ Event " << i
	  << " start ============================" << endl;
     chain->Clear();
     istat = chain->Make(i);
     if (istat==2) {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) {cout << "Error event processed. Status = " << istat << endl;}
     i++;
     goto EventLoop;
 }

    i--;
    cout << "============================ Event " << i
	 << " finish ============================" << endl;
    if (nevents > 1) {
	chain->Clear();
	chain->Finish();
    }
    else {
	if (!b) {
	    b = new TBrowser;
	}
    }
}

void doEventsOut(Int_t nevents,const Char_t *path,const Char_t *file,const char *qaflag)
{
    if (nevents==-1) { Help(); return;}
    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
      fileListQQ=0;
    } else if (path[0]=='-') {
	fileListQQ[0]=file;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }

    doEventsOut(nevents,fileListQQ,qaflag);
}










///////////////////////////////////////////////////////////////////////////////
//
// $Log: doEventsOut.C,v $
// Revision 1.2  2000/04/28 03:47:42  perev
// Change EvOut.root name to more regular
//
// Revision 1.1  2000/04/25 21:01:33  perev
// example macro for writing StEvent
//
// Revision 1.49  2000/04/21 13:40:08  wenaus
// correct the doc for nevents in multifile mode
//
// Revision 1.48  2000/04/18 21:43:12  fine
// make TurnDisplay macro available for doEventsOut
//
// Revision 1.47  2000/04/13 22:14:03  perev
// StFile -> StFileI
//
// Revision 1.46  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.45  2000/04/12 17:33:45  kathy
// put loading of libtpc_Tables back in since Iwona is going back to original tpt_track table
//
// Revision 1.44  2000/04/12 15:29:05  kathy
// comment out libtpc by default
//
// Revision 1.43  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.42  2000/04/07 15:54:26  perev
// GC added
//
// Revision 1.41  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.40  2000/03/17 23:10:06  kathy
// make sure the dst branch is explicitly set in the macros using dst.root files as input - otherwise they don't work properly with soft links
//
// Revision 1.39  2000/01/11 18:20:20  ullrich
// Add latests improvements from Victor.
//
// Revision 1.38  2000/01/10 22:06:09  kathy
// add owner name and comments
//
// Revision 1.37  1999/11/17 14:34:00  ullrich
// Added version with no arguments which prints usage info.
//
// Revision 1.36  1999/11/17 14:23:40  ullrich
// Updated for new StEvent/StEventMaker.
//
// owner: Torre Wenaus,Victor Perevoztchikov
// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
///////////////////////////////////////////////////////////////////////////////





