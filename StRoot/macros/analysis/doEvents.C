///////////////////////////////////////////////////////////////////////////////
//
// $Id: doEvents.C,v 1.39 2000/01/11 18:20:20 ullrich Exp $
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
// found, and 'nevents' events from each will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"-","some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x doEvents.C(10,"-","some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x doEvents.C(9999,"some_directory","*.dst.root")
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//  
///////////////////////////////////////////////////////////////////////////////
//
// $Log: doEvents.C,v $
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
Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;
TBrowser *b=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void doEvents()
{
    cout << "Usage: doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doEvents.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}
void doEvents(Int_t,const Char_t **,const char *qaflag = "");
void doEvents(Int_t nevents=999, const Char_t *path, const Char_t *file,
              const char *qaflag = "off");


void doEvents(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
    //
    // First load some shared libraries we need
    //
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StMagF");
    gSystem->Load("StEventMaker");
    gSystem->Load("StAnalysisMaker");

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");

    StFile *setFiles= new StFile();

    for (int ifil=0; fileList[ifil]; ifil++)
	{ setFiles->AddFile(fileList[ifil]);}
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetDebug();


    //
    // Maker to read events from file or database into StEvent
    //
    StEventMaker *readerMaker =  new StEventMaker("events","title");


    //
    //  Sample analysis maker
    //
    StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");

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
 EventLoop: if (i <= nevents && !istat) {
     cout << "============================ Event " << i
	  << " start ============================" << endl;
     chain->Clear();
     istat = chain->Make(i);
     if (istat) {cout << "Last event processed. Status = " << istat << endl;}
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

void doEvents(const Int_t nevents, const Char_t *path, const Char_t *file,const char *qaflag)
{
    const char *fileListQQ[]={0,0};
    if (path[0]=='-') {
	fileListQQ[0]=file;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    doEvents(nevents,fileListQQ,qaflag);
}


// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
//////////////////////////////////////////////////////////////////////////






