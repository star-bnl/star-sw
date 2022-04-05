///////////////////////////////////////////////////////////////////////////////
//
// $Id: runPeCMaker.C,v 1.10 2010/03/16 16:23:09 fisyak Exp $
//
// Description: 
// Chain for StPeCMaker based on doEvents.C. Runs StEventMaker and StPeCMaker.
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
// .x runPeCMaker.C(10,"-","some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x runPeCMaker.C(10,"-","some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x runPeCMaker.C(9999,"some_directory","*.dst.root")
//
// Author List: Joakim Nystrand, LBNL  3/00 (+authors of doEvents.C)
//  
///////////////////////////////////////////////////////////////////////////////
//
// $Log: runPeCMaker.C,v $
// Revision 1.10  2010/03/16 16:23:09  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 1.9  2006/08/15 21:42:49  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.8  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.7  2001/09/21 02:21:57  jeromel
// StTpcDb needed by StEventMaker.
//
// Revision 1.6  2000/05/09 19:38:35  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.5  2000/04/18 20:20:33  nystrand
// StFile --> StFileI + updated chain return codes
//
// Revision 1.4  2000/04/13 21:46:35  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.3  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.2  2000/04/03 20:47:32  nystrand
// Bug fix
//
// Revision 1.1  2000/03/28 23:52:24  nystrand
// First version
//
// Revision 1.0  2000/03/27 18:20:20  ullrich
// First version.
// 
///////////////////////////////////////////////////////////////////////////////
Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

//void runPeCMaker()
//{
//    cout << "Usage: runPeCMaker.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
//    cout << "       runPeCMaker.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
//    cout << "       runPeCMaker.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
//}

void runPeCMaker(Int_t,const Char_t **,const char *qaflag = "");

void runPeCMaker(Int_t nevents=2, 
              const Char_t *path="-",
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
              const char *qaflag = "off");


void runPeCMaker(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
    // Load shared libraries
    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");


    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEventMaker");
    gSystem->Load("StPeCMaker");
    cout<<"Shared Libraries Loaded!"<<endl;

    chain  = new StChain("StChain");

    StFileI *setFiles= new StFile();

    for (int ifil=0; fileList[ifil]; ifil++){ 
      setFiles->AddFile(fileList[ifil]);
    }
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("dstBranch",0,"r");
    IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetDebug();

    // Maker to read events from file or database into StEvent
    StEventMaker *readerMaker =  new StEventMaker("events","title");

    // The StPeCMaker
    StPeCMaker *analysisMaker = new StPeCMaker("analysis");

    // Initialize chain
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();

    // Event loop
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
    chain->Clear();
    chain->Finish();

}

void runPeCMaker(const Int_t nevents, const Char_t *path, const Char_t *file,const char *qaflag)
{
    const char *fileListQQ[]={0,0};
    if (path[0]=='-') {
	fileListQQ[0]=file;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    runPeCMaker(nevents,fileListQQ,qaflag);
}








