///////////////////////////////////////////////////////////////////////////////
//
// $Id: mDstWrite.C,v 3.5 2010/03/16 16:23:09 fisyak Exp $
// Author: Thomas Ullrich, Oct 2000
//
//////////////////////////////////////////////////////////////////////////////
//
// Description: 
//
//////////////////////////////////////////////////////////////////////////////
//
// $Log: mDstWrite.C,v $
// Revision 3.5  2010/03/16 16:23:09  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 3.4  2006/08/15 21:42:47  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 3.3  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 3.2  2001/09/21 02:21:57  jeromel
// StTpcDb needed by StEventMaker.
//
// Revision 3.1  2000/10/13 19:23:46  ullrich
// Initial Revision
//
///////////////////////////////////////////////////////////////////////////////
#include <string>
class    StChain;
StChain  *chain=0;
class StEventDisplayMaker;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void Help()
{
    cout << "Usage: mDstWrite.C(nevents,\"-\",\"some_directory/some_dst_file.root\",\"outputfile\")" << endl;
    cout << "       mDstWrite.C(nevents,\"some_directory\",\"*.dst.root\",\"outputfile\")" << endl;
    cout << "       outputfile <name>:   data written to file <name>.mdst.root" << endl;
}

void mDstWrite(Int_t nevents = 0, const Char_t *path = 0, const Char_t *file = 0, const Char_t *outfile = 0);

void mDstWrite(Int_t nevents, const Char_t **fileList, const Char_t *outfile)
{
    cout <<  endl << endl << "mDstWrite -  input # events = " << nevents << endl;
    Int_t ilist=0;
    while(fileList[ilist]){ 
	cout << "mDstWrite -  input fileList = " << fileList[ilist] << endl;
	ilist++; 
    }

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
    gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEventMaker");
    gSystem->Load("StMiniDstMaker");
    
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
	    "-s","dst runco",                    // list of components needed
	    "-q","numberOfPrimaryTracks>1500",   // example of user query
	    "-c","/afs/rhic.bnl.gov/star/incoming/GCA/daq/stacs.rc"  // pointer to GC servers for daq
	};
	Int_t Argc=sizeof(Argv)/4;
	setFiles->Init(Argc,Argv);
    }

    //
    //  Setup IO maker
    //
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("dstBranch",0,"r");
    // Un-comment the following line if you read event.root files
    //    IOMk->SetBranch("eventBranch",0,"r");
    IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetDebug();
    
    //
    // Maker to read events from file or database into StEvent
    // (not needed if *.event.root files are used)
    //
    // Comment the following line if you read event.root files
    StEventMaker *readerMaker =  new StEventMaker("events","title");
    
    //
    //  Sample analysis maker
    //
    StMiniDstMaker *analysisMaker = new StMiniDstMaker("analysis");

    //
    // Setup IO for StEvent-mDST
    //
    string ofilename = outfile;
    ofilename += ".mdst.root";
    cout << "mDstWrite: output file is = " << ofilename.c_str() << endl << endl;
    StTreeMaker *outMk = new StTreeMaker("EvOut","","mDstTree");
    outMk->SetIOMode("w");
    outMk->SetBranch("mdstBranch",ofilename.c_str(),"w");
    outMk->IntoBranch("mdstBranch","StEvent");
    
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
    
}

void mDstWrite(const Int_t nevents, const Char_t *path, const Char_t *file,
              const Char_t *ofile)
{
    if (nevents==0 || ofile == 0) { Help(); return;}
    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
	fileListQQ=0;
    }
    else if (path[0]=='-') {
	fileListQQ[0]=file;
    }
    else if (!file[0]) {
	fileListQQ[0]=path;
    }
    else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    mDstWrite(nevents,fileListQQ,ofile);
}
