///////////////////////////////////////////////////////////////////////////////
//
// $Id: doCopy.C,v 1.2 2011/07/19 20:11:25 perev Exp $
//
///////////////////////////////////////////////////////////////////////////////
#include "iostream.h"


class    StChain;
StChain  *chain=0;


//		ProtoTypes

void doCopy(Int_t startEvent, Int_t nEvents,
            const Char_t *file,
            const Char_t *dirout = "QWE.dir"); 

              
// ------------------ Here is the actual method -----------------------------------------

"void doCopy(Int_t startEvent, Int_t nEvents,
            const Char_t *file,
            const Char_t *dirout) 
{
  if (!dirout || !dirout[0]) dirout  = "QWE.dir";
     const char *cwd = gSystem->WorkingDirectory();

     if (strcmp(dirout,"./")==0 || strcmp(dirout,cwd)==0)
     {			//Not allowed
       printf("doCopy: ERROR *** current dir not allowed for dirout ***");
       return;
     }
  gSystem->MakeDirectory(dirout);

  cout <<  endl << endl <<" doCopy -  input # events = " << nEvents << endl;
  Int_t ilist=0;
      cout << " doCopy -  input file = " << file   << endl;
      cout << " doCopy -  ouput dir  = " << dirout << endl;
 
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

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");
    StFileI *setFiles =0;
    char *fileList[2];
    fileList[0] = file; fileList[1]=0;

    setFiles= new StFile(fileList);
 

    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
     IOMk->SetIOMode("r");
     IOMk->SetBranch("dstBranch"    ,0,"r");
     IOMk->SetBranch("geantBranch"  ,0,"r");
     IOMk->SetBranch("histBranch"   ,0,"r");
     IOMk->SetBranch("runcoBranch"  ,0,"r");
     IOMk->SetBranch("eventBranch"  ,0,"r");
     IOMk->SetDebug();
     if (startEvent>1) IOMk->Skip(startEvent-1);

    // WriteOut 
     TString basename = gSystem->BaseName(file);
     basename.ReplaceAll(".root","");
     char *outName  = gSystem->ConcatFileName(dirout,basename);


      StTreeMaker *outMk = new StTreeMaker("EvOut",outName,"bfcTree");
        outMk->SetIOMode("w");
        outMk->IntoBranch("eventBranch","StEvent");
        outMk->IntoBranch("dstBranch"  ,"dst");
        outMk->IntoBranch("histBranch" ,"hist");
        outMk->IntoBranch("runcoBranch","runco");
        outMk->IntoBranch("geantBranch","geant");
      outMk->SetDebug();

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
 EventLoop: if (i <= nEvents && istat!=2) {

     cout << endl << "============================ Event " << i
	  << " start ============================" << endl;

     chain->Clear();
     istat = chain->Make(i);

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl; break;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}

     i++;
     goto EventLoop;
 }

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

}
