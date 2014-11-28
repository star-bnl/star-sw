///////////////////////////////////////////////////////////////////////////////
//
///////////////////////////////////////////////////////////////////////////////

#include "iostream.h"

class    StChain;
StChain  *chain=0;


void Help()
{
    cout << "Usage: trigCtb.C(startEvent,nEvents,\"path/some_dst_file.xdf\")" << endl;
    cout << "       trigCtb.C(nEvents,\"path/*.event.root\")" << endl;
    cout << "       trigCtb.C(nEvents,\"path/file.dst.root\",\"evout\")" << endl;	
}
//		ProtoTypes

void trigCtb(Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);
void trigCtb(Int_t startEvent, Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);

void trigCtb(Int_t nEvents=2, 
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
              const Char_t *qaflag = 0); 

void trigCtb(Int_t startEvent=1,Int_t nEvents=2, 
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
              const Char_t *qaflag = 0);

void trigCtb(const Int_t nEvents=2, 
              const Char_t *path,
              const Char_t *file,
              const Char_t *qaflag, int flag);
              
              
              
              
// ------------------ Here is the actual method -----------------------------------------
void trigCtb(Int_t startEvent, Int_t nEvents, const Char_t **fileList, const Char_t *qaflag)
{

  TString tflag = qaflag;
  cout <<  endl << endl <<" trigCtb -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " trigCtb -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
  cout << " trigCtb -  input qaflag   = " << qaflag << endl;
 
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
    // gSystem->Load("StEventMaker");
    gSystem->Load("StTrgMaker");

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
 
    TString mainBranch;
    if (fileList && fileList[0] && strstr(fileList[0],".root")) {
      mainBranch = fileList[0];
      mainBranch.ReplaceAll(".root","");
      int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
      mainBranch.Replace(0,idot+1,"");
      mainBranch+="Branch";
    }

    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");	//deactivate all branches
     if(!mainBranch.IsNull())	IOMk->SetBranch(mainBranch,0,"r");  
//     IOMk->SetBranch("dstBranch",0,"r");
//     IOMk->SetBranch("runcoBranch",0,"r");
     IOMk->SetDebug();

    //
    // Maker to read events from file or database into StEvent
    //
    // StEventMaker *readerMaker =  new StEventMaker("events","title");

    //
    //  Sample analysis maker
    //
    StTrgMaker *analysisMaker = new StTrgMaker("analysis");

    // WriteOut StEvent
    Int_t wrStEOut = tflag.Contains("evout",TString::kIgnoreCase);
    if (wrStEOut) {
      cout << "!!!! trigCtb: will write out .event.root file !!" << endl << endl;
      StTreeMaker *outMk = new StTreeMaker("EvOut","","bfcTree");
        outMk->SetIOMode("w");
        outMk->SetBranch("eventBranch","test.event.root","w");
        outMk->IntoBranch("eventBranch","StEvent");
    }

    //
    // Initialize chain
    //
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();


//----- added 6/20/00 by Kathy
  TTable   *tabl=0;
  TDataSet *obj=0;
  TDataSet *ddb=0;
  TDataSet *ddstBranch=0;
//------

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
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}

//------------------ added 6/20/00 by Kathy to unpack BfcStatus table
     if (!istat) {
         
       ddstBranch=chain->GetDataSet("dstBranch");

       TDataSetIter dstbranchIter(ddstBranch);

       if (ddstBranch) {

       cout << endl << " QAInfo: in dstBranch " << endl;

         while (ddb=dstbranchIter.Next()) {

         cout << endl << 
             " QAInfo:   found object: " << ddb->GetName() << endl;      
           
         TString dsName =  ddb->GetName();

           if (ddb->InheritsFrom("TTable")) { 

             tabl = (TTable *)ddb;
             cout << " QAInfo:     it is a table with #rows = " 
                        << tabl->GetNRows() << endl;

             if (dsName == "BfcStatus") {	
// Now print out contents of BfcStatus for QA purposes
               TDataSetIter bfcstatiter(ddb);
               St_dst_bfc_status *bfcstat = 
                 (St_dst_bfc_status *) bfcstatiter.Find("BfcStatus");
               St_dst_bfc_status::iterator bth    = bfcstat->begin();
               St_dst_bfc_status::iterator bthEnd = bfcstat->end();
//  loop over all rows in table BfcStatus:
               for (; bth != bthEnd; bth++)
               {
	         cout << " QAInfo:       BfcStatus table -- row " << ij <<
		   ", Maker: "     <<  (*bth).maker_name <<
                   " has istat = "  << (*bth).status << endl;	
	       }   // for bfcstat
             }  // if dsName
           } // if ddb
	 }  // while obj Next
       } // if dstBranch
     } //  if !istat

//------------------

     i++;
     goto EventLoop;
 }

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

}

//--------------------------------------------------------------------------

void trigCtb(const Int_t startEvent, const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    printf("*file = %s\n",file);
    if (nEvents==-1) { Help(); return;}
    const char *fileListQQ[]={0,0};
    if (strncmp(file,"GC",2)==0) {
      fileListQQ=0;
    } else {
	fileListQQ[0]=file;
    }
    trigCtb(startEvent,nEvents,fileListQQ,qaflag);
}
//--------------------------------------------------------------------------
void trigCtb(const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    trigCtb(1,nEvents,file,qaflag);
}

//--------------------------------------------------------------------------
void trigCtb(const Int_t nEvents, const Char_t *path,const Char_t *file, const Char_t *qaflag, int flag)
{
    TString F;
    if (path && path[0] && path[0]!='-') F = path;
    if (file && file[0] && file[0]!='-') 
    {
       if (!F.IsNull()) F +="/"; 
       F += file;
    }
    TString opt = qaflag;
    if (flag) opt += " evout";


    trigCtb(1,nEvents,F.Data(),opt.Data());
}

//--------------------------------------------------------------------------
void trigCtb(Int_t nEvents, const Char_t **fileList, const Char_t *qaflag)
{ trigCtb(1,nEvents,fileList,qaflag); }

