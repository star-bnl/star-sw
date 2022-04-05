//////////////////////////////////////////////////////////////////////////////
//
// $Id: doEEmcSmd.C,v 1.2 2006/08/15 21:41:37 jeromel Exp $
//
// Description: 
//  Run StEEmcSmdAnalysisMaker.
//  Heavily based on doEvents.C.
//      
///////////////////////////////////////////////////////////////////////////////

#include "iostream"
using namespace std;

class     StChain;
StChain  *chain=0;
Int_t iEvt=0,istat=0,nEvents=0;
void doEEmcSmd()
{
    cout << "Usage: doEEmcSmd.C(2)  // work with default event.root file" << endl;
    cout << "       doEEmcSmd.C(startEvent,nEvents,\"path/somefile.event.root\")" << endl;
    cout << "       doEEmcSmd.C(nEvents,\"path/*.event.root\")" << endl;
    cout << "       doEEmcSmd.C(nEvents,\"path/file.dst.root\",\"evout\") //Write out StEvent" << endl;	
    cout << "       doEEmcSmd.C(nEvents,\"path/file.dst.root\",\"display\") //EventDispay" << endl;	
}

//		ProtoTypes
void doEEmcSmd(Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);
void doEEmcSmd(Int_t startEvent, Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);

void doEEmcSmd(Int_t nEvents, 
              const Char_t *file="AuPb200-100.event.root",
              const Char_t *qaflag = 0); 

void doEEmcSmd(Int_t startEvent,Int_t nEvents, 
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const Char_t *qaflag = 0);

void doEEmcSmd(const Int_t nEvents, 
              const Char_t *path,
              const Char_t *file,
              const Char_t *qaflag, int flag);
              
              
              
              
// ------------------ Here is the actual method -----------------------------------------
void doEEmcSmd(Int_t startEvent, Int_t nEventsQQ, const Char_t **fileList, const Char_t *qaflag)
{

  nEvents = nEventsQQ;
  TString tflag = qaflag;
  int eventDisplay = tflag.Contains("disp",TString::kIgnoreCase);

  cout <<  endl << endl <<" doEEmcSmd -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doEEmcSmd -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
//  cout << " doEEmcSmd -  input qaflag   = " << qaflag << endl;
 
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
    gSystem->Load("StEventUtilities");
    gSystem->Load("StMagF");
    gSystem->Load("libtpc_Tables");

    gSystem->Load("StTpcDb");
    gSystem->Load("StEEmcSmdAnalysisMaker");
    gSystem->Load("StEEmcUtil");

    // dbase related libraries
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so");
    gSystem->Load("libStDb_Tables.so");

    gSystem->Load("St_db_Maker.so");

//   		Special libraries for EventDisplay
    if (eventDisplay) {//EventDisplay on
       gSystem->Load("St_g2t");
       gSystem->Load("geometry");
       gSystem->Load("St_geant_Maker");
       gSystem->Load("StTableUtilities");
       gSystem->Load("StEventDisplayMaker");
    }

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
 
//   		Geant maker  for EventDisplay
    if (eventDisplay) {
       int NwGeant=5000000, IwType=0, NwPaw=0;
       St_geant_Maker *geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
       geantMk->LoadGeometry("detp geometry year2001");
       geantMk->SetActive(kFALSE);
    }


    TString mainBranch;
    if (fileList && fileList[0] && strstr(fileList[0],".root")) {
      mainBranch = fileList[0];
      printf("fileList[0] %s %s\n",fileList[0],mainBranch.Data());
      mainBranch.ReplaceAll(".root","");
      int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
      mainBranch.Replace(0,idot+1,"");
      mainBranch+="Branch";
      printf("*** mainBranch=%s ***\n",mainBranch.Data());
    }

    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
     IOMk->SetIOMode("r");
     IOMk->SetBranch("*",0,"0");	//deactivate all branches
     if(!mainBranch.IsNull())	IOMk->SetBranch(mainBranch,0,"r");  
     IOMk->SetBranch("dstBranch",0,"r");

     IOMk->SetDebug();

    //
    // Maker to read events from file or database into StEvent
    //
    if (!mainBranch.Contains("eventBranch")) {
      gSystem->Load("StTpcDb");
      gSystem->Load("StEventMaker");
      StEventMaker *readerMaker =  new StEventMaker("events","title");
    }

    //
    // Db Maker
    //
    char *db2 = "StarDb";
    if (gSystem->AccessPathName(db2) !=0) {
	printf("File %s does not exist\n",db2);
	db2 = "";
    }
    St_db_Maker *dbMk = new St_db_Maker("dbName","MySQL:StarDb","$STAR/StarDb",db2);
    dbMk->SetDateTime(010102,000);
    //
    // EEmc SMD Analysis maker
    //
    StEEmcSmdAnalysisMaker *analysisMaker = new StEEmcSmdAnalysisMaker("EEmcSmdAna");
//    analysisMaker->setHistoFileName("MySmdAdc.root");
//  analysisMaker->setSectors(5,6,7,0);
    analysisMaker->setOuterTrackGeometry();
    analysisMaker->setAnalysisSectorId(6);
    analysisMaker->setMinMomentum(1.0);
    analysisMaker->setMaxGapForMatch(3);


    // WriteOut StEvent
    Int_t wrStEOut = tflag.Contains("evout",TString::kIgnoreCase);
    if (wrStEOut) {
      cout << "!!!! doEEmcSmd: will write out .event.root file !!" << endl << endl;
      StTreeMaker *outMk = new StTreeMaker("EvOut","","bfcTree");
        outMk->SetIOMode("w");
        outMk->SetBranch("eventBranch","test.event.root","w");
        outMk->IntoBranch("eventBranch","StEvent");
    }


//   		 StEventDisplayMaker
    if (eventDisplay) {

      StEventDisplayMaker *displayMk = new StEventDisplayMaker();
      displayMk->AddName("StEvent(All Tracks)");
      displayMk->AddFilter(new StFilterDef("MainFilter"));
     
    }

    //
    // Initialize chain
    //
    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();
    if (startEvent > 1) IOMk->Skip(startEvent-1);



//----- added 6/20/00 by Kathy
  TTable   *tabl=0;
  TDataSet *obj=0;
  TDataSet *ddb=0;
  TDataSet *ddstBranch=0;
//------

    //
    // Event loop
    //
    istat=0,iEvt=1;
    
 EventLoop: if (iEvt <= nEvents && istat!=2) {

     cout << endl << "=== Event " << iEvt << " start ===" << endl;

     chain->Clear();
     istat = chain->Make(iEvt);

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}


     iEvt++;
     goto EventLoop;
 }

    iEvt--;
    cout << endl << "=== Event " << iEvt << " finish ===" << endl;

}

//--------------------------------------------------------------------------

void doEEmcSmd(const Int_t startEvent, const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    printf("*file = %s\n",file);
    const char *fileListQQ[]={0,0};
    if (strncmp(file,"GC",2)==0) {
      fileListQQ=0;
    } else {
	fileListQQ[0]=file;
    }
    doEEmcSmd(startEvent,nEvents,fileListQQ,qaflag);
}
//--------------------------------------------------------------------------
void doEEmcSmd(const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    doEEmcSmd(1,nEvents,file,qaflag);
}

//--------------------------------------------------------------------------
void doEEmcSmd(const Int_t nEvents, const Char_t *path,const Char_t *file, const Char_t *qaflag, int flag)
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


    doEEmcSmd(1,nEvents,F.Data(),opt.Data());
}

//--------------------------------------------------------------------------
void doEEmcSmd(Int_t nEvents, const Char_t **fileList, const Char_t *qaflag)
{ doEEmcSmd(1,nEvents,fileList,qaflag); }

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doEEmcSmd.C,v $
// Revision 1.2  2006/08/15 21:41:37  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.1  2003/11/25 20:12:32  wzhang
// first version
//

//////////////////////////////////////////////////////////////////////////

