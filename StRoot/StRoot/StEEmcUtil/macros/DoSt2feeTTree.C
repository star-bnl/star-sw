///////////////////////////////////////////////////////////////////////////////
//
// $Id: DoSt2feeTTree.C,v 1.8 2009/02/04 20:33:30 ogrebeny Exp $
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
// found, and the first 'nEvents' events in the file set will be
// analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
// If 'file ends in '.xdf', XDF DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x doEvents.C(10,"some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x doEvents.C(9999,"some_directory/*.dst.root")
//
//////////////////////////////////////////////////////////////////////////////
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//  
//  inputs:
//      nEvents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the 1 particular full file name (with directory) you want
//      qaflag = "evout"    turn on writing of output test.event.root
//      qaflag = "display"  turn on EventDisplay
//                 file --- set to off by default 
//      
///////////////////////////////////////////////////////////////////////////////

#include "iostream.h"

class     StChain;
StChain  *chain=0;
class     St_db_Maker;
St_db_Maker *dbMk =0;

Int_t iEvt=0,istat=0,nEvents=0;
void doEvents()
{
    cout << "Usage: doEvents.C(2)  // work with default event.root file" << endl;
    cout << "       doEvents.C(startEvent,nEvents,\"path/somefile.event.root\")" << endl;
    cout << "       doEvents.C(nEvents,\"path/*.event.root\")" << endl;
    cout << "       doEvents.C(nEvents,\"path/file.dst.root\",\"evout\") //Write out StEvent" << endl;	
    cout << "       doEvents.C(nEvents,\"path/file.dst.root\",\"display\") //EventDispay" << endl;	
    cout << "       doEvents.C(nEvents,\"path/file.dst.root\",\"dbon\") //DB on" << endl;	
}
//		ProtoTypes

void doEvents(Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);
void doEvents(Int_t startEvent, Int_t nEvents, const Char_t ** fileList, const Char_t *qaflag =0);

void doEvents(Int_t nEvents, 
              const Char_t *file="st_physics_4095050_raw_0010002.event.root",
              const Char_t *qaflag = "dbon"); 

void doEvents(Int_t startEvent,Int_t nEvents, 
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const Char_t *qaflag = 0);

void doEvents(const Int_t nEvents, 
              const Char_t *path,
              const Char_t *file,
              const Char_t *qaflag, int flag);
              
              
              
              
// ------------------ Here is the actual method -----------------------------------------
void doEvents(Int_t startEvent, Int_t nEventsQQ, const Char_t **fileList, const Char_t *qaflag)
{

  nEvents = nEventsQQ;
  TString tflag = qaflag; tflag.ToLower();
  int eventDisplay = tflag.Contains("disp");

  cout <<  endl << endl <<" doEvents -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
  }
  cout << " doEvents -  input qaflag   = " << qaflag << endl;
 
  //
  // First load some shared libraries we need
  //
  gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");

    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker");    
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StMagF");
    gSystem->Load("StAnalysisMaker");


    //		DB ON
    if (tflag.Contains("dbon") || eventDisplay ) {
      gSystem->Load("libtpc_Tables");
      gSystem->Load("StDbLib.so");
      gSystem->Load("StDbBroker.so");
      gSystem->Load("libStDb_Tables.so");
      gSystem->Load("St_db_Maker.so");
      gSystem->Load("StTpcDb");
      gSystem->Load("StDetectorDbMaker");
     }

    // Special libraries for EventDisplay
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
    if (fileList && fileList[0]) {
      char line[999]; strcpy(line,fileList[0]);
      if (*line=='@') {
         TString command("grep '.root' "); command += line+1;
         FILE *pipe = gSystem->OpenPipe(command.Data(),"r");
         if (pipe) {fgets(line,999,pipe);line[strlen(line)-1] = 0;}
	 fclose(pipe);
      }
      mainBranch = line;
//    printf("fileList[0] %s %s\n",line,mainBranch.Data());
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
     IOMk->SetBranch("evtselBranch",0,"r");
     //     IOMk->SetBranch("dstBranch",0,"r");
//     IOMk->SetBranch("runcoBranch",0,"r");
     IOMk->SetDebug(1);
//for test only     IOMk->SetMaxEvent(2);

//		DB ON
    if (tflag.Contains("dbon")) {
      dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    }

    //
    // Maker to read events from file or database into StEvent
    //
    if (mainBranch.Contains("dstBranch")) {
      gSystem->Load("StEventMaker");
      StEventMaker *readerMaker =  new StEventMaker("events","title");
    }
    //
    //  Sample analysis maker
    //
    StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
// My Makers  1
  StEEmcDbMaker  *myMk1=new StEEmcDbMaker("eemcDBio");
  // myMk1->setDBname("TestScheme/emc");
  myMk1->setSectors(1,12);
  myMk1->setTimeStampDay(20030214);  // format: yyyymmdd


  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");

  // My Makers  2 
  St2eemcFeeRawMaker * myMk3=new St2eemcFeeRawMaker("St2feeTTree");
//  myMk3->setDb(myMk1);
  char *fileT="myOut.eeTree.root"; // output TTree

  // Output TTree
  TFile f(fileT,"RECREATE");
  TTree t("fee","A tree with FEE events"); // define branch
  myMk3->setOutTTree(&t);

/////////////////////////////////////////////////////////////////////
//  IT IS THE PLACE TO ADD USER MAKERS
//  LIKE:
//  gSystem->Load("StUserMaker");
//  StUserMaker *UserMk = new StUserMaker("UserName");
//  UserMk->SetSome(2002);
/////////////////////////////////////////////////////////////////////
    // WriteOut StEvent
    Int_t wrStEOut = tflag.Contains("evout");
    if (wrStEOut) {
      cout << "!!!! doEvents: will write out .event.root file !!" << endl << endl;
      StIOMaker *outMk = new StIOMaker("EvOut","w","test.event.root","bfcTree");
//        outMk->SetBranch("eventBranch","test.event.root","w");
        outMk->IntoBranch("evtselBranch","StEvent");
      IOMk->SetNotify("CloseFile",outMk);
      IOMk->SetNotify("OpenFile" ,outMk);
    }


//   		 StEventDisplayMaker
    if (eventDisplay) {

       StEventDisplayMaker *displayMk = new StEventDisplayMaker();

       // Set the default to display all StEvent tracks
       displayMk->AddName("StEvent(Primary Tracks)");
       displayMk->AddName("StEvent(Kink Tracks)");
       displayMk->AddName("StEvent(V0 Tracks)");
       displayMk->AddName("StEvent(Xi Tracks)");
       //      displayMk->AddName("StEvent(All Tracks)");
       // Set the default StEvent events filter
       displayMk->AddFilter(new StFilterDef("MainFilter"));
       displayMk->AddFilter(new StMuDstFilterHelper("MuL3Filter",kFALSE));
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

    int eventCounter=0;
    int stat=0;
    //---------------------------------------------------
    while ( stat==0 ) {// loop over events
      if(eventCounter>=nEvents) break;
      chain->Clear();
      stat = chain->Make();
      
      printf(" event# %d\n", eventCounter++);
      
  }
    //    istat = chain->EventLoop(1,nEvents);    
    //JB   t.Print();
    
    f.Write();
    printf("\n\n============== TTree closed =========\n\n");


}

//--------------------------------------------------------------------------

void doEvents(const Int_t startEvent, const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    printf("*file = %s\n",file);
    const char *fileListQQ[]={0,0};
    if (strncmp(file,"GC",2)==0) {
      fileListQQ=0;
    } else {
	fileListQQ[0]=file;
    }
    doEvents(startEvent,nEvents,fileListQQ,qaflag);
}
//--------------------------------------------------------------------------
void doEvents(const Int_t nEvents, const Char_t *file, const Char_t *qaflag)
{
    doEvents(1,nEvents,file,qaflag);
}

//--------------------------------------------------------------------------
void doEvents(const Int_t nEvents, const Char_t *path,const Char_t *file, const Char_t *qaflag, int flag)
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


    doEvents(1,nEvents,F.Data(),opt.Data());
}

//--------------------------------------------------------------------------
void doEvents(Int_t nEvents, const Char_t **fileList, const Char_t *qaflag)
{ doEvents(1,nEvents,fileList,qaflag); }

