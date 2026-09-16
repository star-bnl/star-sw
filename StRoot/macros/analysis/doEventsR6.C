// 
//   macro: doEventsR6.C (ROOT 6 Version)
// 

#include <iostream>
#include <cstring>
#include <cstdio>
#include "TString.h"
#include "TObjArray.h"
#include "TSystem.h"
#include "TROOT.h"
#include "TClass.h"
#include "TClassStreamer.h"
#include "TBuffer.h"
using namespace std;

#pragma cling load("libPhysics")
#pragma cling load("libTable")
#pragma cling load("libGeom")
#pragma cling load("libSt_base")
#pragma cling load("libStChain")
#pragma cling load("libSt_Tables")
#pragma cling load("libStUtilities")
#pragma cling load("libStTreeMaker")
#pragma cling load("libStIOMaker")
#pragma cling load("libStarClassLibrary")
#pragma cling load("libStTriggerDataMaker")
#pragma cling load("libStBichsel")
#pragma cling load("libStEvent")
#pragma cling load("libStTpcDb")
#pragma cling load("libStEventUtilities")
#pragma cling load("libStEmcUtil")
#pragma cling load("libStTofUtil")
#pragma cling load("libStPmdUtil")
#pragma cling load("libStPreEclMaker")
#pragma cling load("libStStrangeMuDstMaker")
#pragma cling load("libStMuDSTMaker")  
#pragma cling load("libStarMagField")
#pragma cling load("libStMagF")
#pragma cling load("libStAnalysisMaker")
#pragma cling load("libStMuAnalysisMaker")

// DB dependencies
#pragma cling load("libStDbLib")
#pragma cling load("libStDbBroker")
#pragma cling load("libStDb_Tables")
#pragma cling load("libSt_db_Maker")
#pragma cling load("libStDetectorDbMaker")

// EventDisplay dependencies
// #pragma cling load("libSt_g2t")
// #pragma cling load("libgeometry")
// #pragma cling load("libSt_geant_Maker")
// #pragma cling load("libStTableUtilities")
// #pragma cling load("libStEventDisplayMaker") // not compiled for now 
// #pragma cling load("libStCustomFilter")

// Grid Collector dependencies
#pragma cling load("libStGridCollector")

// needs db+stevent
#pragma cling load("libStEventMaker")

class StChain;
StChain  *chain=0;
class St_db_Maker;
St_db_Maker *dbMk =0;
class StFileI;
StFileI *setFiles =0;
TString mainBranch;

Int_t iEvt=0,istat=0;

// HACK: when root5 reads a corrupted class, it throws a ByteCount error
// and fills the missing data with dummy values.  This is a workaround to 
// quarantine the corrupted classes and avoid the segfault.  The classes 
// are still not usable, but at least the program will not crash.
class QuarantineStreamer : public TClassStreamer {
public:
  void operator()(TBuffer &b, void *obj) override {
    if (b.IsReading()) {
      UInt_t rs, rc;
      b.ReadVersion(&rs, &rc, nullptr);
      // move the buffer pointer to the end 
      // of this corrupted object, bypassing the segfault
      b.SetBufferOffset(rs + rc + sizeof(UInt_t));
    }
  }
};
void applyQuarantine() {
  cout << ">>> Applying ROOT 6 Streamer Quarantine to corrupted classes <<<" << endl;
  TClass *c1 = TClass::GetClass("StTriggerData2019");
  if (c1) c1->AdoptStreamer(new QuarantineStreamer());
  TClass *c2 = TClass::GetClass("StEventClusteringHints");
  if (c2) c2->AdoptStreamer(new QuarantineStreamer());
  TClass *c3 = TClass::GetClass("StTriggerData2022");
  if (c3) c3->AdoptStreamer(new QuarantineStreamer());
  // ... add more classes here as needed
  cout << ">>> Quarantine applied <<<" << endl;
}
//____________________________________________________________________________
void doEventsR6()
{
  cout << "Usage: doEventsR6.C(2)  // work with default event.root file" << endl;
  cout << "       doEventsR6.C(startEvent, nEvents,\"path/somefile.event.root\")" << endl;
  cout << "       doEventsR6.C(nEvents, \"path/*.event.root\")" << endl;
  cout << "       doEventsR6.C(nEvents, \"path/file.dst.root\", \"evout\") //Write out StEvent" << endl;	
  cout << "       doEventsR6.C(nEvents, \"path/file.dst.root\", \"display\") //EventDispay" << endl;	
  cout << "       doEventsR6.C(nEvents, \"path/file.dst.root\", \"dbon\") //DB on" << endl;	
  cout << "       doEventsR6.C(nEvents, \"@file.lis\") //list of files in file.lis " << endl;	
  cout << "       doEventsR6.C(nEvents, \"SELECT MuDST WHERE production=P04ih and zdc1Energy>50\", \"gc\") //GridCollector selects MuDST.root files " << endl;
  cout << "       doEventsR6.C(nEvents, \"SELECT event WHERE production=P04ih and zdc1Energy>50\", \"gc\") //GridCollector selects event.root files" << endl;
  cout << "       doEventsR6.C(nEvents, \"@GridCollector_commands.txt\", \"gc,evout\") //GridCollector commands in file" << endl;	
}

//______________________________________________________________________________
// ProtoTypes
void doEventsR6(Int_t nEvents, const char ** fileList, const char *qaflag =0);
void doEventsR6(Int_t startEvent, Int_t nEvents, const char ** fileList, const char *qaflag =0);

void doEventsR6(Int_t nEvents, 
              const char *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const char *qaflag = 0); 

void doEventsR6(Int_t startEvent,Int_t nEvents, 
              const char *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const char *qaflag = 0);

void doEventsR6(Int_t nEvents, 
              const char *path,
              const char *file,
              const char *qaflag, int flag);
              
void loadLibs(const char *opt);              
int  gcInit  (const char *request); 
              
//______________________________________________________________________________
void doEventsR6(Int_t startEvent, Int_t nEventsQQ, const char **fileList, const char *qaflag)
{
  if (!qaflag) qaflag = "";
  int nEvents = nEventsQQ;
  int eventNumber2Display = 0;
  TString tflag = qaflag; tflag.ToLower();
  int eventDisplay = tflag.Contains("disp");
  
  if (eventDisplay) {
     if (gROOT->IsBatch() )
     {
        cout <<  endl << endl <<" ** Warning  ** You have started the EventDisplay version in a batch mode" << endl;
        return;
     }
     TObjArray *tokens = tflag.Tokenize(":");
     if ( tokens->GetEntries() >= 2 ) {
        // May be event id
        TString eventid = (tokens->At(tokens->GetEntries()-1))->GetName();
        if ( eventid.IsDigit() ) 
           eventNumber2Display = eventid.Atoi();
     } 
     delete tokens;
  }
  
  cout <<  endl << endl <<" doEventsR6 -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
    cout << " doEventsR6 -  input fileList = " << fileList[ilist] << endl;
    ilist++; 
  }
  cout << " doEventsR6 -  input qaflag   = " << qaflag << endl;
 
  // loadLibs is largely redundant in ROOT 6 due to pragmas, but kept for structural backwards compatibility
  loadLibs("");

  // DB ON
  if (tflag.Contains("dbon")) {
    loadLibs("dbon");
  }

  // Special libraries for EventDisplay
  if (eventDisplay) {
    loadLibs("disp");
  }

  applyQuarantine();


  // Four levels of debug (0, 1, 2, 3) for MuDst
  StMuDebug::setLevel(0);

  chain  = new StChain("StChain");
  setFiles =0;

  if (tflag.Contains("gc")) {	// GridCollector
    int nev = gcInit(fileList[0]); 
    if (nev<=0) return;
    if (nEvents <= 0) nEvents = nev;
  } else {		// Normal case -- user has specified a list of files
    setFiles = new StFile(fileList);
    char line[999]; strcpy(line,fileList[0]);
    if (*line=='@') {
      TString command("grep '.root' "); command += line+1;
      FILE *pipe = gSystem->OpenPipe(command.Data(),"r");
      if (pipe) {fgets(line,999,pipe);line[strlen(line)-1] = 0;}
      fclose(pipe);
    }
    mainBranch = line;
    mainBranch.ReplaceAll(".root","");
    int idot = strrchr((char*)mainBranch.Data(),'.') - mainBranch.Data();
    mainBranch.Replace(0,idot+1,"");
    mainBranch += "Branch";
  }
 
  // Geant maker for EventDisplay
  if (eventDisplay) {
    // int NwGeant=5000000, IwType=0, NwPaw=0;
    // St_geant_Maker *geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
    // geantMk->LoadGeometry("detp geometry y2004");
    // geantMk->SetActive(kFALSE);
  }

  if (!mainBranch.IsNull()) {
    printf("*** mainBranch=%s ***\n",mainBranch.Data());
  }

  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");	//deactivate all branches
  if(!mainBranch.IsNull())	IOMk->SetBranch(mainBranch,0,"r");  
  IOMk->SetBranch("evtselBranch",0,"r");
  IOMk->SetDebug(1);

  // DB ON
  if (tflag.Contains("dbon")) {
    dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
  }

  // Maker to read events from file or database into StEvent
  if (!mainBranch.Contains("mudstBranch") && mainBranch.Contains("dstBranch")) {
    new StMagFMaker;
    StEventMaker *readerMaker =  new StEventMaker("events","title");
  }
  
  // Sample analysis maker
  StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");

  // WriteOut StEvent
  Int_t wrStEOut = tflag.Contains("evout");
  if (wrStEOut) {
    cout << "!!!! doEventsR6: will write out .event.root file !!" << endl << endl;
    StIOMaker *outMk = new StIOMaker("EvOut","w","test.event.root","bfcTree");
    outMk->IntoBranch("evtselBranch","StEvent");
    IOMk->SetNotify("CloseFile",outMk);
    IOMk->SetNotify("OpenFile" ,outMk);
  }

  // StEventDisplayMaker
  if (eventDisplay) {
    // StEventDisplayMaker *displayMk = new StEventDisplayMaker();
    // displayMk->SetEventIdToRender(eventNumber2Display);
    // if (eventNumber2Display) 
    //    printf("\n\n\n Display the Event %d only\n", eventNumber2Display);
    // displayMk->AddName("StEvent(Primary Tracks)");
    // displayMk->AddFilter(new StFilterDef("MainFilter"));
    // displayMk->AddFilter(new StMuDstFilterHelper("MuL3Filter",kFALSE));
    // displayMk->AddFilter(new StColorFilterHelper("Color schema",kFALSE));
    // displayMk->AddFilter(new StCustomFilter("Custom filter",kFALSE));
  }

  // Initialize chain
  cout << "----------------------------------------------------------" << endl;
  cout << " doEventsR6 - Initializing and Printing chain information   " << endl;
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal("iInit","on init");
  chain->PrintInfo();
  cout << "----------------------------------------------------------" << endl << endl;

  // go to event startEvent
  if (startEvent > 1) IOMk->Skip(startEvent-1);

  // Event loop
  istat=0,iEvt=1;
  istat = chain->EventLoop(1,nEvents);    
}

//____________________________________________________________________________
void doEventsR6(Int_t startEvent, Int_t nEvents, const char *file, const char *qaflag)
{
    if (!qaflag) qaflag="";
    printf("*file = %s\n",file);
    const char *fileListQQ[]={0,0};
    fileListQQ[0]=file;
    cout << "Calling (startEvent,nEvents,fileListQQ,qaflag)" << endl;
    doEventsR6(startEvent,nEvents,fileListQQ,qaflag);
}

//____________________________________________________________________________
void doEventsR6(Int_t nEvents, const char *file, const char *qaflag)
{
  if (!qaflag) qaflag="";
  cout << "Calling (1,nEvents,file,qaflag)" << endl;
  doEventsR6(1,nEvents,file,qaflag);
}

//____________________________________________________________________________
void doEventsR6(Int_t nEvents, const char *path,const char *file, const char *qaflag, int flag)
{
  if (!qaflag) qaflag="";
  TString F;
  if (path && path[0] && path[0]!='-') F = path;
  if (file && file[0] && file[0]!='-') {
    if (!F.IsNull()) F +="/"; 
    F += file;
  }
  TString opt = qaflag;
  if (flag) opt += " evout";

  cout << "Calling (1,nEvents,F.Data(),opt.Data())" << endl;
  doEventsR6(1,nEvents,F.Data(),opt.Data());
}

//____________________________________________________________________________
void doEventsR6(Int_t nEvents, const char **fileList, const char *qaflag)
{ 
  cout << "Calling (1,nEvents,fileList,qaflag)" << endl;
  doEventsR6(1,nEvents,fileList,qaflag);
}

//____________________________________________________________________________
void loadLibs(const char *opt) 
{}

//____________________________________________________________________________
// read GC command file as one single string,
// return number of bytes in the command string, cmds.
int gcReadCommands(const char *file, TString& cmds)
{   
  if (*file != 0) { // must have a valid file name
    FILE *inp = 0;
    inp = fopen(file, "r");
    if (!inp) { // File not found
      printf("doEventsR6: ERROR.  File Not Found // %s\n",file); // ROOT 6 fix: `req+1` was undefined here, fixed to `file`
      return -1;
    }

    char line[500], *comm, *fst;
    while(fgets(line, 500, inp)) {
      for (int i=0; line[i]; i++) // change new line to space
	if (line[i]=='\t' || line[i]=='\n') line[i]=' ';
      fst = line + strspn(line," \t");
      if (fst[0]            == 0 ) continue; // blank line
      if (fst[0]            =='#') continue; // # comment
      if (strncmp(fst,"//",2)==0 ) continue; // // comment
      comm = strstr(line," //"); if (comm) comm[0]=0; // // comment
      cmds += fst;
    }
    fclose(inp);
  }
  return 0;
}

//____________________________________________________________________________
int gcInit(const char *request) 
{
  Int_t ierr = 0;
  gSystem->Load("StGridCollector");
  StGridCollector *req = StGridCollector::Create();
  req->SetDebug(1);

  if (request == 0 || *request == 0) {
    const char *argv[] = {
      "-v", "5",
      "-c", "/afs/rhic.bnl.gov/star/incoming/GCA/gca.rc", // ROOT 6 Note: Added missing comma
      "-s", "MuDST",
      "-w", "production=P04ih and zdc1Energy>50"
    };
    const Int_t argc = sizeof(argv)/sizeof(char*); // ROOT 6 Fix: Division by 4 on 64bit pointer sizes would fail! Use sizeof(char*)
    ierr = req->Init(argc, argv);
    if (0 != ierr) {
      std::cout << "doEventsR6.C can not initialize the Grid Collector "
		<< "with argument \"";
      std::cout << *argv;
      for (Int_t i = 1; i < argc; ++ i)
	std::cout << " " << argv[i];
      std::cout << "\"\nError code is " << ierr
		<< std::endl;
    }
  }
  else if (*request == '@') { // read the command file
    TString cmds;
    ierr = gcReadCommands(request+1, cmds);
    if (!ierr) ierr = req->Init(cmds.Data());
    if (0 != ierr) {
      std::cout << "doEventsR6.C can not initialize the Grid Collector "
		<< "with argument \"" << cmds.Data()
		<< "\"\nError code is " << ierr
		<< std::endl;
    }
  }
  else { // use the input value directly
    ierr = req->Init(request);
    if (0 != ierr) {
      std::cout << "doEventsR6.C can not initialize the Grid Collector "
		<< "with argument \"" << request
		<< "\"\nError code is " << ierr
		<< std::endl;
    }
  }

  if (0 != ierr) { // initialization failure, message printed already
    ierr = 0;
  }
  else {
    int nEvents =  req->GetNEvents();
    std::cout << "INFO: actual number of events " << nEvents << std::endl;
    setFiles = (StFileI*)req;
    mainBranch = req->GetCompName();
    mainBranch += "Branch";
    ierr = nEvents;
  }
  return ierr;
}