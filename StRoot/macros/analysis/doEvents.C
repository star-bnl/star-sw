/////////////////////////////////////////////////////////////////////////////
// $Id: doEvents.C,v 1.109 2010/03/16 16:23:09 fisyak Exp $
// Description: 
// Chain to read events from files or database into StEvent and analyze.
// what it does: reads .dst.root or .xdf files and then runs StEventMaker
//          to fill StEvent and StAnalysisMaker to show example of analysis
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and the first 'nEvents' events in the file set will be
// analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
// example invocation:
// .x doEvents.C(10,"some_directory/some_dst_file.xdf")
// example ROOT file invocation:
// .x doEvents.C(10,"some_directory/some_dst_file.root")
// example multi-ROOT file invocation:
// .x doEvents.C(9999,"some_directory/*.dst.root")
//
// example using the Grid Collector
// 0) The third argument, qaflag, must contain "gc" in order to access Grid
//    Collector functions
// 1) process first ten events generated, request is embedded in this file
// .x doEvents.C(10, "", "gc")
// 2) specify the request as a string argument (analyze the event branch of
//    selected data from production P02gg).  First argument 0 (or smaller)
//    indicates that all events satisfying the condition will be analyzed.
// .x doEvents.C(0, "select event where Production=P02gg and NV0>2000", "gc")
//
//  The rules for constructing valid conditions are as follows
//  a) simple conditions can be joined together with logical operator "AND",
//     "OR", "XOR" and "!" (for NOT).
//  b) a simple condition is a range such as 'v1 < name' and 'v1 <= name <
//     v2'.  The supported range operators are >, >=, <, <=, == and !=.
//     The name is the name of a leaf of the ROOT tree in tags.root files.
//     In case a leaf contains many values, the name to be used are of the
//     form leaf_name[0], leaf_name[1], and so on.
//  c) only '==' operator is supported for string attributes.  To ensure
//     a string literal is definitely treated as a string literal, not a
//     name, it should be quoted either with "" or with ''.
////////////////////////////////////////////////////////////////////////////
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
//      qaflag = "display"  turn on EventDisplay,
//                          set to off by default.
//      qaflag = "gc"       treat the file argument as a select statement
//                          for Grid Collector.
//      
/////////////////////////////////////////////////////////////////////////////
#include "iostream.h"

class     StChain;
StChain  *chain=0;
class     St_db_Maker;
St_db_Maker *dbMk =0;
class StFileI;
StFileI *setFiles =0;
TString mainBranch;

Int_t iEvt=0,istat=0;
//____________________________________________________________________________
void doEvents()
{
  cout << "Usage: doEvents.C(2)  // work with default event.root file" << endl;
  cout << "       doEvents.C(startEvent, nEvents,\"path/somefile.event.root\")" << endl;
  cout << "       doEvents.C(nEvents, \"path/*.event.root\")" << endl;
  cout << "       doEvents.C(nEvents, \"path/file.dst.root\", \"evout\") //Write out StEvent" << endl;	
  cout << "       doEvents.C(nEvents, \"path/file.dst.root\", \"display\") //EventDispay" << endl;	
  cout << "       doEvents.C(nEvents, \"path/file.dst.root\", \"dbon\") //DB on" << endl;	
  cout << "       doEvents.C(nEvents, \"@file.lis\") //list of files in file.lis " << endl;	
  cout << "       doEvents.C(nEvents, \"SELECT MuDST WHERE production=P04ih and zdc1Energy>50\", \"gc\") //GridCollector selects MuDST.root files " << endl;
  cout << "       doEvents.C(nEvents, \"SELECT event WHERE production=P04ih and zdc1Energy>50\", \"gc\") //GridCollector selects event.root files" << endl;
  cout << "       doEvents.C(nEvents, \"@GridCollector_commands.txt\", \"gc,evout\") //GridCollector commands in file" << endl;	
}


//______________________________________________________________________________
// ProtoTypes
void doEvents(Int_t nEvents, const char ** fileList, const char *qaflag =0);
void doEvents(Int_t startEvent, Int_t nEvents, const char ** fileList, const char *qaflag =0);

void doEvents(Int_t nEvents, 
              const char *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const char *qaflag = 0); 

void doEvents(Int_t startEvent,Int_t nEvents, 
              const char *file="/afs/rhic.bnl.gov/star/data/samples/example.event.root",
              const char *qaflag = 0);

void doEvents(Int_t nEvents, 
              const char *path,
              const char *file,
              const char *qaflag, int flag);
              
void loadLibs(const char *opt);              
int  gcInit  (const char *request); 
              
              
//______________________________________________________________________________
void doEvents(Int_t startEvent, Int_t nEventsQQ, const char **fileList, const char *qaflag)
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
  cout <<  endl << endl <<" doEvents -  input # events = " << nEvents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
    cout << " doEvents -  input fileList = " << fileList[ilist] << endl;
    ilist++; 
  }
  cout << " doEvents -  input qaflag   = " << qaflag << endl;
 
    // First load some shared libraries we need
    // Load all libs           WMZ 7/27/04
  loadLibs("");


  //		DB ON
  if (tflag.Contains("dbon")) {
    loadLibs("dbon");
  }

  // Special libraries for EventDisplay
  if (eventDisplay) {//EventDisplay on
    loadLibs("disp");
  }
  // Four levels of debug (0, 1, 2, 3) for MuDst      WMZ 7/27/04
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
    //    printf("fileList[0] %s %s\n",line,mainBranch.Data());
    mainBranch.ReplaceAll(".root","");
    int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
    mainBranch.Replace(0,idot+1,"");
    mainBranch += "Branch";
  }
 
  //   		Geant maker  for EventDisplay
  if (eventDisplay) {
    int NwGeant=5000000, IwType=0, NwPaw=0;
    St_geant_Maker *geantMk = new St_geant_Maker("geant",NwGeant,NwPaw,IwType);
    geantMk->LoadGeometry("detp geometry y2004");
    geantMk->SetActive(kFALSE);
  }


  if (!mainBranch.IsNull()) {
    printf("*** mainBranch=%s ***\n",mainBranch.Data());
  }

//  For MuDst, the input string fTreeName which is defined in StIOInterface.h 
//  is used to pass a SAVE flag. To save selected  events of MuDst, instance 
//  a new StIOMaker class as follows,                // WMZ 7/27/04
//  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"MuSave");
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

  // Maker to read events from file or database into StEvent
  if (!mainBranch.Contains("mudstBranch") &&
      mainBranch.Contains("dstBranch")) {
    gSystem->Load("libStMagF");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StTpcDb");
    gSystem->Load("StEventMaker");
    new StMagFMaker;
    StEventMaker *readerMaker =  new StEventMaker("events","title");
  }
    //  Sample analysis maker
    StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");
//  Sample analysis maker for MuDst               WMZ 7/27/04
//  StMuAnalysisMaker *analysisMaker = new StMuAnalysisMaker("analysis");


  ///////////////////////////////////////////////////////////////////
  //  IT IS THE PLACE TO ADD USER MAKERS
  //  LIKE:
  //  gSystem->Load("StUserMaker");
  //  StUserMaker *UserMk = new StUserMaker("UserName");
  //  UserMk->SetSome(2002);
  ///////////////////////////////////////////////////////////////////

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
    displayMk->SetEventIdToRender(eventNumber2Display);
    if (eventNumber2Display) 
       printf("\n\n\n Display the Event %d only\n", eventNumber2Display);
    // Set the default to display all StEvent tracks
    displayMk->AddName("StEvent(Primary Tracks)");
    // displayMk->AddName("StEvent(Kink Tracks)");
    // displayMk->AddName("StEvent(V0 Tracks)");
    // displayMk->AddName("StEvent(Xi Tracks)");
    //      displayMk->AddName("StEvent(All Tracks)");
    // Set the default StEvent events filter
    displayMk->AddFilter(new StFilterDef("MainFilter"));
    displayMk->AddFilter(new StMuDstFilterHelper("MuL3Filter",kFALSE));
    displayMk->AddFilter(new StColorFilterHelper("Color schema",kFALSE));
//  Check whether any Custom filter is present  
    if (!gSystem->Load("StCustomFilter")) { 
      displayMk->AddFilter(new StCustomFilter("Custom filter",kFALSE));
    }
    
  }

  // Initialize chain
  cout << "----------------------------------------------------------" << endl;
  cout << " doEvents - Initializing and Printing chain information   " << endl;
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
  cout << "----------------------------------------------------------" << endl << endl;


  // go to event startEvent
  if (startEvent > 1) IOMk->Skip(startEvent-1);

  // Event loop
  istat=0,iEvt=1;
  istat = chain->EventLoop(1,nEvents);    
  //VP  
  // delete setFiles; setFiles=0;
  //  gSystem->Exit(0);
}
//____________________________________________________________________________
void doEvents(Int_t startEvent, Int_t nEvents, const char *file, const char *qaflag)
{
    if (!qaflag) qaflag="";
    printf("*file = %s\n",file);
    const char *fileListQQ[]={0,0};
    fileListQQ[0]=file;
    cout << "Calling (startEvent,nEvents,fileListQQ,qaflag)" << endl;
    doEvents(startEvent,nEvents,fileListQQ,qaflag);
}
//____________________________________________________________________________
void doEvents(Int_t nEvents, const char *file, const char *qaflag)
{
  if (!qaflag) qaflag="";
  cout << "Calling (1,nEvents,file,qaflag)" << endl;
  doEvents(1,nEvents,file,qaflag);
}

//____________________________________________________________________________
void doEvents(Int_t nEvents, const char *path,const char *file, const char *qaflag, int flag)
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
  doEvents(1,nEvents,F.Data(),opt.Data());
}

//____________________________________________________________________________
void doEvents(Int_t nEvents, const char **fileList, const char *qaflag)
{ 
  cout << "Calling (1,nEvents,fileList,qaflag)" << endl;
  doEvents(1,nEvents,fileList,qaflag);
}
//____________________________________________________________________________
void loadLibs(const char *opt) 
{
// Dynamically link needed shared libs

  if (!opt[0]) { //Default set
//	ROOT libs
    gSystem->Load("libPhysics");
    gSystem->Load("libTable");
    gSystem->Load("libGeom");

//	STAR libs  
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");        // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StTofUtil");
    gSystem->Load("StPmdUtil");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");  
    gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("StAnalysisMaker");
    gSystem->Load("StMuAnalysisMaker");
    cout << " loading of shared libraries done" << endl;
    return;
  }
  if (strstr(opt,"dbon")) {// DB stuff
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("St_db_Maker.so");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
  }

  if (strstr(opt,"disp")) {// EventDisplay stuff
    gSystem->Load("St_g2t");  // is a part od St_Tables
    gSystem->Load("geometry");
    gSystem->Load("St_geant_Maker");
    gSystem->Load("StTableUtilities");
    gSystem->Load("StEventDisplayMaker");
  }
}
//____________________________________________________________________________
// read GC command file as one single string,
// return number of bytes in the command string, cmds.
int gcReadCommands(const char *file, TString& cmds)
{   
  if (*file != 0) { // must have a valid file name
    FILE *inp = 0;
    inp = fopen(file, "r");
    if (!inp) { // File not found
      printf("doEvents: ERROR.  File Not Found // %s\n",req+1);
      return -1;
    }

    char line[500], *comm, *fst;
    while(fgets(line, 500, inp)) {
      for (int i=0; line[i]; i++) // change new line to space
	if (line[i]=='\t' || line[i]=='\n') line[i]=' ';
      // strip away comments
      fst = line + strspn(line," \t");
      if (fst[0]            == 0 ) continue; // blank line
      if (fst[0]            =='#') continue; // # comment
      if (strncmp(fst,"//",2)==0 ) continue; // // comment
      //comm = strstr(line,"#"  ); if (comm) comm[0]=0; // # comment
      comm = strstr(line," //"); if (comm) comm[0]=0; // // comment
      cmds += fst;
    }
    fclose(inp);
  }

  return 0;
}
//____________________________________________________________________________
// Initialize global variable setFiles for Grid Collector operations
// also initialize variable mainBranch
int gcInit(const char *request) 
{
  Int_t ierr = 0;
  gSystem->Load("StGridCollector");
  StGridCollector *req = StGridCollector::Create();
  req->SetDebug(1);

  if (request == 0 || *request == 0) {
    // This is an example to show how to initialize Grid Collector in
    // another way.
    const char *argv[] = {
      "-v", "5",
      "-c", "/afs/rhic.bnl.gov/star/incoming/GCA/gca.rc"
      "-s", "MuDST",
      "-w", "production=P04ih and zdc1Energy>50"
    };
    const Int_t argc = sizeof(argv)/4;
    ierr = req->Init(argc, argv);
    if (0 != ierr) {
      std::cout << "doEvents.C can not initialize the Grid Collector "
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
      std::cout << "doEvents.C can not initialize the Grid Collector "
		<< "with argument \"" << cmds.Data()
		<< "\"\nError code is " << ierr
		<< std::endl;
    }
  }
  else { // use the input value directly
    ierr = req->Init(request);
    if (0 != ierr) {
      std::cout << "doEvents.C can not initialize the Grid Collector "
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
    setFiles = req;
    mainBranch = req->GetCompName();
    mainBranch += "Branch";
    ierr = nEvents;
  }
  return ierr;
}
//____________________________________________________________________________
//////////////////////////////////////////////////////////////////////////////
// $Log: doEvents.C,v $
// Revision 1.109  2010/03/16 16:23:09  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 1.108  2009/11/05 21:37:12  fisyak
// Add StMagF for StTpcDb
//
// Revision 1.107  2007/09/01 02:26:12  fine
// introduce Event selection to fix isseu #1051
//
// Revision 1.106  2007/08/28 14:30:55  fine
// Add StTpcDb to load StEventMaker
//
// Revision 1.105  2006/08/15 21:42:43  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.104  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.103  2005/07/14 19:25:59  fine
// Change the default EventDisplay options
//
// Revision 1.102  2004/12/13 21:00:28  fine
// Add a protection against of batch mode for Event Display
//
// Revision 1.101  2004/11/08 17:20:59  fine
// fix the default geometry version from year2001 to y2004
//
// Revision 1.100  2004/08/27 02:23:48  fine
// comment out delete setFile to preserve setFile for interactive user
//
// Revision 1.99  2004/08/27 01:50:12  fine
// restore g2t shared library. Geant complained
//
// Revision 1.98  2004/08/27 00:17:11  fine
// use one extr custom filter is present
//
// Revision 1.97  2004/08/17 15:58:59  perev
// StMuAnalysisMaker loading added
//
// Revision 1.96  2004/08/16 02:27:39  perev
// delete setfiles again
//
// Revision 1.95  2004/08/12 16:43:26  perev
// Cleanup from JohnVu & VP
//
// Revision 1.94  2004/08/12 00:13:45  perev
// JohnWu GC corrections
//
// Revision 1.93  2004/08/10 19:44:19  perev
// Cleanup+StGridCollector
//
// Revision 1.91  2004/07/21 17:31:12  fine
// The default coloring filter has been added to doEvents macro and StEventHeler class
// Revision 1.90  2004/06/23 20:06:02  perev
// const Int_t replaced by Int_t
// Revision 1.89  2004/02/24 16:45:26  fisyak
// Add load of libGeom
// Revision 1.88  2004/02/02 03:02:41  perev
// Defence against qaflag==0
// Revision 1.87  2004/02/01 19:11:26  jeromel
// indent + printing
// Revision 1.86  2003/10/10 19:39:20  perev
// Remeve delete fileset. problem for debuging. A leak is small
// Revision 1.85  2003/09/07 03:49:11  perev
// gcc 3.2 + WarnOff
// Revision 1.84  2003/07/17 15:34:12  perev
// delete setFiles added
// Revision 1.83  2003/05/02 23:12:53  jeromel
// StBichsel
// Revision 1.82  2003/04/26 03:36:25  jeromel
// Forgot to commit
// Revision 1.81  2003/02/27 17:01:06  fine
// the secondary filter in example has been disabled by default
// Revision 1.80  2003/02/26 04:40:29  fine
// add one extra filter to display
// Revision 1.79  2003/01/17 17:14:06  fine
// fix: StEventMaker was not loaded properly
// Revision 1.78  2003/01/17 16:26:13  fine
//  chnage display default parameters
// Revision 1.77  2002/11/26 02:30:29  perev
// EventLoop added
// Revision 1.75  2002/04/14 22:27:29  perev
// remove reading dst by default
// Revision 1.74  2002/02/23 19:25:55  perev
// NotifyMe used
// Revision 1.73  2002/01/15 18:28:53  perev
// @file logic added
// Revision 1.72  2001/12/22 03:47:27  perev
// StTpcDb.so is not loaded for event.root case
// Revision 1.71  2001/09/27 00:51:34  perev
// call StEventMaker conditionally
// Revision 1.69  2001/09/17 00:13:14  perev
// Load StEventUtilities
// Revision 1.67  2001/09/07 18:32:28  perev
// help restored
// Revision 1.66  2001/09/01 19:56:41  perev
// EventDisplay option added
// Revision 1.65  2001/05/19 00:32:53  perev
// Skip added
// Revision 1.64  2001/05/04 20:17:31  perev
// remove St_dst_bfc_status::iterator
// Revision 1.63  2001/02/27 23:19:38  perev
// test for filelist[0]!=0 added
// Revision 1.62  2001/02/21 23:16:19  perev
// clean up
// Revision 1.61  2001/02/14 23:39:17  perev
// classs TTable::iterator example introdiced
// Revision 1.60  2000/09/06 22:42:41  ullrich
// Moved WriteOut StEvent block after the line where the analysis
// maker gets created.
// Revision 1.59  2000/07/21 01:38:46  perev
// GC query changed
// Revision 1.58  2000/07/16 23:04:27  perev
// Remove redundunt Finish() call
// Revision 1.57  2000/07/03 02:08:00  perev
// StEvent: vector<TObject*>
// Revision 1.56  2000/06/20 14:11:46  kathy
// now unpack BfcStatus table in doEvents so people can see if maker errors exist
// Revision 1.55  2000/06/19 23:33:29  perev
// GC for real data
// Revision 1.54  2000/05/18 17:44:54  kathy
// turn off by default the writing of output *.event.root file - had it ON by default by accident
// Revision 1.53  2000/05/17 16:53:50  kathy
// change flag to write out .event.root file to false by default
// Revision 1.52  2000/05/17 16:50:47  kathy
// put Victor's code to write out .event.root file from doEvents.C in here with a flag to turn off and on - now we can get rid of doEventsOut.C ... don't have to keep up 2 sets of macros
// Revision 1.51  2000/05/17 15:58:08  kathy
// added some print statements to beginning
// Revision 1.50  2000/05/09 19:38:17  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
// Revision 1.49  2000/04/21 13:40:08  wenaus
// correct the doc for nEvents in multifile mode
// Revision 1.48  2000/04/18 21:43:12  fine
// make TurnDisplay macro available for doEvents
// Revision 1.47  2000/04/13 22:14:03  perev
// StFile -> StFileI
// Revision 1.46  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
// Revision 1.45  2000/04/12 17:33:45  kathy
// put loading of libtpc_Tables back in since Iwona is going back to original tpt_track table
// Revision 1.44  2000/04/12 15:29:05  kathy
// comment out libtpc by default
// Revision 1.43  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
// Revision 1.42  2000/04/07 15:54:26  perev
// GC added
// Revision 1.41  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
// Revision 1.40  2000/03/17 23:10:06  kathy
// make sure the dst branch is explicitly set in the macros using dst.root files as input - otherwise they don't work properly with soft links
// Revision 1.39  2000/01/11 18:20:20  ullrich
// Add latests improvements from Victor.
// Revision 1.38  2000/01/10 22:06:09  kathy
// add owner name and comments
// Revision 1.37  1999/11/17 14:34:00  ullrich
// Added version with no arguments which prints usage info.
// Revision 1.36  1999/11/17 14:23:40  ullrich
// Updated for new StEvent/StEventMaker.
// owner: Torre Wenaus,Victor Perevoztchikov
// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
////////////////////////////////////////////////////////////////////////
