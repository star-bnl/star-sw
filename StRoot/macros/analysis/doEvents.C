///////////////////////////////////////////////////////////////////////////////
//
// $Id: doEvents.C,v 1.76 2002/04/23 17:30:46 perev Exp $
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
              const Char_t *file="/afs/rhic/star/data/samples/example.event.root",
              const Char_t *qaflag = 0); 

void doEvents(Int_t startEvent,Int_t nEvents, 
              const Char_t *file="/afs/rhic/star/data/samples/example.event.root",
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
    gSystem->Load("StAnalysisMaker");
    
//		DB ON
    if (tflag.Contains("dbon")) {

      gSystem->Load("StDbLib.so");
      gSystem->Load("StDbBroker.so");
      gSystem->Load("libStDb_Tables.so");
      gSystem->Load("St_db_Maker.so");
      gSystem->Load("StTpcDb");
     }

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
	    "-c","/afs/rhic/star/incoming/GCA/daq/stacs.rc"  // pointer to GC servers for daq
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
//     IOMk->SetBranch("dstBranch",0,"r");
//     IOMk->SetBranch("runcoBranch",0,"r");
     IOMk->SetDebug();
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

///////////////////////////////////////////////////////////////////////////////
//
// $Log: doEvents.C,v $
// Revision 1.76  2002/04/23 17:30:46  perev
// DB loaded only with dbon flag
//
// Revision 1.75  2002/04/14 22:27:29  perev
// remove reading dst by default
//
// Revision 1.74  2002/02/23 19:25:55  perev
// NotifyMe used
//
// Revision 1.73  2002/01/15 18:28:53  perev
// @file logic added
//
// Revision 1.72  2001/12/22 03:47:27  perev
// StTpcDb.so is not loaded for event.root case
//
// Revision 1.71  2001/09/27 00:51:34  perev
// call StEventMaker conditionally
//
// Revision 1.69  2001/09/17 00:13:14  perev
// Load StEventUtilities
//
// Revision 1.67  2001/09/07 18:32:28  perev
// help restored
//
// Revision 1.66  2001/09/01 19:56:41  perev
// EventDisplay option added
//
// Revision 1.65  2001/05/19 00:32:53  perev
// Skip added
//
// Revision 1.64  2001/05/04 20:17:31  perev
// remove St_dst_bfc_status::iterator
//
// Revision 1.63  2001/02/27 23:19:38  perev
// test for filelist[0]!=0 added
//
// Revision 1.62  2001/02/21 23:16:19  perev
// clean up
//
// Revision 1.61  2001/02/14 23:39:17  perev
// classs TTable::iterator example introdiced
//
// Revision 1.60  2000/09/06 22:42:41  ullrich
// Moved WriteOut StEvent block after the line where the analysis
// maker gets created.
//
// Revision 1.59  2000/07/21 01:38:46  perev
// GC query changed
//
// Revision 1.58  2000/07/16 23:04:27  perev
// Remove redundunt Finish() call
//
// Revision 1.57  2000/07/03 02:08:00  perev
// StEvent: vector<TObject*>
//
// Revision 1.56  2000/06/20 14:11:46  kathy
// now unpack BfcStatus table in doEvents so people can see if maker errors exist
//
// Revision 1.55  2000/06/19 23:33:29  perev
// GC for real data
//
// Revision 1.54  2000/05/18 17:44:54  kathy
// turn off by default the writing of output *.event.root file - had it ON by default by accident
//
// Revision 1.53  2000/05/17 16:53:50  kathy
// change flag to write out .event.root file to false by default
//
// Revision 1.52  2000/05/17 16:50:47  kathy
// put Victor's code to write out .event.root file from doEvents.C in here with a flag to turn off and on - now we can get rid of doEventsOut.C ... don't have to keep up 2 sets of macros
//
// Revision 1.51  2000/05/17 15:58:08  kathy
// added some print statements to beginning
//
// Revision 1.50  2000/05/09 19:38:17  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.49  2000/04/21 13:40:08  wenaus
// correct the doc for nEvents in multifile mode
//
// Revision 1.48  2000/04/18 21:43:12  fine
// make TurnDisplay macro available for doEvents
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
//////////////////////////////////////////////////////////////////////////






