///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.9 2000/05/11 00:22:28 posk Exp $
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
// found, and 'nevents' events will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doFlowEvents.C(10,"-","some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x doFlowEvents.C(10,"-","some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x doFlowEvents.C(9999,"some_directory","*.dst.root")
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//              Art Poskanzer
//  
///////////////////////////////////////////////////////////////////////////////
//
// $Log: doFlowEvents.C,v $
// Revision 1.9  2000/05/11 00:22:28  posk
// Can read StEvent files which have extention .event.root .
//
// Revision 1.8  2000/05/09 19:38:22  kathy
// update to use standard default input files and only process few events by default - to make it easy to run in automatic macro testing script
//
// Revision 1.7  2000/04/24 20:25:45  posk
// Added doEvents.C updates.
//
// Revision 1.6  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.5  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.4  2000/03/28 23:26:46  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.3  2000/03/15 23:33:54  posk
// Added StFlowSelection.
//
// Revision 1.2  2000/03/07 17:51:23  snelling
// Added switch for Nano DST
//
// Revision 1.1  2000/03/02 23:49:11  posk
// Version of doEvents.C for flow analysis which can set cut parameters.
//
//
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

// Usage: 
// doFlowEvents.C(nevents, -, some_directory/some_dst_file.xdf)
// doFlowEvents.C(nevents, -, some_directory/some_dst_file.root)
// doFlowEvents.C(nevents, some_directory, *.dst.root/*.event.root)	
// doFlowEvents.C(nevents)	
// doFlowEvents.C()	

void doFlowEvents(Int_t, const Char_t **, const char *qaflag = "");
void doFlowEvents(Int_t, const Char_t *, const Char_t *, 
		  const char *qaflag = "off");
void doFlowEvents(Int_t nevents=2);
void doFlowEvents(Int_t);

void doFlowEvents(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
  //
  // First load some shared libraries we need
  // Do it in this order
  //
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");

  gSystem->Load("StFlowMaker");
  //gSystem->Load("StFlowTagMaker");
  //gSystem->Load("StFlowAnalysisMaker");
  
  //
  // Handling depends on whether file is a ROOT file or XDF file
  //
  chain  = new StChain("StChain");
  
  StFile *setFiles= new StFile();
  
  for (int ifil=0; fileList[ifil]; ifil++)
    { setFiles->AddFile(fileList[ifil]); }
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetBranch("runcoBranch",0,"r");
  //IOMk->SetDebug();
  
  cout << "#### First File = " << fileList[0] << endl;
  if (strstr(fileList[0], ".event.root")==0) {
    //
    // Maker to read raw events and make StEvent
    //
    gSystem->Load("StEventMaker");
    StEventMaker *readerMaker =  new StEventMaker("events","title");
  } else {               // read StEvent file
    IOMk->SetBranch("event");
  }
  
  //
  // Flow Makers
  //   Use of the TagMaker is optional.
  //   The AnalysisMaker may be used with a selection object.
  //   If you instantiate more than one AnalysisMaker,
  //      make sure each has a different selection object number
  //      and that you do not instantiate the TagMaker.
  //   If you want to read more than one PhiWeight file, instantiate multiple
  //      FlowMakers with the corresponding selection objects.
  //
  StFlowMaker* flowMaker = new StFlowMaker();
  //StFlowTagMaker* flowTagMaker = new StFlowTagMaker();
  //StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker();
  
  // Make Selection objects and instantiate Makers
//     StFlowSelection flowSelect;
//     StFlowSelection flowSelect1;
//     flowSelect1->SetNumber(1);
//     flowSelect->SetCentrality(0);
//     flowSelect1->SetPid("pi"); // pi+, pi-, pi, or proton
//     flowSelect1->SetPidPart("pi"); // pi+, pi-, pi, or proton
//     char makerName[30];
//     sprintf(makerName, "Flow%s", flowSelect->Number());
//     StFlowMaker* flowMaker = new StFlowMaker(makerName, flowSelect);
//     sprintf(makerName, "FlowAnalysis%s", flowSelect->Number());
//     StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker(makerName, flowSelect);
//     sprintf(makerName, "Flow%s", flowSelect1->Number());
//     StFlowMaker* flowMaker1 = new StFlowMaker(makerName, flowSelect1);
//     sprintf(makerName, "FlowAnalysis%s", flowSelect1->Number());
//     StFlowAnalysisMaker* flowAnalysisMaker1 = new StFlowAnalysisMaker(makerName, flowSelect1);

    // Set read-write flages
//     flowMaker->NanoFlowEventOff();
//     flowMaker->NanoFlowEventOn();
//     flowMaker->FlowEventWrite(kTRUE);
//     flowMaker->FlowEventRead(kTRUE);

    // Set Debug status
//     flowMaker->SetDebug();
//     flowTagMaker->SetDebug();
//     flowAnalysisMaker->SetDebug();

  //
  // Initialize chain
  //
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
  
  //
  // Set the parameters
  //
  // Set the event cuts
  //StFlowCutEvent::SetMult(100, 10000);
  //StFlowCutEvent::SetVertexX(0., 0.);
  //StFlowCutEvent::SetVertexY(0., 0.);
  //StFlowCutEvent::SetVertexZ(-10., 10.);
  //StFlowCutEvent::SetEtaSym(-0.05, 0.05);
  
  // Set the track cuts
  //StFlowCutTrack::SetFitPts(15, 200);
  //StFlowCutTrack::SetFitOverMaxPts(0, 0);
  
  // Set the event plane selections
  //StFlowEvent::SetEtaCut(0.5, 1., 0, 0); // selection 1, harmonic 1
  //StFlowEvent::SetPtCut(0.2, 1., 0, 0);
  
  // Set the PID windows
  //StFlowEvent::SetPiPlusCut(-2., 2.);
  //StFlowEvent::SetPiMinusCut(-2., 2.);
  //StFlowEvent::SetProtonCut(-2., 2.);
  
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

void doFlowEvents(const Int_t nevents, const Char_t *path, const Char_t *file, const char *qaflag)
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  doFlowEvents(nevents,fileListQQ,qaflag);
}

void doFlowEvents(const Int_t nevents)
{
  
  // Commit to cvs with these defaults:
  const Char_t *filePath="-";
  const Char_t *fileExt="/afs/rhic/star/data/samples/gstar.dst.root";

  //Char_t* filePath="/star/rcf/data03/reco/auau200/mevsim/vanilla/flow/year_1h/hadronic_on/tfs_6/";
  //Char_t* fileExt="*.dst.root";

  //Char_t* filePath="/afs/rhic/star/ebye/flow/";
  //Char_t* fileExt="test2.event.root";

  //Char_t* filePath="/afs/rhic/star/ebye/flow/fixed10/";
  //Char_t* filePath="/afs/rhic/star/ebye/flow/random10/";
  //Char_t* fileExt="*.xdf";
  
  //Char_t* filePath="/data06/snelling/flow/";
  //Char_t* fileExt="*.dst.root";
  
  //Char_t* filePath="./";
  //Char_t* fileExt="*.event.root";
  
  doFlowEvents(nevents, filePath, fileExt);
}






