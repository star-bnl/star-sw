///////////////////////////////////////////////////////////////////////////////
//
// $Id: doFlowEvents.C,v 1.6 2000/04/13 21:46:34 kathy Exp $
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
// found, and 'nevents' events from each will be analyzed.
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

void doFlowEvents()
{
    cout << "Usage: doFlowEvents.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doFlowEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doFlowEvents.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}
void doFlowEvents(Int_t,const Char_t **,const char *qaflag = "");
void doFlowEvents(Int_t nevents=999, const Char_t *path, const Char_t *file,
              const char *qaflag = "off");


void doFlowEvents(Int_t nevents, const Char_t **fileList, const char *qaflag)
{
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
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StMagF");
    gSystem->Load("StEventMaker");
    gSystem->Load("StFlowMaker");
    gSystem->Load("StFlowTagMaker");
    gSystem->Load("StFlowAnalysisMaker");

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");

    StFile *setFiles= new StFile();
    
    for (int ifil=0; fileList[ifil]; ifil++)
      { setFiles->AddFile(fileList[ifil]);}
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetBranch("runcoBranch",0,"r");
    IOMk->SetDebug();

    //
    // Maker to read events from file or database into StEvent
    //
    StEventMaker *readerMaker =  new StEventMaker("events","title");

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
    StFlowTagMaker* flowTagMaker = new StFlowTagMaker();
    StFlowAnalysisMaker* flowAnalysisMaker = new StFlowAnalysisMaker();
//     StFlowSelection flowSelect;
//     StFlowSelection flowSelect1;
//     flowSelect1->SetNumber(1);
//     //flowSelect->SetCentrality(0);
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

//     flowMaker->NanoFlowEventOff();
//     flowMaker->NanoFlowEventOn();

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

void doFlowEvents(const Int_t nevents, const Char_t *path="/afs/rhic/star/ebye/flow/fixed10/", const Char_t *file="*.xdf")
{
  //path="/afs/rhic/star/ebye/flow/random10/";
    const char *fileListQQ[]={0,0};
    if (path[0]=='-') {
	fileListQQ[0]=file;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    doFlowEvents(nevents,fileListQQ);
}








