// $Id: StAssociator.C,v 1.16 2000/04/13 21:46:21 kathy Exp $
// $Log: StAssociator.C,v $
// Revision 1.16  2000/04/13 21:46:21  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.15  2000/04/12 17:39:02  kathy
// change to only load table libraries needed: lib*_Tables instead of all tables: St_Tables
//
// Revision 1.14  2000/01/19 21:00:40  kathy
// update macros to use standard default xdf files in /afs/rhic/star/data/samples
//
// Revision 1.13  2000/01/19 19:38:12  calderon
// activate runco branch
//
// Revision 1.12  2000/01/12 20:23:01  calderon
// Changed default file to the one produced weekly by Lidia in
// /star/rcf/test/dev/tfs_Linux/Tue/year_2a/hc_standard/
//
// Revision 1.11  1999/12/14 18:18:01  calderon
// using new StMcEvent, StEvent & StAssociationMaker
//
// Revision 1.10  1999/11/03 22:39:35  calderon
// Changed default file.  Previous one was removed.
//
// Revision 1.9  1999/10/01 13:59:18  calderon
// Opened up default Local X cut to 5 mm as a result of
// studies on merged tracks.
// Changed default Hit resolution histogram to plot from global z vs global x
// to the more significant global z vs local x which are the variables
// used in the association criterion.
// The global z vs global x still belongs to StMcAnalysisMaker and is built
// using the multimap, the global z vs local x is made by default in
// StAssociationMaker without any requirement of association to serve
// as a diagnostic.
// Changed size of default canvas
//
// Revision 1.8  1999/09/10 19:11:55  calderon
// Write the Ntuple in StMcAnalysisMaker into a file.
// This way it can be accessed after the macro finishes,
// otherwise it gets deleted.
//
// Revision 1.7  1999/09/10 00:02:24  calderon
// Made the following changes:
// -load StUtilities
// -add line to print number of processed events
// -create canvas here
// -no longer draw # of Pings histogram
//
// Revision 1.6  1999/07/29 15:08:36  calderon
// Include Mom. Resolution example (Histograms & Ntuple)
//
// Revision 1.5  1999/07/28 21:29:34  calderon
// Modified event loop: use 'if' and 'goto' to avoid using 'for'
//
// Revision 1.4  1999/07/28 20:27:45  calderon
// Version with SL99f libraries
//
// Revision 1.3  1999/07/23 14:35:41  calderon
// Updated names of default files and of packages
//
// Revision 1.2  1999/07/23 10:53:48  kathy
// put in header info in Manuel's macros
//
//////////////////////////////////////////////////////////////////////
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file, 
//               runs a chain of 4 makers: 
//                 StEventReaderMaker, StMcEventMaker,StAssociationMaker,
//                 StMcAnalysisMaker
//
// note: for more info on StMcEvent and StAssociationMaker, do a 
//      cvs checkout and say "make" in the doc/tex directory - you'll
//      get a ps file with user guide and reference manual.
//////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void StAssociator(Int_t nevents=1,
const char *MainFile="/afs/rhic/star/data/samples/*.geant.root")
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("StUtilities");

    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker"); 
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    
    chain = new StChain("StChain"); 
    chain->SetDebug();
   
    // Now we add Makers to the chain...

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
    ioMaker->SetBranch("dstBranch",0,"r"); //activate Event Branch
    ioMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch

    // Note, the title "events" is used in the Association Maker, so don't change it.
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    
    StMcEventMaker*     mcEventReader = new StMcEventMaker; // Make an instance...
    StAssociationMaker* associator    = new StAssociationMaker;
    StMcAnalysisMaker*  examples      = new StMcAnalysisMaker;

    // Define the cuts for the Associations

    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    // TPC
    parameterDB->setXCutTpc(.5); // 5 mm
    parameterDB->setYCutTpc(.5); // 5 mm
    parameterDB->setZCutTpc(.2); // 2 mm
    parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
    // FTPC
    parameterDB->setRCutFtpc(.3); // 3 mm
    parameterDB->setPhiCutFtpc(5*(3.1415927/180.0)); // 5 degrees
    parameterDB->setReqCommonHitsFtpc(3); // Require 3 hits in common for tracks to be associated
    // SVT
    parameterDB->setXCutSvt(.1); // 1 mm
    parameterDB->setYCutSvt(.1); // 1 mm
    parameterDB->setZCutSvt(.1); // 1 mm
    parameterDB->setReqCommonHitsSvt(1); // Require 1 hits in common for tracks to be associated
    
    
    // now execute the chain member functions

    chain->PrintInfo();
    chain->Init(); // This should call the Init() method in ALL makers

    int istat=0,iev=1;
    EventLoop: if (iev<=nevents && !istat) {
	chain->Clear();
	cout << "---------------------- Processing Event : " << iev << endl;
	istat = chain->Make(iev); // This should call the Make() method in ALL makers
	if (istat) {
	    cout << "Last Event Processed. Status = " << istat << endl;
	}
	iev++; goto EventLoop;
    } // Event Loop
    examples->mAssociationCanvas = new TCanvas("mAssociationCanvas", "Histograms",200,10,600,600);
    TCanvas* myCanvas = examples->mAssociationCanvas;
    myCanvas->Divide(2,2);

    myCanvas->cd(1);
    gPad->SetLogy(0);
    examples->mTrackNtuple->Draw("(p-prec)/prec:commTpcHits");

    TList* dList = chain->GetMaker("McAnalysis")->Histograms();
    TH2F* hitRes = dList->At(0);
    TH2F* momRes = dList->At(1);
    TH2F* coordRc = dList->At(2);
    TH2F* coordMc = dList->At(3);
    
    myCanvas->cd(2);
    gPad->SetLogy(0);
    hitRes->Draw();

    myCanvas->cd(3);
    gPad->SetLogy(0);
    momRes->Draw();

    myCanvas->cd(4);
    gPad->SetLogy(0);
    coordRc->Draw();

    myCanvas->cd(4);
    gPad->SetLogy(0);
    coordMc->SetMarkerColor(2);
    coordMc->Draw("same");
    
    //chain->Finish(); // This should call the Finish() method in ALL makers,
                     // comment it out if you want to keep the objects
                     // available at the command line after running
                     // the macro.

    // To look at the ntuple after the macro has executed:
    // f1 = new TFile("TrackMapNtuple.root");  //This opens the file, and loads the Ntuple
    // TrackNtuple->Draw("px:pxrec")  //Once loaded, the Ntuple is available by name.
    // To look at the Histograms once the Macro has executed:
    // TList* dList = chain->GetMaker("McAnalysis")->Histograms();
    // TH2F* hitRes = dList->At(0);  //or whatever index from 0 to 3
}

