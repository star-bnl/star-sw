// $Id: StAssociator.C,v 1.10 1999/11/03 22:39:35 calderon Exp $
// $Log: StAssociator.C,v $
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
const char *MainFile="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/set0373_12_35evts.geant.root")

// ~/TestFiles/merged.geant.root
// /disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/set0373_12_35evts.geant.root
// /disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0043_04_56evts.geant.root
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    //gSystem->Load("StEventReaderMaker"); // For use in SL99e
    gSystem->Load("StEventMaker"); // For use in SL99f (along with at least 5 other changes)
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

    // Note, the title "events" is used in the Association Maker, so don't change it.
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    
    StMcEventMaker*     mcEventReader = new StMcEventMaker; // Make an instance...
    StAssociationMaker* associator    = new StAssociationMaker;
    StMcAnalysisMaker*  examples      = new StMcAnalysisMaker;

    // Define the cuts for the Associations

    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    parameterDB->setXCut(.5); // 5 mm
    parameterDB->setZCut(.2); // 2 mm
    parameterDB->setReqCommonHits(3); // Require 3 hits in common for tracks to be associated
    
    
    // now execute the chain member functions
  
    chain->Init(); // This should call the Init() method in ALL makers
    chain->PrintInfo();

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
    examples->mTrackNtuple->Draw("(p-prec)/prec:commHits");

    myCanvas->cd(2);
    gPad->SetLogy(0);
    associator->mLocalHitResolution->Draw();

    myCanvas->cd(3);
    gPad->SetLogy(0);
    examples->mMomResolution->Draw();

    myCanvas->cd(4);
    gPad->SetLogy(0);
    examples->coordRec->Draw();

    myCanvas->cd(4);
    gPad->SetLogy(0);
    examples->coordMcPartner->SetMarkerColor(2);
    examples->coordMcPartner->Draw("same");
    
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

