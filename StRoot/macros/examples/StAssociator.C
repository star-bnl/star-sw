// $Id: StAssociator.C,v 1.6 1999/07/29 15:08:36 calderon Exp $
// $Log: StAssociator.C,v $
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
const char *MainFile="/disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0043_04_56evts.geant.root")

// /disk0/star/test/SL99d/tfs_Solaris/Fri/year_1b/psc0050_01_40evts.geant.root
// /disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfsr/psc0029_02_40evts.geant.root
// /disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0043_04_56evts.geant.root
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
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
    //StEventReaderMaker* eventReader   = new StEventReaderMaker("events","title");
    StMcEventMaker*     mcEventReader = new StMcEventMaker; // Make an instance...
    StAssociationMaker* associator    = new StAssociationMaker;
    StMcAnalysisMaker*  examples      = new StMcAnalysisMaker;

    // Define the cuts for the Associations

    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    parameterDB->setXCut(.1); // 1 mm
    parameterDB->setZCut(.2); // 2 mm
    parameterDB->setReqCommonHits(3); // Require 3 hits in common for tracks to be associated
    

    // now execute the chain member functions
  
    chain->Init(); // This should call the Init() method in ALL makers
    chain->PrintInfo();

    int istat=0,iev=1;
    EventLoop: if (iev<=nevents && !istat) {
	chain->Clear();
	istat = chain->Make(iev); // This should call the Make() method in ALL makers
	if (istat) {
	    cout << "Last Event Processed. Status = " << istat << endl;
	}
	iev++; goto EventLoop;
    } // Event Loop

    TCanvas* myCanvas = examples->mAssociationCanvas;
    myCanvas->Divide(2,2);

    myCanvas->cd(1);
    gPad->SetLogy(1);
    associator->mNumberOfPings->Draw();

    myCanvas->cd(2);
    gPad->SetLogy(0);
    examples->mHitResolution->Draw();

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
}

