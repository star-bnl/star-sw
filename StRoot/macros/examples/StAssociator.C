
class StChain;
StChain *chain=0;

void StAssociator(Int_t nevents=1,
		  const char *MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.geant.root")

    // File for rlnx02 : /disk0/star/test/SL99d/tfs_Solaris/Fri/year_1b/psc0050_01_40evts.geant.root
    //     const Char_t *MainFile="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf")
    // Usual file: /disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.geant.root

{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventReaderMaker"); // For use in new
    //gSystem->Load("StEventMaker"); // For use in dev (along with at least 5 other changes)
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
    ioMaker->SetBranch("geantBranch",0,"r"); //activate EventBranch
    ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch

    // Note, the title "events" is used in the Association Maker, so don't change it.
    //StEventMaker*       eventReader   = new StEventMaker("events","title");
    StEventReaderMaker* eventReader   = new StEventReaderMaker("events","title");
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
    
    for (Int_t iev=0;iev<nevents; iev++) {
	chain->Clear();
	int iret = chain->Make(iev); // This should call the Make() method in ALL makers
	if (iret) break;

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
    examples->coordRec->Draw();

    myCanvas->cd(4);
    gPad->SetLogy(0);
    examples->coordMcPartner->Draw();
    
    chain->Finish(); // This should call the Finish() method in ALL makers
}

