// $Id: StTpcEval.C,v 1.2 2000/05/25 20:59:19 snelling Exp $
// $Log: StTpcEval.C,v $
// Revision 1.2  2000/05/25 20:59:19  snelling
// Added switches to macro to enable different modes of running
//
// Revision 1.1.1.1  2000/05/23 00:24:06  snelling
// Milton's and Manuel's version
//
//////////////////////////////////////////////////////////////////////
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file, 
//               runs a chain of 4 makers: 
//                 StEventReaderMaker, StMcEventMaker,StAssociationMaker,
//                 StTpcEvalMaker
//
// note: for more info on StMcEvent and StAssociationMaker, do a 
//      cvs checkout and say "make" in the doc/tex directory - you'll
//      get a ps file with user guide and reference manual.
//////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void StTpcEval(Int_t nevents=1,
const char *MainFile="/afs/rhic/star/data/samples/*.geant.root")
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    //    gSystem->Load("libglobal_Tables");
    //    gSystem->Load("libsim_Tables");
    //    gSystem->Load("libgen_Tables");

    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");

    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");

    gSystem->Load("StIOMaker");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker"); 
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StTpcEvalMaker");
    
    chain = new StChain("StChain"); 
    chain->SetDebug();
   
    // MySQL DB
    const char *mainDB = "MySQL:Geometry_tpc";
    St_db_Maker *dbMk = new St_db_Maker("Geometry",mainDB);
    //    dbMk->SetDebug();
  
    const char *calibDB = "MySQL:Calibrations_tpc";
    St_db_Maker *calibMk = new St_db_Maker("Calibrations",calibDB);
    //    calibMk->SetDebug();
  
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  
    // Now we add Makers to the chain...

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");           //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
    ioMaker->SetBranch("dstBranch",0,"r");   //activate Event Branch
    ioMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch

    // Note, the title "events" is used in the Association Maker, so don't change it.
    StEventMaker*       eventReader   = new StEventMaker("events");
    StMcEventMaker*     mcEventReader = new StMcEventMaker; 
    StAssociationMaker* associator    = new StAssociationMaker;
    StTpcEvalMaker*     mTpcEval      = new StTpcEvalMaker;

    // Define the swithes for the TpcEvalMaker
    mTpcEval->DoHitIteration(kTRUE);
    mTpcEval->DoHitSeparation(kFALSE);
    //    mTpcEval->DoHitSeparation(kTRUE);

    // Define the cuts for the Associations
    StMcParameterDB* parameterDB = StMcParameterDB::instance();  
    // TPC
    parameterDB->setXCutTpc(.5); // 5 mm
    parameterDB->setYCutTpc(.5); // 5 mm
    parameterDB->setZCutTpc(1.5); // 5 mm
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
    chain->Init(); 
    chain->ls(9);
    int istat=0,iev=1;
    EventLoop: if (iev<=nevents && !istat) {
	chain->Clear();
	cout << "---------------------- Processing Event : " << iev << endl;
	istat = chain->Make(iev); 
	if (istat) {
	    cout << "Last Event Processed. Status = " << istat << endl;
	}
	iev++; goto EventLoop;
    } // Event Loop


    
    chain->Finish(); // This should call the Finish() method in ALL makers,
                     // comment it out if you want to keep the objects
                     // available at the command line after running
                     // the macro.


}

