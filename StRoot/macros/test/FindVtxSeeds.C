//////////////////////////////////////////////////////////////////////////
//                                                                      //
// FindVtxSeeds.C macro                                                 //
// Author: G. Van Buren, BNL                                            //
// Description: uses StVertexSeedMaker to perform vertex seed-finding   //
// Usage: Input is either event.root or MuDst.root file,                //
//        Output is root ntuple, and possibly a .C table file           //
//          in StarDb/Calibrations/rhic subdirectoru                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

void load() {
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbBroker.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StdEdxY2Maker");
  gSystem->Load("StPass0CalibMaker");
}

void FindVtxSeeds(
           const Int_t   nevents=10,
           const Char_t  *path="/star/data13/reco/dev/2001/10/",
           const Char_t  *file="st_physics_2304060_raw_0303.event.root",
           const Char_t* outDir="./"){

  load();

  // Create a chain
  StChain chain("myChain");


  // Set up VertexSeedMaker
  StVertexSeedMaker* vtxSeedMk=0;
  TString pstr = path;
  if (pstr.Length() > 0 && !(pstr.EndsWith("/"))) pstr.Append("/");
  TString fstr = file;
  if (fstr.EndsWith("event.root")) {
    pstr += fstr;
    StIOMaker* IOMaker = new StIOMaker("IO","r",pstr.Data(),"bfcTree");
    IOMaker->SetBranch("*",0,"0");           //deactivate all branches
    IOMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch
    IOMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
    new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    vtxSeedMk = (StVertexSeedMaker*) (new StEvtVtxSeedMaker());
  } else if (fstr.EndsWith("MuDst.root")) {
    dstMaker = new StMuDstMaker(0,0,pstr.Data(),file,"MuDst.root");
    dstMaker->SetStatus("*",0);
    dstMaker->SetStatus("MuEvent",1);
    dstMaker->SetStatus("PrimaryVertices",1);
    dstMaker->SetStatus("PrimaryTracks",1);
    dstMaker->SetStatus("BTofHeader",1);
    new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    vtxSeedMk = (StVertexSeedMaker*) (new StMuDstVtxSeedMaker());
  } else if (fstr.EndsWith("daq")) {
    cout << "Please process with a BFC chain. Stopping." << endl;
    return;
  } else {
    cout << "Unknown file type. Stopping." << endl;
    return;
  }
  vtxSeedMk->SetDefDir(outDir);

  // Do init
  int status = chain.Init();
  if (status) { chain.Fatal(status,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<nevents; i++ ) {
    chain.Clear();
    status = chain.Make();
    if (status) break;
  }

  // Finish
  chain.Finish();

}

// $Id: FindVtxSeeds.C,v 1.3 2015/05/23 02:39:21 genevb Exp $
// $Log: FindVtxSeeds.C,v $
// Revision 1.3  2015/05/23 02:39:21  genevb
// Reduce number of MuDst branches to read
//
// Revision 1.2  2015/05/11 21:51:41  genevb
// Removed some bugs, added dEdx maker dependence
//
// Revision 1.1  2005/07/01 23:57:40  genevb
// Allow use of StEvent/MuDst in finding vertex seed
//
//
