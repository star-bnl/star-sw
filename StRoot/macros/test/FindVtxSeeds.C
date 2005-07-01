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
  gSystem->Load("StPass0CalibMaker");
}

void FindVtxSeeds(const Int_t   mode=0,
           const Int_t   nevents=10,
           const Char_t  *path="/star/data13/reco/dev/2001/10/",
           const Char_t  *file="st_physics_2304060_raw_0303.event.root",
           const Char_t* outDir="./"){

  load();

  // Create a chain
  StChain chain("myChain");


  // Set up VertexSeedMaker
  StVertexSeedMaker* vtxSeedMk=0;
  TString fstr = file;
  if (fstr.EndsWith("event.root")) {
    TString fullname = path;
    if (fullname.Length() > 0 && !(fullname.EndsWith("/"))) fullname.Append("/");
    fullname.Append(file);

    StIOMaker* IOMaker = new StIOMaker("IO","r",fullname.Data(),"bfcTree");
    IOMaker->SetBranch("*",0,"0");           //deactivate all branches
    IOMaker->SetBranch("runcoBranch",0,"r"); //activate runco Branch
    IOMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
    new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    vtxSeedMk == (StVertexSeedMaker*) (new StEvtVtxSeedMaker());
  } elseif (fstr.EndsWith("MuDst.root")) {
    new StMuDstMaker(0,0,path,file,"MuDst.root");
    new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
    vtxSeedMk == (StVertexSeedMaker*) (new StMuDstVtxSeedMaker());
  } else {
    cout << "Unknown file type. Stopping." << endl;
    return 0;
  }
  vtxSeedMk->SetDefDir(outDir);

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.Fatal(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<nevents; i++ ) {
    int status = chain.Make();
    if (status!=0) break;
    chain.Clear();
  }

  // Finish
  chain.Finish();

}

// $Id: FindVtxSeeds.C,v 1.1 2005/07/01 23:57:40 genevb Exp $
// $Log: FindVtxSeeds.C,v $
// Revision 1.1  2005/07/01 23:57:40  genevb
// Allow use of StEvent/MuDst in finding vertex seed
//
//
