// $Id: makeStrangeMuDst.C,v 2.1 2000/06/09 22:15:10 genevb Exp $
// $Log: makeStrangeMuDst.C,v $
// Revision 2.1  2000/06/09 22:15:10  genevb
// Include code for fixing track topology maps, if necessary
//
// Revision 2.0  2000/06/02 22:28:58  genevb
// Updated for version 2 of Strangeness mico DST package
//
// Revision 1.5  2000/05/18 19:50:48  genevb
// Better handling of status returned by chain.Make()
//
// Revision 1.4  2000/04/13 21:46:35  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.3  2000/04/12 16:16:55  genevb
// Remove unnecessary library loads
//
// Revision 1.2  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.1  2000/03/29 00:24:54  genevb
// Introduction of macro to use StStrangeMuDstMaker
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  Uses StStrangeMuDstMaker to create a micro DST
//                with both v0's and xi's. Uncomment lines after
//                "using Monte Carlo" to include MC info in DST.
//                Uncomment lines after "for MDC3 files" to fix
//                track topology maps from MDC.
//======================================================



void load() {
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");
  gSystem->Load("StEventMaker");
  gSystem->Load("StStrangeMuDstMaker");
// The following are needed for using Monte Carlo info
//  gSystem->Load("StMcEvent");
//  gSystem->Load("StMcEventMaker");
//  gSystem->Load("StAssociationMaker");
//  gSystem->Load("StMcAnalysisMaker");
// The following is needed for MDC3 files with incorrect track topology maps
//  gSystem->Load("StHbtMaker");
}

void run() {
  // Open input file
  const Char_t *file = "/afs/rhic/star/data/samples/hc_standard.40_evts.dst.root";

  StFile *files = new StFile();
  files->AddFile(file);

  // Set number of events to analyse
  const Int_t Nevents = 10;

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  StIOMaker IOMaker("IO","r",files,"bfcTree");
  StEventMaker eventMaker("events","title");
// The following are needed for using Monte Carlo info
//  StMcEventMaker*     mcEventReader = new StMcEventMaker; 
//  StAssociationMaker* associator    = new StAssociationMaker;
// The following is needed for MDC3 files with incorrect track topology maps
//  StRandyTopMapMaker topoMapFixer();
  StStrangeMuDstMaker strangeDst("strangeMuDst");

  // Indicate input branches
  IOMaker.SetBranch("*",0,"0");           //deactivate all branches
  IOMaker.SetBranch("dstBranch",0,"r");   //activate Event Branch
  IOMaker.SetBranch("runcoBranch",0,"r"); //activate runco Branch
// The following is needed for using Monte Carlo info
//  IOMaker.SetBranch("geantBranch",0,"r"); //activate geant Branch

  // Indicate micro DST settings
  strangeDst.DoV0();     // Selects V0 vertices for micro-DST
  strangeDst.DoXi();     // Selects Xi vertices for micro-DST
  strangeDst.SetWrite(); // Sets "write" mode (using default filenames)
// The following is needed for using Monte Carlo info
//  strangeDst.DoMc();

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.Fatal(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; (i<Nevents) && (istatus!=2); i++ ) {
    switch (istatus = chain.Make(i)) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Now skipping event."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Continuing."; gMessMgr->Print(); }
    }
    if( i != Nevents) chain.Clear();
    gMessMgr->Info() << "*** Finished processing event " << i;
    gMessMgr->Print();
  }

  // Finish
  if( Nevents >= 1 ) {
    chain.Finish();
  }
}

void makeStrangeMuDst() {
  load();
  run();
}

