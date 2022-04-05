//
// RedoSpaceCharge.C - macro to create new MuDst.root file from
//                     event.root file, with the ability to fix
//                     space charge distortion corrections
//
// Author: G. Van Buren, BNL
//         June 11, 2003
//
// Usage:
//  RedoSpaceCharge.C(firstEvt,NEvts,redo,"some_directory/some_file.event.root")
//  RedoSpaceCharge.C(NEvts,redo,"some_directory/some_file.event.root")
//      where redo=kTRUE to redo using new SpaceCharge,
//            redo=kFALSE to use the current tracks as they are.
//  RedoSpaceCharge.C(NEvts,"some_directory/some_file.event.root")
//      where the new SpaceCharge is automatically done.
//
// Output:
//  "./some_file.MuDst.root"
//

//----------------------------------------------------------

// Interface:
void RedoSpaceCharge(const int firstEvt,
                     const int NEvts,
                     const Bool_t redo,
                     const char* inputFiles);
void RedoSpaceCharge(const int NEvts,
                     const Bool_t redo,
                     const char* inputFiles);
void RedoSpaceCharge(const int NEvts=10000000,
                     const char* inputFiles=0);
void help();
void load();
void run(const int firstEvt,
         const int NEvts,
         const Bool_t redo,
         const char* inputFiles,
         const char* outDir=0);

// Default files:
//const Char_t *defaultFiles = "/scr21/genevb/ev0/lam_100evt.event.root";
const Char_t *defaultFiles = "@o1.lis";
const Char_t *defaultOutDir = "./";

//----------------------------------------------------------

///Prints a short help message on the command line.
void help() {
  cout << "Usage:\n" <<
    " RedoSpaceCharge.C" <<
    "(firstEvt,NEvts,redo,\"some_directory/some_file.event.root\")\n" <<
    " RedoSpaceCharge.C" <<
    "(NEvts,redo,\"some_directory/some_file.event.root\")\n" <<
    " RedoSpaceCharge.C" <<
    "(NEvts,\"some_directory/some_file.event.root\")\n" <<
    endl;
  cout << "where redo=kTRUE to redo using new SpaceCharge,\n" <<
          "      redo=kFALSE to use the current tracks as they are,\n" <<
          "      and the new SpaceCharge is used if redo is omitted." << endl;
}


void load() {
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StChain");
  gSystem->Load("StTreeMaker");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTriggerDataMaker");
  gSystem->Load("StBichsel");
  gSystem->Load("StEvent");
  gSystem->Load("StSecondaryVertexMaker");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("libglobal_Tables.so");
  gSystem->Load("St_db_Maker.so");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StMuDSTMaker");
}

void run(const int firstEvt,
         const int NEvts,
         const Bool_t redo,
         const char* inputFiles,
         const char* outDir) {

  // Set I/O files/directories
  const Char_t *file  = ( (inputFiles) ? inputFiles : defaultFiles );
  const Char_t *dir  = ( (outDir) ? outDir : defaultOutDir );
  StFile *files = new StFile();
  files->AddFile(file);

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  StIOMaker            IOMaker("IOMaker","r",files,"bfcTree");
  St_db_Maker          dbMaker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
  StMagFMaker          magfMk; // now required for StTpcDbMaker
  StTpcDbMaker         tpcDbMk("tpcDb");
  StDetectorDbMaker    detDbMk;
  StRedoTracks         redoMk;
  StV0FinderMaker      secondaryFinder;
  StStrangeMuDstMaker  strangeDst("strangeMuDst");
  StMuDstMaker         commonDst(1,1,dir);

  // Indicate input branches
  IOMaker.SetBranch("*",0,"0");           //deactivate all branches
  IOMaker.SetBranch("eventBranch",0,"r"); //activate Event Branch
  IOMaker.SetBranch("runcoBranch",0,"r"); //activate runco Branch
  IOMaker.SetBranch("emcBranch",0,"r");   //activate EMC Branch

  // Indicate strangeness DST settings
  strangeDst.DoV0();     // Selects V0 vertices for micro-DST
  strangeDst.DoXi();     // Selects Xi vertices for micro-DST
  strangeDst.DoKink();   // Selects Kink vertices for micro-DST
  strangeDst.SetNoKeep();// Sets "no keep" mode

  // Indicate common DST settings
  commonDst.setProbabilityPidFile();
  StMuL3Filter* l3Filter = new StMuL3Filter();
  commonDst.setL3TrackFilter(l3Filter); 
  StMuFilter* filter = new StMuFilter();
  commonDst.setTrackFilter(filter);

  // Indicate StRedoTracks maker settings
  if (!redo) redoMk.DontDo();

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.Fatal(istatus,"on init"); return; }

  IOMaker.Skip(firstEvt);

  // Loop over events
  for( Int_t i=0; (i<NEvts) && (istatus!=2); i++ ) {
    chain.Clear();
    switch (istatus = chain.Make()) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Now skipping event."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Continuing."; gMessMgr->Print(); }
    }
    gMessMgr->Info() << "*** Finished processing event " << i;
    gMessMgr->Print();
  }

  // Finish
  if( NEvts >= 1 ) chain.Finish();
}

void RedoSpaceCharge(const int firstEvt,
                     const int NEvts,
                     const Bool_t redo,
                     const char* inputFiles) {
  load();
  run(firstEvt,NEvts,redo,inputFiles,defaultOutDir);
}

void RedoSpaceCharge(const int NEvts,
                     const Bool_t redo,
                     const char* inputFiles) {
  RedoSpaceCharge(0,NEvts,redo,inputFiles);
}

void RedoSpaceCharge(const int NEvts,
                     const char* inputFiles) {
  RedoSpaceCharge(0,NEvts,kTRUE,inputFiles);
}

//----------------------------------------------------------
// $Id: RedoSpaceCharge.C,v 1.5 2015/07/27 15:29:56 jeromel Exp $
// $Log: RedoSpaceCharge.C,v $
// Revision 1.5  2015/07/27 15:29:56  jeromel
// LOad StrangeMuDstMkaer again (restored)
//
// Revision 1.4  2015/07/22 17:39:47  jeromel
// Comment out loading StStrangeMuDstMaker as disabled
//
// Revision 1.3  2010/02/22 20:05:16  genevb
// Using StTpcDbMaker now requires StMagFMaker
//
// Revision 1.2  2005/08/31 15:02:43  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.1  2003/07/11 20:26:54  genevb
// Introduction of RedoSpaceCharge.C macro
//
//

