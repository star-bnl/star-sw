// $Id: makeV0MiniDst.C,v 1.8 2000/04/13 21:46:35 kathy Exp $
// $Log: makeV0MiniDst.C,v $
// Revision 1.8  2000/04/13 21:46:35  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.7  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.6  2000/01/05 22:18:06  genevb
// Put comments in order that QA wants
//
// Revision 1.5  1999/09/02 09:12:26  jones
// Changes reflect new I/O file handling
//
// Revision 1.4  1999/08/31 20:49:08  genevb
// StV0MiniDst class now depends on StAnalysisUtilities library
//
// Revision 1.3  1999/08/13 13:36:44  jones
// Modified to reflect new revision of StV0MiniDstMaker
//
// Revision 1.2  1999/07/30 15:07:31  genevb
// Take advantage of StHFillObject inheritance
//
// Revision 1.1  1999/07/13 12:51:01  jones
// Added macros for making and reading v0 mini-DSTs using StV0MiniDstMaker
//
//======================================================
// owner:  Peter G. Jones, University of Birmingham, p.g.jones@bham.ac.uk
// what it does:  uses StV0MiniDstMaker to create a V0 mini-DST
//======================================================



void load() {
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StMagF");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StV0MiniDstMaker");
}

void run() {
  // Open input file
  const Char_t *file = "/afs/rhic/star/strange/xi_dst.xdf";

  StFile *files = new StFile();
  files->AddFile(file);

  // Set number of events to analyse
  const Int_t Nevents = 10;

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  StIOMaker IOMaker("IO","r",files,"bfcTree");
  StEventMaker eventMaker("events","title");
  StV0MiniDstMaker v0dst("v0dst");

  v0dst.SetOutputFile("V0MicroDst.root"); // Set output filename
  v0dst.SetV0VertexType(); // Selects V0 vertices for micro-DST

  // Do init
  Int_t ierr = chain.Init();
  if( ierr ) chain.Fatal(ierr,"on init");

  // Loop over events
  for( Int_t i=1; i<=Nevents; i++ ) {
    if( chain.Make(i) ) break;
    if( i != Nevents) chain.Clear();
    printf("*** Finished processing event %d\n",i);
  }

  // Finish
  if( Nevents >= 1 ) {
    chain.Finish();
  }
}
