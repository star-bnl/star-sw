// Using StV0MiniDstMaker to make v0 mini-DST
// Peter G. Jones, University of Birmingham, p.g.jones@bham.ac.uk
// $Id: makeV0MiniDst.C,v 1.5 1999/09/02 09:12:26 jones Exp $
// $Log: makeV0MiniDst.C,v $
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

void load() {
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("St_Tables");
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
