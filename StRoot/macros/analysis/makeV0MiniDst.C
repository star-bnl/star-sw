// Using StV0MiniDstMaker to make v0 mini-DST
// Peter G. Jones, University of Birmingham, p.g.jones@bham.ac.uk
// $Id: makeV0MiniDst.C,v 1.1 1999/07/13 12:51:01 jones Exp $
// $Log: makeV0MiniDst.C,v $
// Revision 1.1  1999/07/13 12:51:01  jones
// Added macros for making and reading v0 mini-DSTs using StV0MiniDstMaker
//

void load() {
  gSystem->Load("St_base");
  gSystem->Load("St_Tables");
  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StV0MiniDstMaker");
}

void run() {
  // Open input file
  //  const Char_t *file = "/afs/rhic/star/strange/jones/xi_new.xdf";
  const Char_t *file = "/afs/rhic/star/data3/test/dev/tfs_Solaris/Fri/year_1b/psc0050_01_40evts_dst.xdf";

  StFile *files = new StFile();
  files->AddFile(file);

  // Set number of events to analyse
  const Int_t Nevents = 10;

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  StIOMaker IOMaker("IO","r",files,"bfcTree");
  StEventMaker eventMaker("events","title");
  StV0MiniDstMaker v0dst("v0dst","V0MiniDst.root"); // Sets mini-DST filename

  //  IOMaker.SetDebug();

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
