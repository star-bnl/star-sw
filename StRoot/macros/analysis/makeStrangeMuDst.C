// $Id: makeStrangeMuDst.C,v 1.1 2000/03/29 00:24:54 genevb Exp $
// $Log: makeStrangeMuDst.C,v $
// Revision 1.1  2000/03/29 00:24:54  genevb
// Introduction of macro to use StStrangeMuDstMaker
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  uses StStrangeMuDstMaker to create a micro DST
//                with both v0's and xi's
//======================================================



void load() {
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("St_Tables");
  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");
  gSystem->Load("StEventMaker");
  gSystem->Load("StStrangeMuDstMaker");
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
  StStrangeMuDstMaker strangeDst("strangeMuDst");

  strangeDst.DoV0();     // Selects V0 vertices for micro-DST
  strangeDst.DoXi();     // Selects Xi vertices for micro-DST
  strangeDst.SetWrite(); // Sets "write" mode (using default filenames)

  // Do init
  Int_t ierr = chain.Init();
  if( ierr ) { chain.Fatal(ierr,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<Nevents; i++ ) {
    if( chain.Make(i) ) break;
    if( i != Nevents) chain.Clear();
    printf("*** Finished processing event %d\n",i);
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

