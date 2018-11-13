/**
 * \brief A simple example of reading TTree (PicoDst)
 *
 * A simple example that loads file, TTree (PicoDst)
 * sets up branches to read, and prints few quantities
 */
void SimplePicoDstAnalyzer( const char* file = "data/auau200.picoDst.root" ) {

  // Set input file
  TFile *f = new TFile( file );
  if ( !f ) { std::cout << "no file found" << std::endl; return; }
  // Set TTree
  TTree *T = f->Get("PicoDst");
  if ( !T ) { std::cout << "no PicoDst tree found" << std::endl; return; }

  // Turn on/off branches
  T->SetBranchStatus("*", 0);
  T->SetBranchStatus("Event", 1);
  T->SetBranchStatus("Event.*", 1);
  T->SetBranchStatus("Track", 1);
  T->SetBranchStatus("Track.*", 1);
  T->SetMakeClass(1);

  // Retrieve number of events
  Long64_t nentries = T->GetEntriesFast();
  std::cout << "entries: " << nentries << std::endl;

  Int_t           Event_;
  Int_t           Event_mRunId[1];
  Int_t           Event_mEventId[1];
  UShort_t        Event_mNumberOfGlobalTracks[1];

  Int_t           Track_;
  Float_t         Track_mOrigin_mX1[10000];
  Float_t         Track_mOrigin_mX2[10000];
  Float_t         Track_mOrigin_mX3[10000];

  // Set up branches to read
  T->SetBranchAddress("Event", &Event_ );
  T->SetBranchAddress("Event.mRunId", Event_mRunId );
  T->SetBranchAddress("Event.mEventId", Event_mEventId );
  T->SetBranchAddress("Event.mNumberOfGlobalTracks", Event_mNumberOfGlobalTracks );

  T->SetBranchAddress("Track", &Track_ );
  T->SetBranchAddress("Track.mOrigin.mX1", Track_mOrigin_mX1 );
  T->SetBranchAddress("Track.mOrigin.mX2", Track_mOrigin_mX2 );
  T->SetBranchAddress("Track.mOrigin.mX3", Track_mOrigin_mX3 );

  Long64_t ientry = 0;

  std::cout << "getting entry " << ientry << std::endl;

  // Load values for the current event
  Long64_t nbytes = T->GetEntry( ientry );

  std::cout << "entry received" << std::endl;
  std::cout << "Run ID: " << Event_mRunId[0] << std::endl;
  std::cout << "Event ID: " << Event_mEventId[0] << std::endl;
  std::cout << "num tracks: " << Track_ << std::endl;

  std::cout << "Track 0: "
	    << Track_mOrigin_mX1[0] << ", "
	    << Track_mOrigin_mX2[0] << ", "
	    << Track_mOrigin_mX3[0]
	    << std::endl;

  std::cout << "Track 1: "
	    << Track_mOrigin_mX1[1] << ", "
	    << Track_mOrigin_mX2[1] << ", "
	    << Track_mOrigin_mX3[1]
	    << std::endl;

  // Print info for the turned on branches
  T->Show(0);
}	
