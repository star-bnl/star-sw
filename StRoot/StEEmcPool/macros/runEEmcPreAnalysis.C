//
// Runs StMuEEmcPreAnalysis maker, performs a (trivial) clustering
//   around the high-tower in each event, and generates some histograms
//   of tower, preshower and postshower response, and SMD response...
//
// Binning on the histograms may not be optimal...
//


// forward declarations
class StChain;
class StIOMaker;
class StMuDstMaker;
class StMuEEmcPreAnalysisMaker;
class StMuDbReader;
class StEEmcDb;
class St_db_Maker;

StChain                  *chain;
StIOMaker                *ioMaker;
StMuDstMaker             *muDstMaker;
StMuEEmcPreAnalysisMaker *muEEmcAnal;
StMuDbReader             *muDb;
StEEmcDb                 *eemdDb;
St_db_Maker              *starDb;

#include <iomanip>

void runEEmcPreAnalysis ( Int_t nevents = 100,  // Number of events
		      Int_t smdhist = 10,   // Prescale on SMD histograms
		      Char_t *muDst = "MuDst/mcPi0n_field_onpi0_10000_06TC05_5.MuDst.root",
		      Char_t *output = "output.root" // Output file
		      );

/////////////////////////////////////////////////////////////////////////////

void runEEmcPreAnalysis ( Int_t nevents, Int_t smdhist, Char_t *muDst, Char_t *output ) {


  if ( output != "" ) 
    TFile *f = new TFile( output, "RECREATE" );

  ////////////////////////////////////////////
  //
  // Preshower, postshower and tower 2D histos
  //
  TH2F *hPre1VsTow  = new TH2F("hPre1VsTow", "Preshower(1) energy deposit vs reco tower E", 100, 0., 20., 100, 0., 0.2);
  TH2F *hPre2VsTow  = new TH2F("hPre2VsTow", "Preshower(2) energy deposit vs reco tower E", 100, 0., 20., 100, 0., 0.2);
  TH2F *hPostVsTow  = new TH2F("hPostVsTow", "Postshower   energy deposit vs reco tower E", 100, 0., 20., 100, 0., 0.2);
  TH2F *hPre1VsPre2 = new TH2F("hPre1VsPre2","Preshower(1) energy deposit vs preshower(2) energy deposit", 100, 0., 0.2, 100, 0., 0.2);


  ////////////////////////////////////////////
  //
  // Load shared libraries
  //
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StEEmcUtil.so");
  gSystem->Load("StMuEEmcPreAnalysisMaker.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");

  ////////////////////////////////////////////
  //
  // Create the chain and add the muDst maker
  //
  chain      = new StChain("StChain");
  muDstMaker = new StMuDstMaker(0,0,"",muDst,"MuDst.root",1);
  muDb       = StMuDbReader::instance();

  starDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  new StEEmcDbMaker("eemcDb");

  muEEmcAnal = new StMuEEmcPreAnalysisMaker();

  ////////////////////////////////////////////
  //
  // Initialize the chain in preparation to loop over events
  //
  chain -> Init();
  chain -> ls(3);
  eemcDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
  assert(eemcDb);
  //  starDb -> setTimeStampDay(20030516);
  eemcDb -> setTimeStampDay(20040101);
  eemcDb -> setPreferedFlavor( "set492", "eemcPMTcal" );

  ////////////////////////////////////////////
  // 
  // Event Loop
  //
  
  Int_t status = 0;
  Int_t ievent = 0;
  while ( !status && ievent < nevents ) {

    if ( !ievent % 100 ) std::cout << "Processing event number " << ievent << std::endl;

    // Clear all makers on the chain
    chain -> Clear();

    // Call all of the makers on the chain
    status = chain -> Make();

    // Get the high-tower from the EEMC analysis maker
    StMuEEmcTower *seed = muEEmcAnal -> getHighTower();
    //    StMuEEmcTower *neighbors[] = seed -> getNeighbors();

    // We take the 8 surrounding towers, plus the central
    //   "seed" tower, to be the cluster.  The following
    //   loop determines the total energy of the cluster,
    //   total energy deposited in pre and postshower
    //   layers, and determines the first and last U and
    //   V smd strips within the "fiducial volume" of the
    //   cluster.

    Float_t energy      = seed -> getEnergy(0);
    Float_t energy_pre1 = seed -> getEnergy(1);
    Float_t energy_pre2 = seed -> getEnergy(2);
    Float_t energy_post = seed -> getEnergy(3);

    StMuEEmcStrip *ufirst = seed -> getFirstStrip('U');
    StMuEEmcStrip *vfirst = seed -> getFirstStrip('V');
    StMuEEmcStrip *ulast  = seed -> getLastStrip('U');
    StMuEEmcStrip *vlast  = seed -> getLastStrip('V');

    std::cout << ievent << " seed:    " 
	      << std::setw(7) << std::setprecision(3) << energy << " "
	      << std::setw(7) << std::setprecision(3) << energy_pre1 << " "
	      << std::setw(7) << std::setprecision(3) << energy_pre2 << " "
	      << std::setw(7) << std::setprecision(3) << energy_post << " " 
	      << std::setw(3) << ufirst -> getId() << " "
	      << std::setw(3) << ulast -> getId() << " "
	      << std::setw(3) << vfirst -> getId() << " "
	      << std::setw(3) << vlast -> getId() << " "
	      << std::endl;

    StMuEEmcTower *neighbor;

    for ( Int_t i = 0; i < 8; i++ ) {

      // Get a pointer to the neighboring tower, and 
      // punt if its NULL
      neighbor = seed -> getNeighbor(i);
      if ( !neighbor ) continue;

      energy      += neighbor -> getEnergy(0);
      energy_pre1 += neighbor -> getEnergy(1);
      energy_pre2 += neighbor -> getEnergy(2);
      energy_post += neighbor -> getEnergy(3);

      StMuEEmcStrip *mytmp = neighbor -> getFirstStrip('U');
      if ( mytmp -> getId() < ufirst -> getId() ) ufirst = mytmp;
      mytmp = neighbor -> getFirstStrip('V');
      if ( mytmp -> getId() < vfirst -> getId() ) vfirst = mytmp;
      mytmp = neighbor -> getLastStrip('U');
      if ( mytmp -> getId() > ulast -> getId() ) ulast = mytmp;
      mytmp = neighbor -> getLastStrip('V');
      if ( mytmp -> getId() > vlast -> getId() ) vlast = mytmp;

    }

    // Fill preshower/postshower histos
    hPre1VsTow -> Fill( energy, energy_pre1 );
    hPre2VsTow -> Fill( energy, energy_pre2 );
    hPostVsTow -> Fill( energy, energy_post );
    hPre1VsPre2-> Fill( energy_pre2, energy_pre1 );

    ////////////////////////////////////////////
    //
    // How often do we create a histogram for
    //   the SMD strips?
    //
    if ( !(ievent % smdhist) ) {

      f->cd();

      // Book and fill a histogram for SMD's (on event-by-event basis)
      TString myname = "uEvent"; myname += ievent;
      TString mytitle = "U SMD strip energy distribution";
      Int_t min = ufirst -> getId() - 1;
      Int_t max = ulast  -> getId() + 1;
      Int_t nbin = ( max - min + 1 );
      TH1F *uhist = new TH1F(myname,mytitle,nbin,min,max);
      
      StMuEEmcStrip *strip;
      for ( strip = ufirst; strip <= ulast; strip++ ) {
	//$$$uhist -> Fill( (Float_t)strip->getId(), strip -> getEnergy() );

	Int_t   id     = strip -> getId() - min;
	Float_t myenergy = strip -> getEnergy();
	Float_t nphoto = (myenergy>0.) ? strip -> getNumPhotoElectrons() : 1;
	
	uhist -> SetBinContent( id, myenergy );
	uhist -> SetBinError  ( id, myenergy / sqrt(nphoto) );
	
      }
      
      myname = "vEvent"; myname += ievent;
      mytitle = "V SMD strip energy distribution";
      min = vfirst -> getId() - 1;
      max = vlast  -> getId() + 1;
      nbin = ( max - min + 1 );
      TH1F *vhist = new TH1F(myname,mytitle,nbin,min,max);
      
      for ( strip = vfirst; strip <= vlast; strip++ ) {
	//$$$vhist -> Fill( (Float_t)strip->getId(), strip -> getEnergy() );

	Int_t   id     = strip -> getId() - min;
	Float_t myenergy = strip -> getEnergy();
	Float_t nphoto = (myenergy>0.) ? strip -> getNumPhotoElectrons() : 1;
	
	vhist -> SetBinContent( id, myenergy );
	vhist -> SetBinError  ( id, myenergy / sqrt(nphoto) );

      }

    }
    ////////////////////////////////////////////



    // Let the user know we're still alive
    std::cout << ievent << " cluster: " 
	      << std::setw(7) << std::setprecision(3) << energy << " "
	      << std::setw(7) << std::setprecision(3) << energy_pre1 << " "
	      << std::setw(7) << std::setprecision(3) << energy_pre2 << " "
	      << std::setw(7) << std::setprecision(3) << energy_post << " " 
	      << std::setw(3) << ufirst -> getId() << " "
	      << std::setw(3) << ulast -> getId() << " "
	      << std::setw(3) << vfirst -> getId() << " "
	      << std::setw(3) << vlast -> getId() << " "
	      << std::endl
	      << std::endl;

    // Increment the event counter
    ievent++; 

  }

  f -> Write();

}
