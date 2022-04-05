//
// Runs StMuEEmcPreCluster maker, performs a (trivial) clustering
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
class StMuEEmcPreClusterMaker;
class StMuDbReader;
class StEEmcDb;
class St_db_Maker;

StChain                  *chain;
StIOMaker                *ioMaker;
StMuDstMaker             *muDstMaker;
StMuEEmcPreAnalysisMaker *muEEmcAnal;
StMuEEmcPreClusterMaker  *muEEmcClust;
StMuDbReader             *muDb;
StEEmcDb                 *eemdDb;
St_db_Maker              *starDb;

#include <iomanip>
#include <vector>

void runEEmcPreCluster ( Int_t nevents = 100,  // Number of events
			 Char_t *muDst = "MuDst/mcPi0n_field_onelectron_10000_06TC05_20.MuDst.root",
			 Char_t *output = "output.root" // Output file
			 );

/////////////////////////////////////////////////////////////////////////////

void runEEmcPreCluster ( Int_t nevents, 
			 Char_t *muDst, 
			 Char_t *output ) {

  if ( output != "" ) 
    TFile *f = new TFile( output, "RECREATE" );

  ////////////////////////////////////////////
  //
  // Preshower, postshower and tower 2D histos
  //
  TH2F *hPre1VsTow  = new TH2F("hPre1VsTow", "Preshower(1) energy deposit vs reco tower E", 100, 0., 20., 500, 0., 0.08);
  TH2F *hPre2VsTow  = new TH2F("hPre2VsTow", "Preshower(2) energy deposit vs reco tower E", 100, 0., 20., 500, 0., 0.08);
  TH2F *hPostVsTow  = new TH2F("hPostVsTow", "Postshower   energy deposit vs reco tower E", 100, 0., 20., 500, 0., 0.08);
  TH2F *hPre1VsPre2 = new TH2F("hPre1VsPre2","Preshower(1) energy deposit vs preshower(2) energy deposit", 500, 0., 0.08, 500, 0., 0.08);


  ////////////////////////////////////////////
  //
  // Load shared libraries
  //
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StEEmcUtil");
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
  muEEmcClust = new StMuEEmcPreClusterMaker();

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

    //    if ( !ievent % 100 ) 
    std::cout << "Processing event number " << ievent << std::endl;

    // Clear all makers on the chain
    chain -> Clear();

    // Call all of the makers on the chain
    status = chain -> Make();

    for ( Int_t i = 0; i < muEEmcClust -> getNumClusters(); i++ ) {

      StMuEEmcCluster cluster = muEEmcClust -> getCluster(i);

      Float_t energy_tow  = cluster.getEnergy(0);
      Float_t energy_pre1 = cluster.getEnergy(1);
      Float_t energy_pre2 = cluster.getEnergy(2);
      Float_t energy_post = cluster.getEnergy(3);

      hPre1VsTow -> Fill( energy_tow, energy_pre1 );
      hPre2VsTow -> Fill( energy_tow, energy_pre2 );
      hPostVsTow -> Fill( energy_tow, energy_post );
      hPre1VsPre2 -> Fill ( energy_pre2, energy_pre1 );
      
    }

    // Increment the event counter
    ievent++; 

  }

  f->cd();
  muEEmcClust -> GetHistList() -> Write();

  f -> Write();

}
