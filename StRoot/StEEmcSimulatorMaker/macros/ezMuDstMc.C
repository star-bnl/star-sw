/////////////////////////////////////////////////////////////////////////////
//
// ezMuDstMc.C
//
// Author: Jason C. Webb <jwebb@iucf.indiana.edu>
//
// Example macro for running the StMuEEmcSimuReMaker.
//
// 1. Checkout and compile the code needed to run this example:
//
// $ cvs co StRoot/StEEmcSimulatorMaker
// $ cvs co StRoot/StEEmcPool/StMuEEmcClusterMaker
// $ cons
//
// (produces 1 warning on EEezCluster)
//
// 2. Run the code
//
// $ root4star -q -b ezMuDstMc.C
//
// 3. Output will be in a file named according to the input file,
//    MuDst replaced with hist.

class StChain;
class StEEmcDb;
class StMuEmcCollection;
class St_db_Maker;

StChain       *chain=0;
StEEmcDb      *eemcDb=0;
St_db_Maker   *starDb=0;

// eemc5090009.lis

void ezMuDstMc( Int_t nevents = 10,
		Char_t *file = "mcgamma_5000_06TC05_10.MuDst.root",
		Int_t nfiles = 1,
		Char_t *inDir = "/star/data04/sim/jwebb/MonteCarlo/single_gamma/" ) {


  //-- Load in shared libraries
  gROOT -> LoadMacro("StRoot/StEEmcPool/StMuEEmcClusterMaker/macros/StMuEEmcClusterMakerUtils.C");
  loadLibraries();

  gSystem -> Load("StEEmcSimulatorMaker");

  //-- Create the analysis chain
  chain = new StChain("StChain");

  //-- Add the muDst maker
  muDst = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nfiles);

  //-- EEMC database goes here
  starDb = new St_db_Maker("StarDb","MySQL:StarDb");
  new StEEmcDbMaker("eemcDb");

  //-- override timestamp just so something happens...
  starDb -> setTimeStampDay(20040320);  // format: yyyymmdd 

  //-- Create an instance of the EEMC "slow" muDst simulator,
  StMuEEmcSimuReMaker *muSim = new StMuEEmcSimuReMaker("muSim");
  muSim -> setDbName( "eemcDb" );
 
  //-- Add the EEMC cluster maker
  TString myname = file;
  myname.ReplaceAll("MuDst","clusters");
  TFile *myFile1 = new TFile(myname,"RECREATE");
  myFile1 -> cd();
  muCluster = new StMuEEmcClusterMaker();
  muCluster -> setSeedEnergy(0.7);
  muCluster -> setShapeLimit(0.75);
  myFile1 -> cd("..");
 
  //-- Initialize the chain
  chain -> Init();
  chain -> ls(3);

  eemcDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
  //$$$cDb -> setThreshold(5.0);
  eemcDb -> setPreferredFlavor( "onlped", "eemcPMTped" );
  eemcDb -> setPreferredFlavor( "highStrip1", "eemcPIXcal" );
  //$$$eemcDb -> requestDataBase(20040320,1,12);
  //--
  //-- At this point, one could override the database
  //-- values by reading in an ascii file.
  //--
  //$$$  eemcDb -> setAsciiDatabase("eemc-database-smd.dat");



  //-- Loop over all requested events, or until we hit an error--
  Int_t stat  = 0;
  Int_t event = 0;
  Int_t first = 0;


  for ( event = first; event < nevents; event++ ) {

    //    if ( event++ >= nevents ) break;
    if ( stat != 0 ) break;

    //-- Clear the chain from the previous event
    chain -> Clear();

    std::cout << "Processing event number " << event << std::endl;

    //-- Call ::Make() on all makers
    stat = chain -> Make();

    //-- Print out number of seed towers found (causes seg fault for some reason?)
    std::cout << "N seed    = " << muCluster -> ezAnalysis() -> getNSeedTowers() << std::endl;
    std::cout << "N cluster = " << muCluster -> ezClusters() -> getNClusters() << std::endl;
    std::cout << std::endl;
    

  }

  eemcDb -> exportAscii("test.db");
  myFile1 -> Write();

}
	      
	      
