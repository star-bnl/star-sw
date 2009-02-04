//
// minesweeper.C
//
// Runs the EEmc tower-only cluster finder.
//



void pi0nsweeper( int    nEve     = 10,
		  char  *file     = "/star/data04/sim/MuDst/minbias_pp200_pythia6.203_2003_hadronicon_trsic/rcf1200_2576_2000evts.MuDst.root",
		  Int_t  nFiles   = 1,
		  char  *inDir    = "NONE",
		  char  *dbFlavor = "NONE",
		  char  *outfile  = "pi0ns.hist.root",
		  float  scaleFactor = 0.8
		  ) {
  
  // Output an initial summary of what we're doing...
  std::cout << "Running EEMC Tower Only pi0 Finder" << std::endl;
  std::cout << "Processing:   " << nEve << " events" << std::endl;
  std::cout << "Input file:   " << file << std::endl;
  std::cout << "DB Flavor:    " << dbFlavor << std::endl;
  std::cout << "Output file:  " << outfile << std::endl;
  std::cout << "scaleFactor:  " << scaleFactor << std::endl;
  std::cout << std::flush;

  // Load dynamic libraries
  LoadLibs();

  // Create the Analysis Chain
  chain = new StChain("StChain"); 
 
  // Some histograms for study, not compiled into our makers
  TH1F *nclust = new TH1F("nclust","Number of clusters",10,0,10);

  // Now we add Makers to the chain...   


  // The cluster maker is designed to run on micro dst's rather
  //   than StEvent files.  We initialize w/ default "MuDst" name...
  std::cout << "Instantiating muDstMk: " << file << std::endl;
  StMuDstMaker *muDstMk = new StMuDstMaker(0,0,"",file,"",nFiles);
  StMuDbReader *muDb    = StMuDbReader::instance();

  //
  // Configure the database, based on user input.  We default to
  //   "NONE" here, which will skip the database and (for the 
  //   time being) the cluster maker defaults to idealized gains
  //   in use in the Monte Carlo.
  //
  if ( dbFlavor != "NONE" &&
       dbFlavor != "none" ) {

    // Database Makers
    St_db_Maker    *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");
    StEEmcDbMaker  *myMk = new StEEmcDbMaker("eemcDb");  
    // Force a timestamp here, so as not to run afoul of an assert
    dbMk -> setTimeStampDay(20030516);
 
  }


  // Cluster maker
  StMuEETowerClusterMaker 
    *muEEClusterMaker = new StMuEETowerClusterMaker("muEEClusterMaker","MuDst");
  // Set seed threshold of 0.6 GeV, and a user specified "scale factor".
  //   The default on this is 1.0, but should be 0.8 for Monte Carlo events,
  //   as this is needed to reconstruct the pi0 mass in the Monte Carlo...
  muEEClusterMaker -> setSeedThreshold(0.6);
  muEEClusterMaker -> setScaleFactor(scaleFactor);


  // After the cluster maker, we add the pi0n finder to the
  //   chain.  This pi0n finder requires that the two seed 
  //   towers identified by the cluster finder be separated
  //   by at least 1 tower.  Combinatoric backgrounds are
  //   estimated by randomly mixing clusters from previous
  //   events.

  // NOTE: At present, the combinatoric background has problems.
  //   This is probably due to a combination of factors,
  //   including not quite getting the 1-tower separation
  //   correct for mixed events.  The net pi0 yields should
  //   only be trusted at about the 20% level right now...
  
  StMuEETowerPi0nMaker *muPi0nMaker = 
    new StMuEETowerPi0nMaker("muPi0nMaker","MuDst","muEEClusterMaker");


  // Initialize the chain
  chain->Init();
  chain->ls(3);

    StEEmcDb *eemcDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
    // Only look at the bottom (aka "first") 1/3 of the endcap
    eemcDb -> setSectors(5,8); 
    // Setup default flavor
    eemcDb -> setPreferedFlavor( dbFlavor, "eemcPMTcal" ); 

  int eventCounter=0;
  int stat=0;

  //---------------------------------------------------
  while ( stat==0 ) {// loop over events    

    if(eventCounter>=nEve) break;
    chain->Clear();

    std::cout << "Processing event number " << eventCounter << std::endl;
    stat = chain->Make();

    // Fill histograms if you really want to
    nclust -> Fill( muEEClusterMaker -> getNClusters() );

    // Print a summary of the cluster finder
    //   and pi0n candidates

    if ( muEEClusterMaker -> getNClusters() > 1 ) {
      muEEClusterMaker -> Print();
      muPi0nMaker -> Print();
    }

    eventCounter++;

  }

  // Tell us what happened if we terminated earlyx
  if ( stat != 0 ) std::cout << "Loop terminated w/ stat = " << stat << std::endl;

  TFile f( outfile, "RECREATE" );
  f.cd();

  // Output a selection of the many histograms
  //   created by the pi0 finder...
  muPi0nMaker -> GetHistList() -> FindObject("mEventStats") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mMass_real_int") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mMass_mixed_int") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mEnergy_real_int") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mEnergy_mixed_int") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mZ_real_int") -> Write();
  muPi0nMaker -> GetHistList() -> FindObject("mZ_mixed_int") -> Write();



  //////////////////////////////////////////////////////////////////

}


void LoadLibs() {

  std::cout << "Loading libraries" << std::endl;

  if (gClassTable->GetID("TTable") < 0)
  gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  assert(gSystem->Load("StMuDSTMaker")==0);
  assert(gSystem->Load("StEEmcUtil")==0);
  assert(gSystem->Load("StEEmcPoolmuDst")==0 );
  assert(gSystem->Load("StEEmcPoolTowerOnly")==0 );

}
