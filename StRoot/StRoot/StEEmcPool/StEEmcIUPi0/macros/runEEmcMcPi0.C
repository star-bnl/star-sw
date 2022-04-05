//--
//-- runEEmcMcPi0.C
//-- used by Monte-Carlo and pythia sample.
//-- example script for running the EEMC pi0 finder.  The macro
//-- will process the first nevents of the MuDst (or list of MuDst's).
//-- if nevents is negative, the macro will process all events in the
//-- mudst.
//-- author: Weihong He

//-- switch should be commented out when analysing real data
#define MONTE_CARLO
#define NEW 

class StChain;
class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;
class StMcOutputMaker;
class StEEmcA2EMaker;
class StEEmcIUClusterMaker;
class StEEmcIUPointMaker;
//class StEEmcPointFitMaker;
class StEEmcIUMixMaker;
//class StEEmcMixQAMaker;
class StSpinDbMaker;
//--
//-- globals
//--
StChain            *mChain        = 0;
St_db_Maker        *mStarDatabase = 0;
StEEmcDb           *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;
StMcOutputMaker    *mySputMk      = 0;
StEEmcIUClusterMaker *mEEclusters   = 0;
StEEmcIUPointMaker   *mEEpoints     = 0;
//StEEmcPointFitMaker *mEEpoints = 0;

StEEmcIUMixMaker     *mEEmixer      = 0;
//StEEmcMixQAMaker   *mEEmixqa      = 0;
StSpinDbMaker      *mSpinDb       = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 100;

void runEEmcMcPi0( Int_t nevents = 1000,
			Char_t *name = "dipi0_10000evts.MuDst.root",
			//Char_t *name = "/star/data05/scratch/hew/JanSample/pi0_set2.MuDst.root",
			Char_t *ofile= "test.root",
			Char_t *path = "/star/u/hew/pi0finder/ezGames/backyard/multiphoton/",
			//Char_t *path = "",///star/data13/reco/pp200/pythia6_205/25_35gev/cdf_a/y2004y/gheisha_on/p05ih/", 
			Int_t nfiles = 100)
		      
{
  
  TString pathname = path; 
  pathname += name;
  TString fileG=name;
  fileG.ReplaceAll("MuDst","geant");
  
  fileG=path+fileG;
  
  //--
  //-- Load shared libraries
  //--
  LoadLibs();
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");

  //assert(!gSystem->Load("StEEmcMcReadMaker"));

  //--
  //-- Create the analysis chain
  //--
  mChain = new StChain("eemcAnalysisChain");
  
  StIOMaker* ioMaker = new StIOMaker();

  printf("%s\n",fileG.Data());
  ioMaker->SetFile(fileG);

  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"1");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("minimcBranch",0,"r");   //activate geant Branch

  
  //--
  //-- MuDst maker for reading input
  //--
  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);

  StMcEventMaker *mcEventMaker = new StMcEventMaker();
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;

  //--
  //-- Connect to the STAR databse
  //--
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");


#ifdef MONTE_CARLO  
  //--
  //-- Setup ideal gains for processing MC data
  //--
  
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
  mStarDatabase->SetDateTime(20050101,0);

#endif

  //--
  //-- Initialize EEMC database
  //--
  new StEEmcDbMaker("eemcDb");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");

  mSpinDb = new StSpinDbMaker("mSpinDb");

#ifdef MONTE_CARLO
  //--
  //-- Initialize slow simulator
  //--
  StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
  slowSim->setNpePerMipSmd(1.0);
 
#endif
 

  //--
  //-- Energy to ADC maker
  //--
  mEEanalysis=new StEEmcA2EMaker("AandE");
  mEEanalysis->database("eemcDb");          // sets db connection
  mEEanalysis->source("MuDst",1);           // sets mudst as input
  mEEanalysis->threshold(3.0,0);            // tower threshold
  mEEanalysis->threshold(3.0,1);            // pre1 threshold 
  mEEanalysis->threshold(3.0,2);            // pre2 threshold
  mEEanalysis->threshold(3.0,3);            // post threshold
//mEEanalysis->threshold(4,5.0);            // smdu threshold
//mEEanalysis->threshold(5,5.0);            // smdv threshold
  mEEanalysis->scale(1.3);                  // scale energies by x1.2
  
  
  //--
  //-- Some simple QA histograms
  //--
  StEEmcQAMaker *eemcQA=new StEEmcQAMaker("eeqa");
  eemcQA->analysis("AandE");
  eemcQA->mudst("MuDst");
//eemcQA->trigger(96261);      // add specified trigger to list  
  eemcQA->nVertexMin=0;        // cut on min number of verticies
  eemcQA->nVertexMax=999;      // cut on max number of verticies


  //--
  //-- Cluster maker.  Generates tower, preshower, postshower clusters
  //-- (using Minesweeper algo) and smd clusters (seed strip w/ truncation).
  //--
  mEEclusters=new StEEmcIUClusterMaker("mEEclusters");
  mEEclusters->analysis("AandE");
  mEEclusters->seedEnergy(0.8,0);       // tower seed energy
  mEEclusters->seedEnergy(1.5/1000.,4); // 2 MeV smd-u strip
  mEEclusters->seedEnergy(1.5/1000.,5); // 2 MeV smd-v strip
  mEEclusters->setSeedFloor(1.0);       // floating seed threshold near clusters
  mEEclusters->setMaxExtent(3);         // maximum distance from seed strip
//mEEclusters->suppress();              // disallows seeds in two strips adjacent to any cluster
  
  //--
  //-- Point maker.  Matches pairs of smd clusters to active towers.
  //-- Energy is divided between points using a tower energy-sharing
  //-- vs position function.
  //--
  mEEpoints=new StEEmcIUPointMaker("mEEpoints");
  //mEEpoints=new StEEmcPointFitMaker("mEEpoints");
  mEEpoints->analysis("AandE");
  mEEpoints->clusters("mEEclusters");
  mEEpoints->setEnergyMode(0);
  mEEpoints->setLimit(10);
  //  mEEpoints -> doPermutations(false);

  // output maker  tracker
  HList=new TObjArray;
  StMcOutputMaker *mySputMk=new StMcOutputMaker("mcRead");
  mySputMk->SetHList(HList);


  //--
  //-- Pi0 mixer, takes the points identified above and mixes pi0 pairs.
  //--
  mEEmixer = new StEEmcIUMixMaker("mEEmixer");
  mEEmixer -> mudst("MuDst");
  mEEmixer -> analysis("AandE");
  mEEmixer -> points("mEEpoints");
  for ( Int_t i=0;i<12;i++ ) // activate all 12 sectors
    mEEmixer->sector(i); 
//mEEmixer->trigger(96251); // specify trigger id(s) to process
//for Monte-Carlo sample, fix vertex to zero. Otherwise, comment it out.
  mEEmixer->fixedVertex(0.,0.,0.);  // fixes the vertex positiion

 
  //pi0 analysis
  mEEpi0analysis=new StEEmcIUPi0Analysis("pi0analy");
  // mEEpi0analysis->trigger(96261);
  //mEEpi0analysis->minbias(96011);
  mEEpi0analysis->mudst("MuDst");
  mEEpi0analysis->points("mEEpoints");
  mEEpi0analysis->mixer("mEEmixer");
  mEEpi0analysis->analysis("AandE"); 
  mEEpi0analysis->spin("mSpinDb");


  //--
  //-- QA/analysis histograms for pi0's
  //--
  //mEEmixqa = new StEEmcMixQAMaker("mEEmixqa");
  //mEEmixqa -> mixer( "mEEmixer", 0.08, 0.18 ); // specify mixer and mass range for gated histograms
  //mEEmixqa -> points( "mEEpoints" );
  //mEEmixqa -> maxPerCluster =   1;  // max number of pairs matched to a cluster of towers
  //mEEmixqa -> maxPerSector  =   100;  // max number of pairs allowed per sector
  //mEEmixqa -> maxPerEvent   = 100;  // max number of pairs allowed per event

  mChain->ls(3);
  mChain->Init();

  //-----------------------------------------------------------------
  //--
  //-- This is where the business happens.  We loop over all events.
  //-- when mChain -> Make() is called, ::Make() will be called on 
  //-- all of the makers created above.

  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  int TTnumsmdu=0,TTnumsmdv=0,Tnumsmdu=0,Tnumsmdv=0,Tnumpoints=0,Tnumpairs=0,Tnumfp=0,n2clusteru=0,n2clusterv=0,n2point=0,j=0;
  //FILE *fout=fopen("lowratio.txt","w");assert(fout);
  while ( stat == 0 ) {


    //--
    //-- Terminate once we reach nevents --
    //--
    if ( count++ >= nevents && nevents>0 ) break;

    //--
    //-- Call clear on all makers 
    //--
    mChain -> Clear();

    
    //--
    //-- Process the event through all makers 
    //--
    stat = mChain -> Make();

    //--
    //-- Set to printout on every 10th event
    //--
    if ( (count%prescale)==0 ) //continue;
      {
	std::cout << "------------------------------------------------";
	std::cout << "event=" << count << std::endl;
      }
    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--
    Int_t nhits[]={0,0,0,0,0,0};
    float umeandiff=0,vmeandiff;
    for ( int i = 0; i < 4; i++ ) {
      //      std::cout << " layer=" << i 
      //		<< " nhits=" << mEEanalysis->numberOfHitTowers(i) << std::endl;
      nhits[i]+=mEEanalysis->numberOfHitTowers(i);
    }

    //--
    //-- Print the total number of smd strips which fired
    //--
    Int_t nu=0,nv=0;
    for ( Int_t sec=0;sec<12;sec++ )
      {
	nu+=mEEanalysis->numberOfHitStrips(sec,0);
	nv+=mEEanalysis->numberOfHitStrips(sec,1);
      }
    nhits[4]=nu;
    nhits[5]=nv;


    //--
    //-- Print number of clusters in each layer
    //--
    Int_t ncl[8]={0,0,0,0,0,0,0,0};
    for ( Int_t i=0;i<12;i++ )
      {
	ncl[0]+=mEEclusters->numberOfClusters(i,0);
	ncl[1]+=mEEclusters->numberOfClusters(i,1);
	ncl[2]+=mEEclusters->numberOfClusters(i,2);
	ncl[3]+=mEEclusters->numberOfClusters(i,3);
	ncl[4]+=mEEclusters->numberOfSmdClusters(i,0);
	ncl[5]+=mEEclusters->numberOfSmdClusters(i,1);
	ncl[6]+=mEEclusters->TnumberOfSmdClusters(i,0);
	ncl[7]+=mEEclusters->TnumberOfSmdClusters(i,1);

      }
    
    // h0->Fill(ncl[4]);
    //h1->Fill(ncl[5]);
    //h3->Fill(mEEpoints -> numberOfPoints());
    if(ncl[4]==2) n2clusteru+=1;
    if(ncl[5]==2) n2clusterv+=1;
    if(mEEpoints->numberOfPoints()==2) n2point+=1;

    
    Tnumsmdu+=ncl[4];
    Tnumsmdv+=ncl[5];
    TTnumsmdu+=ncl[6];
    TTnumsmdv+=ncl[7];
    Tnumpoints+=mEEpoints->numberOfPoints();
    Tnumpairs+=mEEmixer -> numberOfCandidates();
 
  }
  std::cout << "total number of cluster in smdu=" << Tnumsmdu << std::endl;
  std::cout << "total number of cluster in smdv=" << Tnumsmdv << std::endl;
  std::cout << "temp total number of cluster in smdu=" << TTnumsmdu << std::endl;
  std::cout << "temp total number of cluster in smdv=" << TTnumsmdv << std::endl;
  std::cout << "total number of points =" << Tnumpoints << std::endl;
  std::cout << "number of 2cluster/event in Vplane" << n2clusterv << std::endl;
  std::cout << "total number of pairs =" << Tnumpairs << std::endl;
  std::cout << "number of 2cluster/event in Uplane" << n2clusteru << std::endl;
  std::cout << "number of 2point  /event          " << n2point    << std::endl;

  //-----------------------------------------------------------------


  //--
  //-- For debugging purposes, it's often useful to print out the 
  //-- database 
  //--
  mEEmcDatabase = (StEEmcDb*)mChain->GetDataSet("StEEmcDb");
  if (mEEmcDatabase) mEEmcDatabase->exportAscii("dbdump.dat"); 

  //--
  //-- Calls the ::Finish() method on all makers
  //--
  mChain -> Finish(); 


  //--
  //-- Output the QA histograms to disk
  //--
  TFile *file=new TFile(ofile,"RECREATE");

  mEEclusters -> GetHistList() -> Write();
  mEEpoints -> GetHistList() -> Write();
  mEEanalysis->GetHistList()->Write(); 
  mEEpi0analysis->GetHistList()->Write(); 
  mySputMk->GetHistList()->Write();
  mEEmixer->GetHistList()->Write();

  file->Close();



  delete file;


  return;
    
}

void LoadLibs()
{
  //-- Load muDst shared libraries --
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcSimulatorMaker");
  gSystem->Load("StEEmcA2EMaker");
#ifdef NEW
  gSystem->Load("StEEmcIUPi0");
#else
  gSystem->Load("StMaxStripPi0");
#endif 
  gSystem->Load("StSpinDbMaker");

}
