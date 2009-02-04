//#define MONTE_CARLO

class StChain;
class StMuEmcCollection;

class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;

StChain        *mChain        = 0;
St_db_Maker    *mStarDatabase = 0;
StEEmcDb       *mEEmcDatabase = 0;
StMuDstMaker   *mMuDstMaker   = 0;

class StEEmcA2EMaker; 
StEEmcA2EMaker *mEEmcA2E = 0;

class StEEmcClusterMaker;
StEEmcClusterMaker *mEEclusters=0;



Bool_t useSlow=1;

void runEEmcPi0Maker( Int_t nevents = 5000, 
		      Char_t *name = "mcpi0_5000_06TC05_10.MuDst.root",
		      Char_t *ofile= "mcpi0_5000_06TC05_10.pi0tree.root",
		      Char_t *path = "/star/data04/sim/jwebb/MonteCarlo/single_gamma/", 
		      Int_t nfiles = 100
		      )
{



  TString pathname = path; 
  pathname += name;

  gROOT->LoadMacro("StRoot/StEEmcPool/StMaxStripPi0/macros/loadlibs.C");
  loadlibs();
  
  /// This MUST be created in order to ::Instance() to work.(?)
  EEmcGeomSimple *g=new EEmcGeomSimple();
  

  //-- Create the analysis chain --
  mChain = new StChain("eemcAnalysisChain");

  //-- Create micro-dst maker and load in MuDsts --
  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);

  //-- Initialize database connection --
  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  
  //-- DATE TIME FOR MONTE CARLO --
#ifdef MONTE_CARLO  // flags for M-C events
  TDatime *time = new TDatime();
  mStarDatabase -> SetDateTime( time -> GetDate(), time -> GetTime() );
  mStarDatabase->SetDateTime(20031120,0);
  mStarDatabase->SetFlavor("sim","eemcPMTcal");
  mStarDatabase->SetFlavor("sim","eemcPIXcal");
  mStarDatabase->SetFlavor("sim","eemcPMTped");
  mStarDatabase->SetFlavor("sim","eemcPMTstat");
  mStarDatabase->SetFlavor("sim","eemcPMTname");
  mStarDatabase->SetFlavor("sim","eemcADCconf");
#endif

  //-- Initialize EEMC database --
  new StEEmcDbMaker("eemcDb");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");

#ifdef MONTE_CARLO
  //-- Create the slow simulator for the endcap
  if ( useSlow ) {
    StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
    slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
    slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
    slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
    slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
  }
#endif


  
  mEEmcA2E=new StEEmcA2EMaker("AandE");
  mEEmcA2E->database("eemcDb");
  mEEmcA2E->source("MuDst",1);
  mEEmcA2E->threshold(0,3.0);
  mEEmcA2E->threshold(1,3.0);
  mEEmcA2E->threshold(2,3.0);
  mEEmcA2E->threshold(3,3.0);
#ifdef MONTE_CARLO
  mEEmcA2E->scale(1.2);
#endif

  
  mEEclusters=new StEEmcClusterMaker("mEEclusters");
  mEEclusters->analysis("AandE");
  mEEclusters->seedEnergy(0.8,0); // tower seed energy
  mEEclusters->seedEnergy(2.0/1000.,4); // 2 MeV smd-u strip
  mEEclusters->seedEnergy(2.0/1000.,5); // 2 MeV smd-v strip
  mEEclusters->setSeedFloor(3.0);
  //  mEEclusters->setFillStEvent();

  mEEclusters->setMaxExtent(3);
 

  
      
  mEEpoints=new StEEmcPointMaker("mEEpoints"); 
  mEEpoints->analysis("AandE");
  mEEpoints->clusters("mEEclusters"); 
  
  
  
  //mEEmixer=new StEEmcMixMaker("mEEmixer"); 
  mEEmixer=new StEEmcMixTreeMaker("mEEmixer");
  mEEmixer->setFilename( ofile  );  
  mEEmixer->mudst("MuDst");
  mEEmixer->analysis("AandE");
  mEEmixer->points("mEEpoints");
  mEEmixer->sector(4);
  mEEmixer->sector(5);
  mEEmixer->sector(6);
  mEEmixer->sector(7);
  mEEmixer->trigger(45203);


  mEEmixer2 = new StEEmcMixHistMaker("mEEmixer2");
  mEEmixer2->mudst("MuDst");
  mEEmixer2->analysis("AandE");
  mEEmixer2->points("mEEpoints");
  mEEmixer2->sector(4);
  mEEmixer2->sector(5);
  mEEmixer2->sector(6);
  mEEmixer2->sector(7);
  mEEmixer2->trigger(45203);
  
  

  //  TFile *f=new TFile("test.root","recreate");
  //  TH1F *hNpoints=new TH1F("hNpoints","Number of points",10,0.,10.);
  

  mChain->ls(3);
  mChain->Init();

  //-- Loop over all events in the muDst --
  Int_t stat  = 0;
  Int_t count = 0;
  while ( stat == 0 ) {

    //-- Terminate once we reach nevents --
    if ( count++ >= nevents )
      if ( nevents > 0 ) break;

    //-- Call clear on all makers --
    mChain -> Clear();


    //-- Process the event through all makers --
    stat = mChain -> Make();

    if ( (count%100) ) continue;

    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;
    for ( int i = 0; i < 4; i++ ) {
      std::cout << " layer=" << i << " nhits=" << mEEmcA2E->numberOfHitTowers(i) << std::endl;
    }
        
	    
  }// loop over all events

  //-- Finish up --
  mChain -> Finish();
  //  f->Write();

  TString oo=ofile;
  oo.ReplaceAll("pi0tree","pi0hist");
  TFile *f=new TFile(oo,"recreate");
  f->cd();
  mEEmixer2->GetHistList()->Write();
  f->Write();
  f->Close();
  delete f;
  


  return;
    
}

void LoadLibs()
{
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");

  gSystem->Load("StEEmcBFC2005");

  gSystem->Load("StEEmcSimulatorMaker");

}



void testNeighborhood()
{

  for ( Int_t i = 0; i < 720; i++ ) {

    mEEmcA2E->tower(i).print();
    
    Int_t num= mEEmcA2E->tower(i).numberOfNeighbors();
    std::cout << "+ number of neighbors=" << num << std::endl;

    for ( Int_t j = 0; j < num; j++ ) {

      std::cout << "+ ";
      Int_t nn =mEEmcA2E->tower(i).neighborIndex(j);
      mEEmcA2E->tower(nn).print();

    }

  }

}
