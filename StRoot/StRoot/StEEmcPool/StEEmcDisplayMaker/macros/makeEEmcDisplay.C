//-- switch should be commented out when analysing real data
//#define MONTE_CARLO

class StChain;
class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;
class StEEmcA2EMaker;
class StEEmcGenericClusterMaker;
class StEEmcGenericPointMaker;
class StEEmcPointFitMaker;
class StEEmcMixMaker;
class StEEmcMixQAMaker;
class StSpinDbMaker;
class StEEmcPi0Maker;
class StEEmcDisplayMaker;

//--
//-- globals
//--
StChain            *mChain        = 0;
St_db_Maker        *mStarDatabase = 0;
StEEmcDb           *mEEmcDatabase = 0;
StMuDstMaker       *mMuDstMaker   = 0;
StEEmcA2EMaker     *mEEanalysis   = 0;
StEEmcGenericClusterMaker *mEEclusters   = 0;
StEEmcGenericPointMaker   *mEEpoints     = 0;

StEEmcPi0Maker *mEEpi0s = 0;
StEEmcDisplayMaker *mEEdisplay=0;

//StEEmcPointFitMaker *mEEpoints = 0;

//StEEmcMixMaker     *mEEmixer      = 0;
//StEEmcMixQAMaker   *mEEmixqa      = 0;

//StSpinDbMaker      *mSpinDb       = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 1; 

TFile *file = 0;

void makeEEmcDisplay( Int_t nevents = -1, 
		      Char_t *name="mc.list",
		      Char_t *ofile="mc.root",
		      Char_t *path = "", 
		      Int_t trigID=96261,
		      Int_t nfiles = 100
		      )
{


  TString pathname = path; 
  pathname += name;

  std::cout << "------------ blah ----------------" << std::endl;

  //--
  //-- Load shared libraries
  //--
  LoadLibs();
  
  //--
  //-- Create the analysis chain
  //--
  mChain = new StChain("eemcAnalysisChain");

  //--
  //-- MuDst maker for reading input
  //--
  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
  //mMuDstMaker->SetStatus("*",0);
  //mMuDstMaker->SetStatus("MuEvent",1);
  //mMuDstMaker->SetStatus("EmcAll",1);

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
  // -> setSectors(1,7); 

  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");



  //--
  //-- Initialize SPIN database
  //--
  mSpinDb = new StSpinDbMaker("mSpinDb");



#ifdef MONTE_CARLO
  //#ifdef BLAH
  //--
  //-- Initialize slow simulator
  //--
  StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
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
  mEEanalysis->threshold(3.0,4);            // smdu threshold
  mEEanalysis->threshold(3.0,5);            // smdv threshold
#ifdef MONTE_CARLO
  mEEanalysis->scale(1.2);                  // scale energies by x1.2
#endif
/*
  mEEanalysis->scale(1./1.56,4);            // smd-u
  mEEanalysis->scale(1./1.56,5);            // smd-v 
*/

 
  //--
  //-- Cluster maker.
  //--
  mEEclusters=new StMyClusterMaker("mEEclusters", mEEanalysis, mMuDstMaker );
  ((StMyClusterMaker*)mEEclusters)->setSmdSeedEnergy( 5.0/1000.0 );
  ((StMyClusterMaker*)mEEclusters)->setSmdMinimumEnergy(0.5/1000.0 );
  ((StMyClusterMaker*)mEEclusters)->setSmdMaximumSize(5);

  //  ((StMyClusterMaker*)mEEclusters)->setDoBreakTruncation(true);
  //  ((StMyClusterMaker*)mEEclusters)->setSmdTruncationRatio(1.20);

  ((StMyClusterMaker*)mEEclusters)->setSmdMinimumStrips(3);
  ((StMyClusterMaker*)mEEclusters)->setSeedEnergy(3.0);
  ((StMyClusterMaker*)mEEclusters)->setMinimumEnergy(0.5);
  ((StMyClusterMaker*)mEEclusters)->setFloor(0.20,3.5);

  //((StMyClusterMaker*)mEEclusters)->setEtaCut(1);
  //((StMyClusterMaker*)mEEclusters)->setPhiCut(1);


  mEEpoints=new StMyPointMaker("mEEpoints",mEEanalysis,mEEclusters);
  //  ((StMyPointMaker*)mEEpoints)->setSplit();
  //  ((StMyPointMaker*)mEEpoints)->setSplitMinimumET(6.0);
  ((StMyPointMaker*)mEEpoints)->setSmdMinFraction(0.00);

  /***
  mEEpi0s = new StEEmcPi0Maker("mEEpi0s",mEEanalysis,mEEclusters,mEEpoints);
  mEEpi0s->addTrigger(96261); 
  mEEpi0s->setCheckTrigger(true); 
****/ 

  mEEdisplay=new StEEmcDisplayMaker("mEEdisplay");
  mEEdisplay->adc2energy(mEEanalysis);
  mEEdisplay->clusters(mEEclusters);
  mEEdisplay->points(mEEpoints);
  mEEdisplay->addTrigger(96261);
  mEEdisplay->setCheckTrigger(true);


  // output file for histograms/ttrees
  file=new TFile(ofile,"RECREATE");
  //mEEpi0s->setFile(file);
  mEEdisplay->setFile(file);



  mChain->ls(3);
  mChain->Init();

  


  //-----------------------------------------------------------------
  //--
  //-- This is where the business happens.  We loop over all events.
  //-- when mChain -> Make() is called, ::Make() will be called on 
  //-- all of the makers created above.
  //--
  //


  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  while ( stat == 0 ) {


    printf("event=%d\n",count);

    //--
    //-- Terminate once we reach nevents --
    //--
    if ( count++ >= nevents ) if ( nevents > 0 ) break;

    //--
    //-- Call clear on all makers 
    //--
    mChain -> Clear();

    
    //--
    //-- Process the event through all makers 
    //--
    stat = mChain -> Make();
    //std::cout << "Number of pairs = " << mEEpi0s->numberOfPairs() << std::endl;

    //--
    //-- Set to printout on every 10th event
    //--
   // if ( (count%prescale) ) continue;

    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;

    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--
    Int_t nhits[]={0,0,0,0,0,0};
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
    Int_t ncl[6]={0,0,0,0,0,0};
    ncl[0]+=mEEclusters->numberOfClusters(0);
    ncl[1]+=mEEclusters->numberOfClusters(1);
    ncl[2]+=mEEclusters->numberOfClusters(2);
    ncl[3]+=mEEclusters->numberOfClusters(3);
    ncl[4]+=mEEclusters->numberOfClusters(4);
    ncl[5]+=mEEclusters->numberOfClusters(5);
  
    const Char_t *lay[]={"tower:","pre1: ", "pre2: ", "post: ", "smdu: ", "smdv: "};
    for ( Int_t i=0;i<6;i++ )
      {
	std::cout << lay[i] << " " << nhits[i] << " " << ncl[i] << std::endl;
      }

    std::cout << "number of points: " << mEEpoints -> numberOfPoints() << std::endl;
    // std::cout << "number of pairs:  " << mEEmixer -> numberOfCandidates() << std::endl;







	    
  }
  //--
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
  file->cd(); 
  //mEEpi0s->GetHistList()->Write(); 
  //mEEpi0s->GetHistList()->Print();
  mEEdisplay->tree()->Write();
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
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");
  gSystem->Load("StEEmcDisplayMaker");

  gSystem->Load("StSpinDbMaker");
}


