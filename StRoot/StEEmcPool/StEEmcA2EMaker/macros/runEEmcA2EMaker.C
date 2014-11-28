//--
//-- runEEmcA2EMaker.C
//--
//-- example script for running the adc --> energy maker.  The macro
//-- will process the first nevents of the MuDst (or list of MuDst's).
//-- if nevents is negative, the macro will process all events in the
//-- mudst.
//--

//-- switch should be commented out when analysing real data
//#define MONTE_CARLO

class StChain;
class St_db_Maker;
class StMuDstMaker;
class StEEmcDb;
class StEEmcA2EMaker; 

//--
//-- globals
//--
StChain        *mChain        = 0;
St_db_Maker    *mStarDatabase = 0;
StEEmcDb       *mEEmcDatabase = 0;
StMuDstMaker   *mMuDstMaker   = 0;
StEEmcA2EMaker *mEEanalysis   = 0;

Int_t           count         = 0;
Int_t           stat          = 0; 

void runEEmcA2EMaker( Int_t nevents = 50, 
		      Char_t *name = "mcpi0_5000_06TC05_3.MuDst.root",
		      Char_t *ofile= "test.root",
		      Char_t *path = "./test/", 
		      Int_t nfiles = 100
		      )
{

  TString pathname = path; 
  pathname += name;

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
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);


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



#ifdef MONTE_CARLO
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
//mEEanalysis->source("StEvent",2);         // sets StEvent as input
//mEEanalysis->threshold(3.0,0);            // tower threshold, adc > 3.0*sigma + ped 
//mEEanalysis->threshold(3.0,1);            // pre1 threshold, adc > 3.0*sigma + ped 
//mEEanalysis->threshold(3.0,2);            // pre2 threshold, adc > 3.0*sigma + ped 
//mEEanalysis->threshold(3.0,3);            // post threshold, adc > 3.0*sigma + ped 
//mEEanalysis->threshold(3.0,4);            // smdu threshold, adc > 3.0*sigma + ped 
//mEEanalysis->threshold(3.0,5);            // smdv threshold, adc > 3.0*sigma + ped 
#ifdef MONTE_CARLO
  mEEanalysis->scale(1.2);                  // scale energies by x1.2
#endif 

  mChain->ls(3);
  mChain->Init();

  //-----------------------------------------------------------------
  //--
  //-- This is where the business happens.  We loop over all events.
  //-- when mChain -> Make() is called, ::Make() will be called on 
  //-- all of the makers created above.
  //--

  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  while ( stat == 0 ) {


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

    //--
    //-- Set to printout on every 10th event
    //--
    if ( (count%10) ) continue;

    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;

    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--
    for ( int i = 0; i < 4; i++ ) {
      std::cout << " layer=" << i 
		<< " nhits=" << mEEanalysis->numberOfHitTowers(i) << std::endl;
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
    std::cout << " layer=u nhits=" << nu << std::endl;
    std::cout << " layer=v nhits=" << nv << std::endl;
	    
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
  //TFile *file=new TFile(ofile,"RECREATE");
  //file->mkdir("QA");
  //file->cd("QA");
  //eemcQA -> GetHistList() -> Write();
  //file -> Close();
  //delete file;


  return;
    
}

void LoadLibs()
{
  gSystem->Load("libMinuit");
  //-- Load muDst shared libraries --
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
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

}

