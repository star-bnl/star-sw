/******************************************************************************
 **
 ** runEEmcTiming.C
 **
 ** Arguements:
 **
 ** nevents - number of events to process
 ** name    - filename, format = <run>.list, eg 8081048.list, containing
 **           a list of MuDst's from a single run
 ** ofile   - output histogram filename, suggested <run>.root.  Output
 **           will be saved in subdirectory timing_files/
 **
 ** tower_delay - the TCD phase delay (towers) for this run 
 ** mapmt_delay - the TCD phase delay (mapmt) for this run
 ** 
 **/

class StChain;
class St_db_Maker;
class StEEmcDb;
class StMuDstMaker;
class StEEmcA2EMaker; 
class StEEmcTimingMaker;

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

Int_t nzeros = 0;
Int_t max_zero_count = 100;


void runEEmcL2Timing( Int_t nevents = 30000,
		      Char_t *name = "8327013.list",
		      Char_t *ofile= "8327013.root", 
		      Float_t tower_delay=19.,
		      Float_t mapmt_delay=65.,
		      Int_t nfiles = 1000
		    )
{

  Char_t *path = "./";
  TString pathname = path; 
  pathname += name;

  //--
  //-- Load shared libraries
  //--
  LoadLibs();
  gMessMgr -> SwitchOn("I");

  gSystem->mkdir("timing_files/");
  TString myofile="timing_files/";
  myofile += ofile;
  
  TFile *out=new TFile(ofile,"recreate");  
  StEEmcTimingMaker *timing=new StEEmcTimingMaker("timing");
  timing->setRunNumber( atoi( name ) );
  timing->setTiming( tower_delay, mapmt_delay );
  timing->setTowerCuts( 25, 75  );
  timing->setMapmtCuts( 50, 150 );
  timing->setOutputFile( myofile );

  timing->addTowerMask( 2, 98 ); // crate 2 ch 98 looks hot


  ifstream input(name);
  Char_t buf[256];
  input >> buf;

  timing->processFromL2(buf,nevents);
  timing->Finish();

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
  gSystem->Load("StEEmcTimingMaker");

}

