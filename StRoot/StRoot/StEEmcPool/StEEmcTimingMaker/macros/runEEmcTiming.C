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


void runEEmcTiming( Int_t nevents = 30000,
		    Char_t *name = "8095104.list",
		    Char_t *ofile= "8095104.root", 
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

 

  //--
  //-- Initialize EEMC database
  //--
  new StEEmcDbMaker("eemcdb");
  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOn("I");


 

  //--
  //-- Energy to ADC maker
  //--
  mEEanalysis=new StEEmcA2EMaker("AandE");
  mEEanalysis->database("eemcdb");          // sets db connection
  mEEanalysis->source("MuDst",1);           // sets mudst as input
  // set a negative threshold for each channel 
  for ( int ii=0;ii<6;ii++ ) mEEanalysis->threshold(-300.0,ii);


  
  StEEmcTimingMaker *timing=new StEEmcTimingMaker("timing");
  timing->setRunNumber( atoi( name ) );
  timing->setTiming( tower_delay, mapmt_delay );
  timing->setTowerCuts( 25, 75  );
  timing->setMapmtCuts( 50, 150 );

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
    //-- Check that EEMC data are valid and terminate if not
    //--
    Int_t sum=0;
    for ( Int_t i=0;i<720;i++ )
      {
	StEEmcTower t=mEEanalysis->tower(i,0);
	sum+=t.raw();
      }    
    if (sum==0)nzeros++;
    if ( nzeros > max_zero_count ) {
      std::cout << "ADC sum for EEMC zero for > max events" << std::endl;
      break;
    }

    //--
    //-- Set to printout on every 10th event
    //--
        if ( (count%100) ) continue;


    std::cout << "------------------------------------------------";
    std::cout << "event=" << count << std::endl;

    //--
    //-- Print the number of hits in the towers, pre/postshower layers
    //--

    for ( Int_t i=0;i<720;i++ )
      {
	StEEmcTower t=mEEanalysis->tower(i,0);
	std::cout << t.raw() << " ";
	if ( !((i+1)%24) ) std::cout << std::endl;
      }    
    std::cout << std::endl;


	    
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

  TString psfile=ofile;psfile.ReplaceAll("root","ps");
  TString dtfile=ofile;dtfile.ReplaceAll("root","dat");

//  timing->dumpPDF( psfile );
  timing->dumpAsciiFile(dtfile);

  //--
  //-- Output the QA histograms to disk
  //--
  gSystem->mkdir("timing_files/");
  TString myofile="timing_files/";
  TFile *file=new TFile(myofile+ofile,"RECREATE");
  timing->GetHistList()->Write();
  file -> Close();
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
  gSystem->Load("StEEmcTimingMaker");

}

