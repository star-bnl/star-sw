#include <TSystem.h>

int runqa( Int_t runnumber = 14032027, Int_t ped=0, Int_t nevents = 9999999, int zs=1,
	   const Char_t *evpdir = "/evp/a/",
	   Float_t chargrms=1.0, Float_t thr=4.0, Float_t thr2add=3.0, 
	   Bool_t useSeed5=true, Bool_t cutShortEvents = 0){
  
  int day=runnumber/1000;

  LoadLibs();   
  Int_t ierr = 0;
  
  cout << "Constructing the chain" << endl;
  StChain* analysisChain = new StChain("fgtQAChain");
  
  TString dir0 = "MySQL:StarDb";
  TString dir1 = "$STAR/StarDb";
  St_db_Maker *dbMkr = new St_db_Maker( "dbMkr", dir0, dir1 );  
  
  cout << "Constructing StFgtDbMaker" << endl;
  fgtDbMkr = new StFgtDbMaker();
  //dbMkr->SetDateTime(20120401,000000); //run2012???
  
  cout << "Constructing the FGT raw daq reader" << endl;
  char filename[200]; 
  sprintf(filename,"%s/%d",evpdir,runnumber);
  //sprintf(filename,"%s/%d_DELETE",evpdir,runnumber);
  daqRdr = new StFgtRawDaqReader( "daqReader", filename);
  daqRdr->setIsCosmic( false );
  daqRdr->cutShortEvents( cutShortEvents );
  //daqRdr->setStartTbin(1);
  //daqRdr->setNumTbin(8);
  if(zs>0){daqRdr->setZSdataOnly();}
  
  if(ped==1) {
    daqRdr->setNoneZSdataOnly();
    cout << "Loading and Constructing the StFgtPedMaker" << endl;
    gSystem->Load("StFgtPedMaker");
    StFgtRobustPedMaker* pedMkr  = new StFgtRobustPedMaker();
    pedMkr->setFgtDbMkrName("fgtDb");
    pedMkr->setTimeBinMask(0x0);
    pedMkr->setNumBins(4200);
    pedMkr->setMaxAdc(4200);
    pedMkr->setNumSmooth(0);
    sprintf(filename,"%d/ped/ped.%d.txt",day,runnumber);
    pedMkr->setToSaveToFile(filename);

    cout << "Loading and Constructing the Status Maker" << endl;
    gSystem->Load("StFgtStatusMaker");
    StFgtStatusMaker *statMkr = new StFgtStatusMaker( "FgtStatusMaker", "FgtPedMaker" );
    sprintf(filename,"%d/status/status.%d.txt",day,runnumber);
    statMkr->setToSaveToFile(filename);
    statMkr->setTimeBin(0);
    statMkr->setPedRange(200,1000);
    statMkr->setRmsRange(15,200);
    statMkr->setFracRange(0.0,1.0);
    statMkr->setMaxDeadPerApv(128);
  }else{
    cout << "Loading and Constructing the StFgtA2CMaker" << endl;
    gSystem->Load("StFgtA2CMaker");
    StFgtA2CMaker* a2cMkr  = new StFgtA2CMaker(  "FgtA2CMaker" );
    a2cMkr->setFgtDb(fgtDbMkr->getDbTables());
    a2cMkr ->setAbsThres( -5000 );  // set to below -4096 to skip cut
    //    a2cMkr ->setAbsThres( 300 );  // set to below -4096 to skip cut
    //a2cMkr ->setRelThres( 3.);  // set to zero to skip cut
    a2cMkr ->setRelThres(thr);  // set to zero to skip cut
    //    a2cMkr->doCutBadStatus(true);//parameter is useless from looking at the function
    a2cMkr->doCutBadStatus();
    a2cMkr->acceptLongPulses(true);
    ////you have to set the relative threshold to 3 if you set the cluster threshold to 0.6 (meaning 3)
    //a2cMkr->setClusterThreshold(0.6);  
    a2cMkr->setClusterThreshold(thr/5.0);  
    a2cMkr->setPedSigFactor4Charge(chargrms);
    a2cMkr->useLeastRestrictiveSeed(useSeed5);
    ///this cuts ~10% of the events
    //   a2cMkr->doRemoveNonSignal(false);
    //   a2cMkr->doRemoveNonPulse(false);
    a2cMkr->setPedestalFile("ped.txt");
    a2cMkr->setStatusFile("status.txt");
    
    gSystem->Load("StFgtClusterMaker");
    Char_t *myMaker = "StFgtClusterMaker";
    StFgtClusterMaker* clusterMk =new StFgtClusterMaker("FgtClustMaker"); 
    clusterMk->SetDEBUG();
    //  simpleClusAlgo = new StFgtSimpleClusterAlgo();
    seededClusAlgo = new StFgtSeededClusterAlgo();
    seededClusAlgo->setJumpSingleStrip(true); // if a strip in cluster has no charge 
    seededClusAlgo->setThreshold2AddStrip(thr2add); //threshold to add strips to cluster 
    clusterMk->setClusterAlgo( seededClusAlgo );
    
    gSystem->Load("StFgtPointMaker");
    StFgtPointMaker* pointMk =new StFgtPointMaker();
    StFgtSimplePointAlgo * simplePointAlgo = new StFgtSimplePointAlgo();    
    pointMk->setPointAlgo( simplePointAlgo );
    simplePointAlgo->setMaxChargeAsym(0.2);

    gSystem->Load("StFgtClusterTools");
    fgtGenBase=new StFgtGeneralBase("fgtGenBase");
    fgtGenBase->fillFromEvent();
    fgtStraightTrackMaker =new StFgtStraightTrackMaker("fgtStraightTracker");
    fgtStraightTrackMaker->setMinNumFitPoints(3);
    fgtStraightTrackMaker->setMaxClusters(30);
    //fgtStraightPlotter=new StFgtStraightPlotter("fgtStraightPlotter");
    
    gSystem->Load("libMinuit");
    gSystem->Load("StFgtAlignmentMaker");
    StFgtAlignmentMaker* algMk =new StFgtAlignmentMaker();
    algMk->setTrackType(0);
    algMk->setDataSource(2);
    algMk->setRunNumber(runnumber);
    sprintf(filename,"%d/alignment_trkout.%d.root",day,runnumber);
    algMk->setWriteTree(filename);

    gSystem->Load("StFgtQAMaker");
    StFgtQAMaker* qaMkr =new StFgtQAMaker();
    qaMkr->setRunNumber(runnumber);
  }
   
  cout << "Initializing" << endl;
  ierr = analysisChain->Init();
  
  if( ierr ){
    cout << "Error initializing" << endl;
    return;
  };
  
  if( nevents < 0 )
    nevents = 1<<30; // a big number
  
  cout << "max nevents = " << nevents << endl;
  for( int i=0; i<nevents && !ierr; ++i ){
    if( i+1 % 100 == 1 ) cout << "\t on event number **************" << i << endl;
    //cout << "clear (agv)" << endl;
    analysisChain->Clear();
    //cout << "make" << endl;
    ierr = analysisChain->Make();
    //cout <<" done " <<endl;
  };
  
  //   fgtDbMkr->printFgtDumpCSV("fgtMapDump.csv");
  //
  // Calls the ::Finish() method on all makers
  //
  cout << "finish" << endl;
  analysisChain->Finish();
  
  // Now write a status table
  
  /*
    if( runnumber ){
    std::stringstream ss;
    fout << "Times given in the run log are " << endl;
    ss << "lynx -dump 'http://online.star.bnl.gov/RunLogRun12/index.php?r=" << runnumber << "' | grep GMT";
    FILE *f = gSystem->OpenPipe(ss.str().data(),"r");
    Char_t c;
    while((c=fgetc(f))!=EOF)
    fout << c;
    };
  */
  
  bool doOutputPdf=false;
  // convert ps to pdf
  if( doOutputPdf ){
    cout << "converting ps to pdf" << endl;
    gSystem->Exec(( std::string("ps2pdf -dAutoRotatePages=/None ") + pdfFile ).data());
  };
  
  cout << "\tall done" << endl;
  return;
};


// load the shared libraries
void LoadLibs() {
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbBroker");
  gSystem->Load("RTS");
  gSystem->Load("StFgtUtil");
  gSystem->Load("StFgtDbMaker");
  gSystem->Load("StFgtRawDaqReader");
};
