/************************************************************************
 * $Id: doEStructPythia.C,v 1.2 2004/04/15 18:46:33 msd Exp $
 *
 * Author: Jeff Porter 
 *
 *  example code for reading in MuDst files contained int "fileListFile"
 *  and running the 2pt correlations analysis, producing hist files in,
 *
 *  outputDir/jobName/data/XX/
 *  
 *    where XX = event level selection imposed by the cut files.
 *               This example (from PP) contains 3 selections on event mult
 *
 *************************************************************************/
void doEStructPythia(const char* fileListFile, const char* outputDir, const char* jobName=0, int nset=3, int maxNumEvents=0){

  // input cut files presumed to be in cwd or use full path 
  char* evtCutFile[]={"PythiaCuts01.txt","PythiaCuts02.txt","PythiaCuts03.txt"};

  // data and cut directories sub to outputDir/jobName 
  char* datadirs[]  ={"data/01","data/02","data/03"};
  char* cutdirs[]   ={"cuts/01","cuts/02","cuts/03"}; 

  // in this example, all cuts (event, track, pair) are in same file
  char* trackCutFile=evtCutFile[0];
  char* pairCutFile =evtCutFile[0];

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  // pythia stuff
  gSystem->Load("libEG");
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");    
  gSystem->Load("StEStructPoolEventGenerators.so");

  gROOT->LoadMacro("getOutFileName.C");
  
  // simple (global) centrality definition ...not persistant to event file.. 
  // and not used in this particular example
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[4]={0,4,7,50};
  cent->setCentralities(temp,4);

  // create the low-level reader (here for MuDst)
  //  StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",500); 
  
  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2Pythia");

  // Set up the EStruct data Readers and Writer codes
  char* outputFile[3];
  StEStructEventCuts* ecuts[3];
  StEStructTrackCuts* tcuts[3];
  StPythiaTest*  analysis[3];
  StEStructPythia* readers[3];

  TPythia6* pythia=new TPythia6();
  pythia->Initialize("CMS","p","p",200.);

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool skipMake[3];
  skipMake[0]=false;
  skipMake[1]=true;
  skipMake[2]=true;


  //  build the 3 readers & 3 analysis objects
  //  analysis = analysis interface & contains pair-cut object (needs file)
  //  reader = reader interface + pointer to StMuDstMaker + cut classes

  for(int i=0;i<nset;i++){

     outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
     analysis[i]=new StPythiaTest();
     //     analysis[i]->setCutFile(pairCutFile);
     analysis[i]->setOutputFileName(outputFile[i]);

     ecuts[i]=new StEStructEventCuts(evtCutFile[i]);
     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     readers[i]=new StEStructPythia(1000,pythia,ecuts[i],tcuts[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  //
  // --- now do the work ---
  //

    int istat=0,i=1;
    estructMaker->Init();
    estructMaker->startTimer();

    int counter=0;
  while(istat!=2){

      istat=estructMaker->Make();
      i++; counter++;
      if(counter==200){ 
        cout<<"doing event ="<<i<<endl;
        counter=0;
      }
      if( maxNumEvents!=0 && i>=maxNumEvents )istat=2;
   }
  estructMaker->stopTimer();

  // write event-stats to log file 
  cout<<endl;
  estructMaker->logAnalysisTime(cout);
  estructMaker->logInputEvents(cout);
  estructMaker->logOutputEvents(cout);
  estructMaker->logOutputRate(cout);

  // 
  // --> cuts stats streamed to stdout (logfile)
  //
  for(int i=0;i<nset;i++){
     ecuts[i]->printCuts(cout);
     tcuts[i]->printCuts(cout);
     //     StEStructPairCuts& pcuts=analysis[i]->getPairCuts();
     //     pcuts.printCuts(cout);

     // 
     // --> root cut file 
     // 
     char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     //     pcuts.writeCutHists(tf);
     tf->Close();
  }


  estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStructPythia.C,v $
 * Revision 1.2  2004/04/15 18:46:33  msd
 * Updated centrality variable types
 *
 * Revision 1.1  2003/11/21 06:26:40  porter
 * macros for running pythia
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


























































