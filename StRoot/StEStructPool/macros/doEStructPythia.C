/************************************************************************
 * $Id: doEStructPythia.C,v 1.3 2004/06/25 03:14:56 porter Exp $
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


  char* evtCutFile[10];
  char* datadirs[10];
  char* cutdirs[10];

  if(nset>9)nset=9;
  for(int i=0;i<nset;i++){
    TString cf("PythiaCuts0");
    cf+=i+1;
    cf+=".txt";
    evtCutFile[i]=new char[strlen(cf.Data())+1];
    strcpy(evtCutFile[i],cf.Data());
    TString dd("data/0");
    dd+=i+1;
    datadirs[i]=new char[strlen(dd.Data())+1];
    strcpy(datadirs[i],dd.Data());
    TString cd("cuts/0");
    cd+=i+1;
    cutdirs[i]=new char[strlen(cd.Data())+1];
    strcpy(cutdirs[i],cd.Data());
  }
 

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
  gROOT->LoadMacro("getPythia.C");

  char* jobid=gSystem->Getenv("JOBID");
  int jobId=1;
  if(!jobid){
    jobId=2;
  } else {
    char* ptr=strstr(jobid,"_");
    if(ptr){
       ptr++;
       jobId=atoi(ptr);
    }  
  }
  TPythia6* pythia=getPythia(jobId);
  
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
  char* outputFile[10];
  StEStructEventCuts* ecuts[10];
  StEStructTrackCuts* tcuts[10];
  StPythiaTest*  analysis[10];
  StEStructPythia* readers[10];


  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool skipMake[10];
  skipMake[0]=false;
  for(int i=1;i<10;i++)skipMake[i]=true;
 

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
 * Revision 1.3  2004/06/25 03:14:56  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
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


























































