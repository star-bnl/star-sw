/************************************************************************
 * $Id: writeEStructExample.C,v 1.1 2003/10/15 18:20:57 porter Exp $
 *
 * Author: Jeff Porter 
 *
 *  example code for reading in MuDst files contained int "fileListFile"
 *  and writing *.estruct.root files to 
 *
 *  outputDir/jobName/event/XX/
 *  
 *    where XX = event level selection imposed by the cut files.
 *               This example (from PP) contains 3 selections on event mult
 *
 *************************************************************************/

void writeEStructExample(const char* fileListFile, const char* outputDir, const char* jobName=0, int nset=3, int maxNumEvents=0){

  // input cut files presumed to be in cwd or use full path 
  char* evtCutFile[]={"PPCuts01.txt","PPCuts02.txt","PPCuts03.txt"};

  // data and cut directories sub to outputDir/jobName 
  char* datadirs[]  ={"event/01","event/02","event/03"};
  char* cutdirs[]   ={"cuts/01","cuts/02","cuts/03"}; 

  // in this example, all cuts (event, track ) are in same file
  char* trackCutFile=evtCutFile[0];  

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C");


  // simple (global) centrality definition ...not persistant to event file.. 
  // and not used in this particular example
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const int temp[4]={0,4,7,50};
  cent->setCentralities(temp,4);


  // create the low-level reader (here for MuDst)
  StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",500); 

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the EStruct data Readers and Writer codes

  char* outputFile[3];
  StEStructEventCuts* ecuts[3];
  StEStructTrackCuts* tcuts[3];
  StEStructEventWriter*  analysis[3];
  StEStructEventMaker*   writeMakers[3];
  StEStructMuDstReader* readers[3];

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool skipMake[3]; 
  skipMake[0]=false;
  skipMake[1]=true;
  skipMake[2]=true;

  // just to keep root happy, we'll give each writer maker a diff. name
  char* makerNames[]={"EStructEventMaker01","EStructEventMaker02","EStructEventMaker03"};

  // build the 3 readers & 3 writers.
  //  writer = analysis interface with a pointer to a StEStructEventMaker
  //  reader = reader interface + pointer to StMuDstMaker + cut classes
  for(int i=0;i<nset;i++){

     writeMakers[i]=new StEStructEventMaker(makerNames[i]);
     outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
     analysis[i]=new StEStructEventWriter(writeMakers[i]);
     analysis[i]->setOutputFileName(outputFile[i]);

     ecuts[i]=new StEStructEventCuts(evtCutFile[i]);
     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     readers[i]=new StEStructMuDstReader(mk,ecuts[i],tcuts[i],skipMake[i]);
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

  // write stats to log file 
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

     //
     // --> root cut file 
     // 
     char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     tf->Close();
  }

  estructMaker->Finish();
}


/**********************************************************************
 *
 * $Log: writeEStructExample.C,v $
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

























































