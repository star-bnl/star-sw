/************************************************************************
 * $Id: doEStruct2ptPythia.C,v 1.4 2004/07/01 00:39:24 porter Exp $
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
void doEStruct2ptPythia(const char* fileListFile, const char* outputDir, const char* cutFile, const char* jobName=0, int nset=3, int maxNumEvents=0){

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C");
  gROOT->LoadMacro("support.C");

  /* *******************************************************************
     changed the use model of the cut file. I found that I always keep 
     the cuts for a given pass the same except for the centrality cut. 
     Now I add this cut directly and I use only 1 cut file, PPCutsAll.txt, 
     and with the centrality cut set below
  ************************************************************************/

  // in this example, all cuts (event, track, pair) are in same file
  const char* evtCutFile=cutFile;

  const char* trackCutFile=evtCutFile;
  const char* pairCutFile =evtCutFile;

  char** datadirs=getDirNames("data",nset);
  char** cutdirs=getDirNames("cuts",nset);
  
  gSystem->Load("libEG");
  gSystem->Load("libEGPythia6");
  gSystem->Load("libPythia6");    
  gSystem->Load("StEStructPoolEventGenerators.so");
  gROOT->LoadMacro("getPythia.C");

  char* jobid=gSystem->Getenv("JOBID");
  int jobId=1;
  TPythia6* pythia=getPythia(jobId,jobid); 

  // simple (global) centrality definition ...not persistant to event file.. 
  // and not used in this particular example
  // For MC events the centrality is set based on impact parameter.
  // For data one can use multiplicity, but this is not set in
  // the reader code.
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[4]={2,5,9,99};
  cent->setCentralities(temp,4);

  // choose the mode for the binning
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(3);
  
  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2Pythia");

  // Set up the EStruct data Readers and Writer codes
  char** outputFile                    = new char*[nset];
  StEStructEventCuts** ecuts           = new StEStructEventCuts*[nset];
  StEStructTrackCuts** tcuts           = new StEStructTrackCuts*[nset];
  StEStruct2ptCorrelations**  analysis = new StEStruct2ptCorrelations*[nset];
  StEStructPythia** readers            = new StEStructPythia*[nset];

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool* skipMake=new bool[nset];
  skipMake[0]=false;
  for(int i=1;i<nset;i++)skipMake[i]=true;

  //  build the 3 readers & 3 analysis objects
  //  analysis = analysis interface & contains pair-cut object (needs file)
  //  reader = reader interface + pointer to StMuDstMaker + cut classes

  for(int i=0;i<nset;i++){

     outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
     analysis[i]=new StEStruct2ptCorrelations();
     analysis[i]->setCutFile(pairCutFile);
     analysis[i]->setOutputFileName(outputFile[i]);

     /* here's the new way to set the numTracks cut */
      ecuts[i]=new StEStructEventCuts(evtCutFile);
      int min=floor(cent->minCentrality(i));
      int max=floor(cent->maxCentrality(i));
      char** tmp=getNumTracksStrings(min,max);  // find in support.C
      ecuts[i]->loadBaseCuts("numTracks",tmp,2);
 
     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     readers[i]=new StEStructPythia(maxNumEvents,pythia,ecuts[i],tcuts[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  // --- now do the work ---
  doTheWork(estructMaker,maxNumEvents);

  //--- now write out stats and cuts ---
  char* statsFileName=getOutFileName(outputDir,jobName,"stats");
  ofstream ofs(statsFileName);

  ofs<<endl;
  estructMaker->logAnalysisTime(ofs);
  estructMaker->logInputEvents(ofs);
  estructMaker->logOutputEvents(ofs);
  estructMaker->logOutputRate(ofs);
  estructMaker->logAnalysisStats(ofs);

  for(int i=0;i<nset;i++){
    ofs<<"  *************** ";
    ofs<<" Cut Stats for Analysis Number = "<<i;
    ofs<<"  *************** "<<endl;
     ecuts[i]->printCuts(ofs);
     tcuts[i]->printCuts(ofs);

     StEStructPairCuts* pcuts=NULL;
     if(analysis){
        pcuts=&analysis[i]->getPairCuts();
        pcuts->printCuts(ofs);
     }

     // --> root cut file 
     char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     if(pcuts)pcuts->writeCutHists(tf);
     tf->Close();

  }
  ofs.close();

  estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStruct2ptPythia.C,v $
 * Revision 1.4  2004/07/01 00:39:24  porter
 * added 'libPhysics.so' to load2ptLibs.C which allows our codes to run
 * in root.exe instead of root4star. Thus we can use TPythia6.
 *
 * added a rewritten version of my makePlots.C with example.
 * Also, 'selectASNS.C' is an example for using the StEStructSupport code.
 *
 * Revision 1.3  2004/06/25 03:14:56  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 * Revision 1.2  2004/03/02 21:51:11  prindle
 *
 *   I forgot to cvs add my EventGenerator readers.
 *
 * Revision 1.1  2003/11/21 06:26:40  porter
 * macros for running pythia
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


























































