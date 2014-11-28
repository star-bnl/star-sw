/************************************************************************
 * $Id: doEStruct2ptDst.C,v 1.3 2004/06/25 03:14:56 porter Exp $
 *
 * Author: Jeff Porter 
 *
 *  example code for reading in EStruct Dst files contained in "fileListFile"
 *  and running the 2pt correlations analysis, producing hist files in,
 *
 *  outputDir/jobName/data/XX/
 *  
 *    where XX = event level selection imposed by the cut files.
 *               This example (from PP) contains 3 selections on event mult
 *
 *************************************************************************/
void doEStruct2ptDst(const char* fileListFile, const char* outputDir, const char* cutFile, const char* jobName=0, int nset=3, int maxNumEvents=0){

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C");
  gROOT->LoadMacro("support.C");

  // in this example, all cuts (event, track, pair) are in same file
  char* evtCutFile=cutFile;
  char* trackCutFile=evtCutFile;
  char* pairCutFile =evtCutFile;

  char** datadirs=getDirNames("data",nset);
  char** cutdirs=getDirNames("cuts",nset);

  // simple (global) centrality definition ...not persistant to event file.. 
  // and not used in this particular example
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[4]={0,4,7,50};
  cent->setCentralities(temp,4);

  // create the low-level reader (here for EStructDst)
  StEStructEventMaker* mk = new StEStructEventMaker();
  mk->setInputFileList(fileListFile); 
  
  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the EStruct data Readers and Writer codes
  char** outputFile                    = new char*[nset];
  StEStructEventCuts** ecuts           = new StEStructEventCuts*[nset];
  StEStructTrackCuts** tcuts           = new StEStructTrackCuts*[nset];
  StEStruct2ptCorrelations**  analysis = new StEStruct2ptCorrelations*[nset];
  StEStructDstReader** readers         = new StEStructDstReader*[nset];

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool* skipMake=new bool[nset];
  skipMake[0]=false;
  for(int i=1;i<nset;i++)skipMake[i]=true;

  // build the NSEt readers & NSET analysis objects
  //  analysis = analysis interface & contains pair-cut object (needs file)
  //  reader = reader interface + pointer to StMuDstMaker + cut classes

  for(int i=0;i<nset;i++){

     outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
     analysis[i]=new StEStruct2ptCorrelations(1);
     analysis[i]->setCutFile(pairCutFile);
     analysis[i]->setOutputFileName(outputFile[i]);

     /* here's the new way to set the numTracks cut */
      ecuts[i]=new StEStructEventCuts(evtCutFile);
      int min=floor(cent->minCentrality(i));
      int max=floor(cent->maxCentrality(i));
      char** tmp=getNumTracksStrings(min,max);  // find in getOutFileName.C
      ecuts[i]->loadBaseCuts("numTracks",tmp,2);

     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     readers[i]=new StEStructDstReader(mk,ecuts[i],tcuts[i],skipMake[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  // --- now do the work ---
  doTheWork(estructMaker,maxNumEvents);

  // write event-stats to log file 
  char* statsFileName=getOutFileName(outputDir,jobName,"stats");
  ofstream ofs(statsFileName);

  ofs<<endl;
  estructMaker->logAnalysisTime(ofs);
  estructMaker->logInputEvents(ofs);
  estructMaker->logOutputEvents(ofs);
  estructMaker->logOutputRate(ofs);
  estructMaker->logAnalysisStats(ofs);

  // 
  // --> cuts stats streamed to stdout (logfile)
  //
  for(int i=0;i<nset;i++){
    ofs<<"  *************** ";
    ofs<<" Cut Stats for Analysis Number = "<<i;
    ofs<<"  *************** "<<endl;
     ecuts[i]->printCuts(ofs);
     tcuts[i]->printCuts(ofs);
     StEStructPairCuts& pcuts=analysis[i]->getPairCuts();
     pcuts.printCuts(ofs);

     // 
     // --> root cut file 
     // 
     char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     pcuts.writeCutHists(tf);
     tf->Close();
  }
  ofs.close();

  // --- write out the data
  estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStruct2ptDst.C,v $
 * Revision 1.3  2004/06/25 03:14:56  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 * Revision 1.2  2004/04/15 18:46:32  msd
 * Updated centrality variable types
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


























































