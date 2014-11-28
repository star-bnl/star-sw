/************************************************************************
 * $Id: doEStruct2ptAuAu.C,v 1.2 2006/04/26 18:52:50 dkettler Exp $
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
void doEStruct2ptAuAu(const char* fileListFile, const char* outputDir, const char* cutFile, const char* jobName=0, int cutBinMode=0, int maxNumEvents=0){

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
  // cutFile can also refer to an entry in the cut DB 
  //    (see StEStructPool/AnalysisMaker/StEStructCuts.cxx for pre-compiled cuts)
  const char* evtCutFile=cutFile;

  const char* trackCutFile=evtCutFile;
  const char* pairCutFile =evtCutFile;

  
  // simple (global) centrality definition ...not persistant to event file.. 
  // and not used in this particular example
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[6]={4,30,100,200,400,2000}; //=5 centralies
  cent->setCentralities(temp,6);
  int nset=cent->numCentralities()-1;

  char** datadirs=getDirNames("data",nset);
  char** cutdirs=getDirNames("cuts",nset);

  // choose the mode for the binning
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(cutBinMode);

  // create the low-level reader (here for MuDst)
  StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",5000);   

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the EStruct data Readers and Writer codes
  char** outputFile                    = new char*[nset];
  StEStructEventCuts** ecuts           = new StEStructEventCuts*[nset];
  StEStructTrackCuts** tcuts           = new StEStructTrackCuts*[nset];
  StEStruct2ptCorrelations**  analysis = new StEStruct2ptCorrelations*[nset];
  StEStructMuDstReader** readers       = new StEStructMuDstReader*[nset];

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool* skipMake=new bool[nset];
  skipMake[0]=false;
  for(int i=1;i<nset;i++)skipMake[i]=true;

  //  build the NSET readers & NSET analysis objects
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
     readers[i]=new StEStructMuDstReader(mk,ecuts[i],tcuts[i],skipMake[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  // --- now do the work ---
  //estructMaker->SetReactionPlaneAnalysis("/home/dkettler/working/weights/PhiWgt.root");
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

  // --- write out the data 
  estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStruct2ptAuAu.C,v $
 * Revision 1.2  2006/04/26 18:52:50  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Added reaction plane angle calculation
 *
 * Case 3 in buildPtChargeTypes needs to be corrected
 *
 * Flag added for enabling reaction plane analysis
 *
 * Revision 1.1  2005/03/03 18:30:59  porter
 * example correlations macro and scheduler xml
 *
 * Revision 1.6  2004/08/23 19:12:46  msd
 * Added note about usage for pre-compiled cut database
 *
 * Revision 1.5  2004/06/26 16:28:42  porter
 * fixed typo in getDirNames.C
 *
 * Revision 1.4  2004/06/25 03:14:55  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 * Revision 1.3  2004/04/15 18:46:31  msd
 * Updated centrality variable types
 *
 * Revision 1.2  2003/11/21 06:26:40  porter
 * macros for running pythia
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


























































