/************************************************************************
 * $Id: doEStructFluct.C,v 1.1 2006/04/04 22:15:56 porter Exp $
 *
 * Author: Duncan Prindle & Jeff Porter 
 *
 *  example code for reading in MuDst files and running 
 *  the estruct fluctuations analysis, producing hist files....
 *************************************************************************/

void doEStruct( const char* filelist,
                const char* outputDir,
                const char* scriptDir,
                int maxNumEvents = 0 ) {


    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();

    char cutFile[1024];
    sprintf(cutFile,"%s/CutsFile.txt",scriptDir);

    gROOT->LoadMacro("getOutFileName.C");
    const char* scratchDir = "PPFluct";

  // define centrality
    StEStructCentrality* cent=StEStructCentrality::Instance();
    const double mbBins[] = {5, 24, 69, 156, 297, 513, 911};
    int mbNBins = 1+1+1+1+1+1+1;
    cent->setCentralities(mbBins,mbNBins);

    const  double ptCut[] = {0.15, 0.5, 2.0};
    int mbNPts = 1+1+1;

    const  double ptMultCut[] = {0.0, 6.0, 8.6, 10.6, 12.4, 14.0, 20.0};
    int mbNPtBins = 1+1+1+1+1+1+1;;
    cent->setPts(ptCut,mbNPts,ptMultCut,mbNPtBins);

    int numberOfAnalyses=1;

    char *analysisType = "StEStructFluctuation";

    // Need an EStruct maker.
    StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker(analysisType);
    
    //  reader = reader interface + pointer to Data Maker + cut classes
    //   char fileListFile[1024];
    // sprintf(fileListFile,"%s/%s",scriptDir,filelist);
    StMuDstMaker* mk = new StMuDstMaker(0,0,"",filelist,".",500);
   // Set up the reader with event/track cuts
    StEStructEventCuts*    ecuts = new StEStructEventCuts(cutFile);
    StEStructTrackCuts*    tcuts = new StEStructTrackCuts(cutFile);
    StEStructMuDstReader* reader = new StEStructMuDstReader(mk,ecuts,tcuts,false);
    estructMaker->SetEventReader(reader);

    StEStructFluctAnal** analysis = new StEStructFluctAnal*[numberOfAnalyses];
    StEStructFluctAnal*  currentAnalysis;           
    StEStructPairCuts*   pcuts = new StEStructPairCuts(cutFile);

    for(int i=0;i<numberOfAnalyses;i++){   
      int ic=i;
      if(numberOfAnalyses==1)ic=-1;
      analysis[i] = new StEStructFluctAnal();   
      analysis[i]->initStructures(tcuts);  // cuts define min and max vals   
      analysis[i]->setPairCuts(pcuts);
      analysis[i]->setOutputFileName(getOutFileName(outputDir,scratchDir,"data",ic));

   }
   estructMaker->SetAnalyses(analysis,numberOfAnalyses);

  // --- now do the work ---

    estructMaker->Init();
    estructMaker->startTimer();

    int counter=0, istat=0, i=0;
    while (istat!=2) {

        istat=estructMaker->Make();

        i++; counter++;
        if (counter==100) {
            cout<<"doing event ="<<i<<endl;
            counter=0;
        }
        if ( maxNumEvents!=0 && i>=maxNumEvents ) {
            istat=2;
        }
    }
    estructMaker->stopTimer();

  //--- now write out stats and cuts ---
   ofstream ofs(getOutFileName(outputDir,scratchDir,"stats"));
    estructMaker->logAllStats(ofs);
    ecuts->printCuts(ofs);
    tcuts->printCuts(ofs);
    // --- not in fluctuations ---    pcuts->printCuts(ofs);
   ofs<<endl;
   ofs.close();
  
   // --> root cut file 
   TFile* tf=new TFile(getOutFileName(outputDir,scratchDir,"cuts"),"RECREATE");
   ecuts->writeCutHists(tf);
   tcuts->writeCutHists(tf);
   tf->Close();

   // --> root qa histogram file 
   estructMaker->writeQAHists(getOutFileName(outputDir,scratchDir,"QA"));

   // --- write out the data 
   estructMaker->Finish();
}
            
/**********************************************************************
 *
 * $Log: doEStructFluct.C,v $
 * Revision 1.1  2006/04/04 22:15:56  porter
 * a large number of changes were done to simplify the doEStruct macros
 * in the sense that these are now more similar and should be easier
 * for Duncan's GUI to build.  Here are some examples.
 *
 *
 *********************************************************************/
