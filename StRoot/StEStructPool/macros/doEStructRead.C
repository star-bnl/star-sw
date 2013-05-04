 
void doEStructRead( const char* inputfile,
                    const char* outputDir,
                    const char* scriptDir,
                    int maxNumEvents = 1000 ) {

    char cutFile[1024];
    sprintf(cutFile,"%s/CutsFile.txt",scriptDir);
    const char* jobName = "ReadEStruct";

    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();
    gROOT->LoadMacro("getOutFileName.C");
    gROOT->LoadMacro("support.C");

    StEStructCentrality* cent=StEStructCentrality::Instance();
    const double mbBins[] = {2, 15, 35, 68, 117, 187, 281, 401, 551, 739, 852, 1002};
    int mbNBins = 12;
    cent->setCentralities(mbBins,mbNBins);

    // >>>>>>Check: Probably want to keep ZBuffers for central bins. I won't bother for testing though.
    const  int mkeepZBuffBins[] = {0};
    int mNZBufs = 1;
    int cutBinMode = 3;
    int analysisMode = 0x60;

    // choose the mode for the binning
    int numberOfAnalyses = mbNBins-1;
    StEStructCutBin* cb=StEStructCutBin::Instance();
    cb->setMode(cutBinMode);
    StEStruct2ptCorrelations** analysis = new StEStruct2ptCorrelations*[numberOfAnalyses];

    StEStructPairCuts* pcuts = new StEStructPairCuts(cutFile);

    // Need an EStruct maker.
    char *analysisType = "StEStructCorrelation";
    StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker(analysisType);

    StEStructEventCuts* ecuts  = new StEStructEventCuts(cutFile);
    StEStructTrackCuts* tcuts  = new StEStructTrackCuts(cutFile);
    StEStructEventReader* reader = new StEStructEStructReader(inputfile,ecuts,tcuts);
    estructMaker->SetEventReader(reader);

    int EventType = 0;
    StEStructQAHists* qaHists = new StEStructQAHists(EventType);
    estructMaker->SetQAHists(qaHists);
                                                         
    //  build the numberOfAnalyses readers and numberOfAnalyses analysis objects
    //  analysis = analysis interface and contains pair-cut object (needs file)
    for(int i=0;i<numberOfAnalyses;i++) {

        analysis[i] = new StEStruct2ptCorrelations(pcuts,analysisMode);
//        analysis[i]->setZBuffLimits(ecuts);
        analysis[i]->setAnalysisIndex(i);
        analysis[i]->setOutputFileName(getOutFileName(outputDir,jobName,"data",i));
        analysis[i]->setQAHists(qaHists);
        if ((mNZBufs == numberOfAnalyses) && (0 != mkeepZBuffBins[i])) {
            analysis[i]->setZBufferBinning(1);
        }
            

    }
    estructMaker->SetAnalyses(analysis,numberOfAnalyses);
    estructMaker->Init();

    ecuts->setDoFillHists(true);
    tcuts->setDoFillHists(true);
// How do we get number of events in advance?
//    cout << "Total number of events in file = " << nEvents << endl;
    int iev = 0;
    int istat = 0;
    TTimeStamp TS;
    int startTime = TS.GetSec();
    while (istat != 2) {
        istat = estructMaker->Make();
        iev++;
        if ((iev%200) == 0) {
            TTimeStamp TS;
            cout << "Analysed " << iev << " events in " << TS.GetSec()-startTime << " seconds." << endl;
        }
        if ((maxNumEvents> 0) && (iev >= maxNumEvents)) {
            break;
        }
    }
    TTimeStamp TS;
    cout << "Seconds for entire event loop = " << TS.GetSec()-startTime << endl;

    
    // If special processing needs clean up put code here.
            


    // --> statistics file
    ofstream ofs(getOutFileName(outputDir,jobName,"stats"));
    estructMaker->logAllStats(ofs);
    ecuts->printCuts(ofs);
    ecuts->printCutStats(ofs);
    tcuts->printCuts(ofs);
    tcuts->printCutStats(ofs);
    pcuts->printCuts(ofs);
    ofs<<endl;
    ofs.close();

    // --> root cut histogram file
    char *cutsFile = getOutFileName(outputDir,jobName,"cuts");
    cout << "getOutFileName(outputDir,jobName,\"cuts\") = " << cutsFile << endl;
    TFile *tfc = new TFile(cutsFile,"RECREATE");
    ecuts->writeCutHists(tfc);
    tcuts->writeCutHists(tfc);
    tfc->Close();

    // --> root QA histogram file
    char *qaFile = getOutFileName(outputDir,jobName,"QA");
    cout << "getOutFileName(outputDir,jobName,\"QA\") = " << qaFile << endl;
    estructMaker->writeQAHists(qaFile);

    // --> Write out the data in the form of root histograms.
    estructMaker->Finish();
}
