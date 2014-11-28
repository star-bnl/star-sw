 
void doEStruct( const char* filelist,
                const char* outputDir,
                const char* scriptDir,
                int maxNumEvents = 1000 ) {

    // I have taken a standard doEStruct.C macro and stripped it as much as I
    // could so it only reads MuDst events, applies event and track cuts while
    // converting to EStructEvent, then write out the EStructEvent.
    char cutFile[1024];
    sprintf(cutFile,"%s/CutsFile.txt",scriptDir);
    const char* jobName = "AuAu200_2011_StEStructEventTest";

    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();
    gROOT->LoadMacro("getOutFileName.C");
    gROOT->LoadMacro("support.C");

    bool useGlobalTracks = false;
    // Need an EStruct maker.
    char *analysisType = "StEStructCorrelation";
    StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker(analysisType);

    // Set up the Analysis codes and initialize the cuts.
    StEStructEventCuts* ecuts  = new StEStructEventCuts(cutFile);
    StEStructTrackCuts* tcuts  = new StEStructTrackCuts(cutFile);

    
    //  reader = reader interface + pointer to Data Maker + cut classes
    StMuDstMaker*         mk     = new StMuDstMaker(0,0,"",filelist,".",500);
    StEStructMuDstReader* reader = new StEStructMuDstReader(mk,ecuts,tcuts);
    reader->setUseGlobalTracks(useGlobalTracks);
    estructMaker->SetEventReader(reader);
                                                                                                                       
    // create the QAHist object (must come after centrality and cutbin objects)
    // QA histograms include centrality class definitions and occupancies.
    // These can be re-defined in the macro that analyzes EStructEvents.
    StEStructCentrality* cent=StEStructCentrality::Instance();
    const double mbBins[] = {2, 15, 35, 68, 117, 187, 281, 401, 551, 739, 852, 1002};
    int mbNBins = 12;
    cent->setCentralities(mbBins,mbNBins);
    int EventType = 0;
    StEStructQAHists* qaHists = new StEStructQAHists(EventType);
    estructMaker->SetQAHists(qaHists);
    
    // Put special processing that needs to be done before the event loop here.
    TChain *ch = mk->chain();
    int nEvents = ch->GetEntries();
    cout << "Total number of events in chain = " << nEvents << endl;

    ch->SetBranchStatus("*",0);
    ch->SetBranchStatus("MuEvent.*",1);
    ch->SetBranchStatus("PrimaryTracks.*",1);
    ch->SetBranchStatus("GlobalTracks.*",1);
    if (ch->GetBranch("PrimaryVertices")) {
        ch->SetBranchStatus("PrimaryVertices.*",1);
    }
    ecuts->setDoFillHists(true);
    tcuts->setDoFillHists(true);

    StEStructEvent *ev;
    int igood = 0;
    int iev = 0;
    bool done = false;
    TTimeStamp TS;
    int startTime = TS.GetSec();
    char *outFile = getOutFileName(outputDir,jobName,"EStruct");
    cout << " getOutFileName(outputDir,jobName,\"EStruct\") = " << outFile << endl;
    TFile *fESOut = new TFile(outFile,"RECREATE");
    while (!done) {
        // This simply reads an event and fills a StEStructEvent object,
        // applying cuts. If event fails ev will be NULL;
        iev++;
        ev = reader->next();
        done = reader->done();
        if ((iev%1000) == 0) {
            TTimeStamp TS;
            cout << "Found " << igood << " events after scanning " << iev << ". Has been " << TS.GetSec()-startTime << " seconds since start of scan loop." << endl;
        }
        if (!ev) {
            continue;
        }
        igood++;
        TString evName("EStructEvent");  evName += iev;
        ev->Write(evName.Data());
        delete ev;
        if ((maxNumEvents!=0) && (iev>=maxNumEvents)) {
            done = true;
        }
    }
    fESOut->Close();

    // --> statistics file
    char *statsFile = getOutFileName(outputDir,jobName,"stats");
    cout << "getOutFileName(outputDir,jobName,\"stats\") = " << statsFile << endl;;
    ofstream ofs(statsFile);
    estructMaker->logAllStats(ofs);
    ecuts->printCuts(ofs);
    ecuts->printCutStats(ofs);
    tcuts->printCuts(ofs);
    tcuts->printCutStats(ofs);
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

}
