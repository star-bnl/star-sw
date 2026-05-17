//usr/bin/env root4star -l -b -q $0'("'${1:-/gpfs01/star/pwg/mrosales/jetFinderTest2024/star-sw/Jet_Data_NoFilter_500/pythia_jet_vz0_run100.fzd}'",'${2:-100}')'; exit $?
// that is a valid shebang to run script as executable, but with only two arg

// generate some input data using genfzd

TFile *output = 0;

bool RunFttChain = false; // we use GEANT directly
bool RunFstChain = false; // we use GEANT directly
bool RunFcsChain = true;
bool RunFwdChain = true;
bool RunMuDstMaker = true;

bool UseCachedGeom = true;
bool UseConstBz = false;
bool UseZeroB = false;

TString _fttChain = "fttSim";
TString _fcsChain = "fcsSim fcsWFF fcsCluster";
TString _fstChain = "fstFastSim";
TString _fwdTrackChain = "fwdTrack";
TString _geom = "y2024 agml usexgeom";

void DisableTrackFitting() {
    // Disable track fitting
    StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );
    assert( fwdTrack );
    fwdTrack->setTrackFittingOff();
}

void DoOnlyGlobalTrackFitting() {
    // Disable track fitting
    StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );
    if ( fwdTrack ){
        fwdTrack->setConfigKeyValue("TrackFitter:refit", false);
        fwdTrack->setConfigKeyValue("TrackFitter:doGlobalTrackFitting", true);
        fwdTrack->setConfigKeyValue("TrackFitter:doBeamlineTrackFitting", false);
        fwdTrack->setConfigKeyValue("TrackFitter:doPrimaryTrackFitting", false);
        fwdTrack->setConfigKeyValue("TrackFitter:doSecondaryTrackFitting", false);
        // skip finding fwd vertices
        fwdTrack->setConfigKeyValue("TrackFitter:findFwdVertices", false);
    }
}

void sim(   char *inFile =  "/gpfs01/star/pwg/mrosales/jetFinderTest2024/star-sw/Jet_Data_NoFilter_500/pythia_jet_vz0_run100.fzd",
            int n = 1000 // nEvents to run
        ) {
    // report all of the parameters passed in
    cout << "inFile = " << inFile << endl;
    cout << "# of Events = " << n << endl;

    // to use the geom cache (skip agml build which is faster)
    // set the _geom string to "" and make sure the cache file ("fGeom.root") is present
    if (UseCachedGeom)
        _geom = "";

    // Setup the chain for reading an FZD
    TString _chain = "";

    // Now turn off parts of the chain that we don't need
    if (!RunFttChain)
        _fttChain = "";
    if (!RunFcsChain)
        _fcsChain = "";
    if (!RunFstChain)
        _fstChain = "";
    if (!RunFwdChain)
        _fwdTrackChain = "";

    if (RunFcsChain && RunFwdChain){
        _fwdTrackChain = "fwdTrack fcsTrackMatch";
    }
    
    // Form the complete chain
    _chain = Form("fzin %s sdt20211016 %s %s %s %s MakeEvent StEvent McEvent ReverseField bigbig evout cmudst tree", _geom.Data(), _fttChain.Data(), _fcsChain.Data(), _fstChain.Data(), _fwdTrackChain.Data()); 
    // Note, I dont include the PicoWrite and PicoVtxless in chain because they load a bunch of things I dont want (and somehow cannot remove with -options)
    printf("Chain: \n%s\n", _chain.Data());
    

    gSystem->Load( "libStarRoot.so" );
    gROOT->LoadMacro("bfc.C");
    bfc(-1, _chain, inFile);

    // gSystem->Load("StEEmcDb.so");

    if ( UseConstBz )
        StarMagField::setConstBz(true);

    // FCS setup, if included
    if (RunFcsChain) {

        StFcsDbMaker* fcsdbmkr = (StFcsDbMaker*) chain->GetMaker("fcsDbMkr");
        cout << "fcsdbmkr="<<fcsdbmkr<<endl;
        StFcsDb* fcsdb = (StFcsDb*) chain->GetDataSet("fcsDb");
        fcsdb->forceFixGain();
        fcsdb->forceFixGainCorrection();
        cout << "fcsdb="<<fcsdb<<endl;
        //fcsdbmkr->setDbAccess(1);

        // Configure FCS simulator
        StFcsFastSimulatorMaker *fcssim = (StFcsFastSimulatorMaker*) chain->GetMaker("fcsSim");
        fcssim->setDebug(1);
        //fcssim->setLeakyHcal(0);

        StFcsWaveformFitMaker *fcsWFF= (StFcsWaveformFitMaker*) chain->GetMaker("StFcsWaveformFitMaker");
        fcsWFF->setEnergySelect(0);

        StFcsClusterMaker *fcsclu = (StFcsClusterMaker*) chain->GetMaker("StFcsClusterMaker");
        fcsclu->setDebug(1);
    }

    gSystem->Load("StFwdUtils.so");

    // Configure FST FastSim
    if (RunFstChain){ // otherwise it is not loaded
        StFstFastSimMaker *fstFastSim = (StFstFastSimMaker*) chain->GetMaker( "fstFastSim" );;
        if (fstFastSim) {
            printf("fstFastSim = %p\n", fstFastSim);
            TString qaoutname(gSystem->BaseName(inFile));
            qaoutname.ReplaceAll(".fzd", ".FastSimu.QA.root");
            
            // if (SiIneff)
            //     fstFastSim->SetInEfficiency(0.1); // inefficiency of Si

            fstFastSim->SetQAFileName(qaoutname);
        }
    }

    gSystem->Load( "StFttDbMaker" );
    gSystem->Load( "libStFttSimMaker" );
    gSystem->Load( "libStFttClusterPointMaker" );
    // make an StFttClusterPointMaker
    StFttClusterPointMaker * fttClusterPointMaker = new StFttClusterPointMaker("fttClusterPointMaker");
    fttClusterPointMaker->SetDebug(1);
    fttClusterPointMaker->setUseGeantData( true );
    chain->AddBefore("fwdTrack", fttClusterPointMaker);
        
    // Configure the Forward Tracker
        StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );

        if ( fwdTrack ){
            if ( _geom == "" ){
                cout << "Using the Geometry cache: fGeom.root" << endl;
                fwdTrack->setGeoCache( "fGeom.root" );
            }

            fwdTrack->setOutputFilename( TString::Format( "%s.output.root", inFile ).Data() );

            // Fitter
            fwdTrack->setFitDebugLvl( 0 );
            fwdTrack->setFitMinIterations( 10 );
            fwdTrack->setFitMaxIterations( 20 );
            
            fwdTrack->setDeltaPval( 1e-1 );
            fwdTrack->setRelChi2Change( 1e-6 );
            
            // fwdTrack->setFttHitSource( 0 /*StFwdHitLoader::GEANT*/ );
            fwdTrack->setFttHitSource( 1 /*StFwdHitLoader::IGNORE*/ );
            fwdTrack->setFstHitSource( 0 /*StFwdHitLoader::GEANT*/ );

            // DisableTrackFitting();
            // DoOnlyGlobalTrackFitting();
            // fwdTrack->setTrackFittingOff();
            fwdTrack->setConfigKeyValue( "TrackFitter:refit", true );
            
            if ( UseZeroB ){
                cout << "Setting B = 0" << endl;
                fwdTrack->setZeroB( true );
            }
            if ( UseConstBz ){
                cout << "Setting Bz = const everywhere" << endl;
                fwdTrack->setConstBz( true );
            }

            
            cout << "fwd tracker setup" << endl;
        }
    
    bool doFitQA = false;
    if ( doFitQA ){
        StFwdFitQAMaker *fwdFitQA = new StFwdFitQAMaker();
        fwdFitQA->SetDebug();
        TString fitqaoutname(gSystem->BaseName(inFile));
        fitqaoutname.ReplaceAll(".fzd", ".FwdFitQA.root");
        fwdFitQA->setOutputFilename( fitqaoutname );
        chain->AddAfter("fwdTrack", fwdFitQA);
    }

    bool doFwdAna = false;
    if (!RunFcsChain && doFwdAna ){
        StFwdAnalysisMaker *fwdAna = new StFwdAnalysisMaker();
        fwdAna->SetDebug();
        chain->AddAfter("fwdTrack", fwdAna);
    }


    StMuDstMaker * muDstMaker = (StMuDstMaker*)chain->GetMaker( "MuDst" );
    // if (RunFcsChain) {
    //     // FwdTrack and FcsCluster assciation
    //     gSystem->Load("StFcsTrackMatchMaker");
    //     StFcsTrackMatchMaker *match = new StFcsTrackMatchMaker();
    //     match->setMaxDistance(6,10);
    //     match->setFileName("fcstrk.root");
    //     match->SetDebug();
    //     chain->AddMaker(match);

    //     if ( doFwdAna ){
    //         StFwdAnalysisMaker *fwdAna = new StFwdAnalysisMaker();
    //         fwdAna->SetDebug();
    //         chain->AddAfter("FcsTrkMatch", fwdAna);
    //     }

    //     // Produce MuDst output
    //     if ( muDstMaker )
    //         chain->AddAfter( "FcsTrkMatch", muDstMaker );
    // } else {
    //     if ( muDstMaker && doFwdAna )
    //         chain->AddAfter( "fwdAna", muDstMaker );
    // }

    // The PicoDst
    gSystem->Load("libStPicoEvent");
    gSystem->Load("libStPicoDstMaker");
    StPicoDstMaker *picoMk = new StPicoDstMaker(StPicoDstMaker::IoWrite);
    cout << "picoMk = " << picoMk << endl;
    picoMk->setVtxMode(StPicoDstMaker::Vtxless);

    StMemStat stmem;
    stmem.PrintMem("MEM before Chain::Init");
chain_loop:
	chain->Init();
    stmem.PrintMem("MEM after Chain::Init");

    //_____________________________________________________________________________
    //
    // MAIN EVENT LOOP
    //_____________________________________________________________________________
    for (int i = 0; i < n; i++) {

        cout << "--------->START EVENT: " << i << endl;

        if (i > 1)
            stmem.PrintMem("MEM before Chain::Clear + Make");
        chain->Clear();
        if (kStOK != chain->Make())
            break;

        if (i > 1)
            stmem.PrintMem("MEM after Chain::Clear + Make");

        // StMuDst * mds = muDstMaker->muDst();
        // StMuFwdTrackCollection * ftc = mds->muFwdTrackCollection();
        // cout << "Number of StMuFwdTracks: " << ftc->numberOfFwdTracks() << endl;
        // for ( size_t iTrack = 0; iTrack < ftc->numberOfFwdTracks(); iTrack++ ){
        //     StMuFwdTrack * muFwdTrack = ftc->getFwdTrack( iTrack );
        //     cout << "muFwdTrack->mPt = " << muFwdTrack->momentum().Pt() << endl;

        // }
        cout << "<---------- END EVENT" << endl;
    } // event loop

    stmem.PrintMem("MEM after event loop");
    // delete chain;
    stmem.PrintMem("MEM after delete chain");
}
