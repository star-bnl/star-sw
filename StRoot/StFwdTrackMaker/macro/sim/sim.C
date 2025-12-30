//usr/bin/env root4star -l -b -q $0'("'${1:-sim.fzd}'",'${2:-2000}')'; exit $?
// that is a valid shebang to run script as executable, but with only one arg


// Run very fast fwd tracking
// generate some input data using genfzd

TFile *output = 0;

void sim(       char *inFile =  "sim.fzd",
                int n = 100, // nEvents to run
                bool useFstForSeedFinding = true, // use FTT (default) or FST for track finding
                bool enableTrackRefit = true, // Enable track refit (default off)
                bool realisticSim = true, // enables data-like mode, real track finding and fitting without MC seed
                bool useZeroB = false
            ) {
    // report all of the parameters passed in
    cout << "inFile = " << inFile << endl;
    cout << "n = " << n << endl;
    cout << "useFstForSeedFinding = " << useFstForSeedFinding << endl;
    cout << "enableTrackRefit = " << enableTrackRefit << endl;
    cout << "realisticSim = " << realisticSim << endl;
    cout << "useZeroB = " << useZeroB << endl;
    const char *geom = "y2024 agml usexgeom";
    TString _geom = geom;

    // Switches for common options
    bool SiIneff = false;
    bool useConstBz = false;
    bool useFCS = true;

    // use cached
    _geom = "";

    // to use the geom cache (skip agml build which is faster)
    // set the _geom string to "" and make sure the cache file ("fGeom.root") is present
    // _geom = "";

    // Setup the chain for reading an FZD
    TString _chain;
    if ( useFCS )
        _chain = Form("fzin %s sdt20211016 fstFastSim fcsSim fcsWFF fcsCluster fwdTrack MakeEvent StEvent McEvent ReverseField bigbig evout cmudst tree", _geom.Data() );
    else
        _chain = Form("fzin %s sdt20211016 MakeEvent StEvent ReverseField bigbig fstFastSim fcsSim fwdTrack evout cmudst tree", _geom.Data());

    gSystem->Load( "libStarRoot.so" );
    gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
    gROOT->LoadMacro("bfc.C");
    bfc(-1, _chain, inFile);

    if ( useConstBz )
        StarMagField::setConstBz(true);

    gSystem->Load( "libStFttSimMaker" );
    gSystem->Load( "libStFcsTrackMatchMaker" );

    gSystem->Load( "libMathMore.so" );
    gSystem->Load( "libStarGeneratorUtil" );

    StFttFastSimMaker * fttSim = new StFttFastSimMaker();
    fttSim->SetDebug();
    chain->AddAfter("fcsSim", fttSim);

    // FCS setup, if included
    if (useFCS) {

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

    // {
        gSystem->Load("StFwdUtils.so");
    //     StFwdJPsiMaker *fwdJPsi = new StFwdJPsiMaker();
    //     fwdJPsi->SetDebug();
    //     chain->AddMaker(fwdJPsi);
    //     goto chain_loop;
    // }


    // Configure FST FastSim
        TString qaoutname(gSystem->BaseName(inFile));
        qaoutname.ReplaceAll(".fzd", ".FastSimu.QA.root");
        StFstFastSimMaker *fstFastSim = (StFstFastSimMaker*) chain->GetMaker( "fstFastSim" );;

        if (SiIneff)
            fstFastSim->SetInEfficiency(0.1); // inefficiency of Si

        fstFastSim->SetQAFileName(qaoutname);

        cout << "Adding StFstFastSimMaker to chain" << endl;
        chain->AddAfter("fcsSim", fstFastSim);


    // Configure the Forward Tracker
        StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );

        if ( fwdTrack ){
            fwdTrack->SetDebug(1);
            // config file set here for ideal simulation
            if (!realisticSim){
                cout << "Configured for ideal simulation (MC finding + MC mom seed)" << endl;
                fwdTrack->setConfigForIdealSim( );
            } else {
                cout << "Configured for realistic simulation" << endl;
                fwdTrack->setConfigForRealisticSim( );
                cout << "Configured for realistic simulation DONE" << endl;
            }

            if ( _geom == "" ){
                cout << "Using the Geometry cache: fGeom.root" << endl;
                fwdTrack->setGeoCache( "fGeom.root" );
            }

            // choose
                if (useFstForSeedFinding)
                    fwdTrack->setSeedFindingWithFst();
                else { // default for this true/false option
                    fwdTrack->setSeedFindingWithFtt();
                }
            // other options
                // fwdTrack->setSeedFindingWithFtt();
                // fwdTrack->setSeedFindingWithFstFttSequential();
                // fwdTrack->setSeedFindingWithFstFttSimultaneous();

            fwdTrack->setTrackRefit( enableTrackRefit );
            fwdTrack->setConstB( useConstBz );
            fwdTrack->setOutputFilename( TString::Format( "%s.output.root", inFile ).Data() );
            fwdTrack->SetVisualize( false );
            fwdTrack->SetDebug();
            fwdTrack->setIncludePrimaryVertexInFit( false );

            // fwdTrack->setTrackFittingOff();
            // fwdTrack->setUseMcSeedForFit(true);
            // fwdTrack->setConfigKeyValue("")
            if ( useZeroB ){
                cout << "Setting B = 0" << endl;
                fwdTrack->setZeroB( true );
            }
            bool doFitQA = true;
            if ( doFitQA ){
                StFwdFitQAMaker *fwdFitQA = new StFwdFitQAMaker();
                fwdFitQA->SetDebug();
                TString fitqaoutname(gSystem->BaseName(inFile));
                fitqaoutname.ReplaceAll(".fzd", ".FwdFitQA.root");
                fwdFitQA->setOutputFilename( fitqaoutname );
                chain->AddAfter("fwdTrack", fwdFitQA);
            }
            cout << "fwd tracker setup" << endl;
        }

        bool doFwdAna = true;
        if (!useFCS && doFwdAna ){
            StFwdAnalysisMaker *fwdAna = new StFwdAnalysisMaker();
            fwdAna->SetDebug();
            chain->AddAfter("fwdTrack", fwdAna);
        }


    StMuDstMaker * muDstMaker = (StMuDstMaker*)chain->GetMaker( "MuDst" );
    if (useFCS) {
        // FwdTrack and FcsCluster assciation
        gSystem->Load("StFcsTrackMatchMaker");
        StFcsTrackMatchMaker *match = new StFcsTrackMatchMaker();
        match->setMaxDistance(6,10);
        match->setFileName("fcstrk.root");
        match->SetDebug();
        chain->AddMaker(match);

        if ( doFwdAna ){
            StFwdAnalysisMaker *fwdAna = new StFwdAnalysisMaker();
            fwdAna->SetDebug();
            chain->AddAfter("FcsTrkMatch", fwdAna);
        }

        // Produce MuDst output
        if ( muDstMaker )
            chain->AddAfter( "FcsTrkMatch", muDstMaker );
    } else {
        if ( muDstMaker )
            chain->AddAfter( "fwdAna", muDstMaker );
    }

    if (muDstMaker){
        StFwdQAMaker *fwdQA = new StFwdQAMaker();
        fwdQA->SetDebug(2);
        TString fwdqaname(gSystem->BaseName(inFile));
        fwdqaname.ReplaceAll(".fzd", ".FwdTree.root");
        fwdQA->setTreeFilename(fwdqaname);
        chain->AddAfter("MuDst", fwdQA);
    }

    // The PicoDst
    gSystem->Load("libStPicoEvent");
    gSystem->Load("libStPicoDstMaker");
    StPicoDstMaker *picoMk = new StPicoDstMaker(StPicoDstMaker::IoWrite);
    cout << "picoMk = " << picoMk << endl;
    picoMk->setVtxMode(StPicoDstMaker::Default);


chain_loop:
	chain->Init();

    //_____________________________________________________________________________
    //
    // MAIN EVENT LOOP
    //_____________________________________________________________________________
    for (int i = 0; i < n; i++) {

        cout << "--------->START EVENT: " << i << endl;

        chain->Clear();
        if (kStOK != chain->Make())
            break;


        // StMuDst * mds = muDstMaker->muDst();
        // StMuFwdTrackCollection * ftc = mds->muFwdTrackCollection();
        // cout << "Number of StMuFwdTracks: " << ftc->numberOfFwdTracks() << endl;
        // for ( size_t iTrack = 0; iTrack < ftc->numberOfFwdTracks(); iTrack++ ){
        //     StMuFwdTrack * muFwdTrack = ftc->getFwdTrack( iTrack );
        //     cout << "muFwdTrack->mPt = " << muFwdTrack->momentum().Pt() << endl;

        // }
        cout << "<---------- END EVENT" << endl;
    } // event loop
}
