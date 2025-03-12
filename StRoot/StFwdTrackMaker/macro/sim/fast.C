//usr/bin/env root4star -l -b -q $0'("'${1:-sim.fzd}'",'${2:-5}')'; exit $?
// that is a valid shebang to run script as executable, but with only one arg


// Run very fast fwd tracking
// generate some input data using genfzd

TFile *output = 0;

void fast(       char *inFile =  "sim.fzd",
                int n = 1000, // nEvents to run
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
    const char *geom = "";
    TString _geom = geom;

    // Switches for common options
    bool SiIneff = false;
    bool useConstBz = false;
    bool useFCS = true;

    // to use the geom cache (skip agml build which is faster)
    // set the _geom string to "" and make sure the cache file ("fGeom.root") is present
    // _geom = "";

    // Setup the chain for reading an FZD
    TString _chain;
    
    _chain = Form("fzin %s sdt20211016 fwdTrack MakeEvent bigbig evout cmudst tree", _geom.Data() );
    

    gSystem->Load( "libStarRoot.so" );
    gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
    gROOT->LoadMacro("bfc.C");
    bfc(-1, _chain, inFile);

    gSystem->Load( "libStFttSimMaker" );
    gSystem->Load( "libStFcsTrackMatchMaker" );

    gSystem->Load( "libMathMore.so" );
    gSystem->Load( "libStarGeneratorUtil" );

    
    gSystem->Load("StFwdUtils.so");



    // Configure the Forward Tracker
        StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );

        if ( fwdTrack ){
            fwdTrack->SetDebug(1);
            // config file set here for ideal simulation
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

            
            fwdTrack->setOutputFilename( TString::Format( "%s.output.root", inFile ).Data() );
            fwdTrack->SetVisualize( false );
            fwdTrack->SetDebug();
            fwdTrack->setTrackRefit( enableTrackRefit );
            fwdTrack->setConstB( useConstBz );
            
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

    StMuDstMaker * muDstMaker = (StMuDstMaker*)chain->GetMaker( "MuDst" );
    
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
        cout << "<---------- END EVENT" << endl;
    } // event loop
}
