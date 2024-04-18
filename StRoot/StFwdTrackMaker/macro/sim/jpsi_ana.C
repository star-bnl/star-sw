//usr/bin/env root4star -l -b -q $0'('$1')'; exit $?
// that is a valid shebang to run script as executable, but with only one arg


// Run very fast fwd tracking
// generate some input data using genfzd 

TFile *output = 0;

void jpsi_ana( int n = 5, // nEvents to run
                string outputName = "stFwdTrackMaker_ideal_jpsi.root",
                bool useFstForSeedFinding = false, // use FTT (default) or FST for track finding
                bool enableTrackRefit = true, // Enable track refit (default off)
                bool realisticSim = false, // enables data-like mode, real track finding and fitting without MC seed
                char *inFile =  "jpsi.fzd"
            ) {
    cout << "Running " << n << " events from " << inFile << endl;
    const char *geom = "y2023";
    TString _geom = geom;

    // Switches for common options 
    bool SiIneff = false;
    bool useConstBz = false;
    bool useFCS = true;
    
    
    // Setup the chain for reading an FZD
    TString _chain;
    if ( useFCS )
        _chain = Form("fzin %s sdt20211016 fstFastSim fcsSim fcsWFF fcsCluster fwdTrack MakeEvent StEvent ReverseField agml usexgeom bigbig  evout cmudst tree", _geom.Data());
    else 
        _chain = Form("fzin %s sdt20211016 MakeEvent StEvent ReverseField agml usexgeom bigbig fstFastSim fcsSim fwdTrack evout cmudst tree", _geom.Data());

    gSystem->Load( "libStarRoot.so" );
    gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
    gROOT->LoadMacro("bfc.C");
    bfc(-1, _chain, inFile);

    if ( useConstBz )
        StarMagField::setConstBz(true);

    gSystem->Load( "libStFttSimMaker" );
    gSystem->Load( "libStFcsTrackMatchMaker" );

    // FCS setup, if included
    if (useFCS) {

        StFcsDbMaker* fcsdbmkr = (StFcsDbMaker*) chain->GetMaker("fcsDbMkr");  
        cout << "fcsdbmkr="<<fcsdbmkr<<endl;
        StFcsDb* fcsdb = (StFcsDb*) chain->GetDataSet("fcsDb");  
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
        chain->AddMaker(fstFastSim);


    // Configure the Forward Tracker
        StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );
        
        // config file set here for ideal simulation
        if (!realisticSim){
            cout << "Configured for ideal simulation (MC finding + MC mom seed)" << endl;
            fwdTrack->setConfigForIdealSim( );
        } else {
            cout << "Configured for realistic simulation" << endl;
            fwdTrack->setConfigForRealisticSim( );
            cout << "Configured for realistic simulation DONE" << endl;
        }

        if (useFstForSeedFinding)
            fwdTrack->setSeedFindingWithFst();
        else
            fwdTrack->setSeedFindingWithFtt();

        fwdTrack->setTrackRefit( enableTrackRefit );
        fwdTrack->setOutputFilename( outputName );
        fwdTrack->SetGenerateTree( true );
        fwdTrack->SetGenerateHistograms( true );
        fwdTrack->SetDebug();

        cout << "fwd tracker setup" << endl;

        
        if (!useFCS){
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

        StFwdAnalysisMaker *fwdAna = new StFwdAnalysisMaker();
        fwdAna->SetDebug();
        chain->AddAfter("FcsTrkMatch", fwdAna);

        StFwdJPsiMaker *fwdJPsi = new StFwdJPsiMaker();
        fwdJPsi->SetDebug();
        chain->AddAfter("FcsTrkMatch", fwdJPsi);

	gSystem->Load("StFcsDiLeptonMaker");
        StFcsDiLeptonMaker *dilep = new StFcsDiLeptonMaker;
	//TString dilepfile(outfile); dilepfile.ReplaceAll(".root",".dilep.root");                                                                  
        dilep->setFileName("dilep.root");//dilepfile.Data());                                                                                       
        //chain->AddAfter("FcsTrkMatch", dilep);            

        // Produce MuDst output
        chain->AddAfter( "FcsTrkMatch", muDstMaker );
    } else {
        chain->AddAfter( "fwdAna", muDstMaker );
    }

    

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
