//usr/bin/env root4star -l -b -q $0'("'${1:-sim.fzd}'",'${2:-50}','${3:-0.001}','${4:-0.001}','${5:-3.0}','${6:-0.004}','${7:-0}',"'${8:- }'")'; exit $?
// that is a valid shebang to run script as executable, but with only one arg


// Run very fast fwd tracking
// generate some input data using genfzd

TFile *output = 0;

void close(     char *inFile =  "sim.fzd",
                int n = 100000, // nEvents to run
                double primaryVertexSigmaXY = 0.001,
                double primaryVertexSigmaZ = 0.001,
                double fstRasterR = 3.0,
                double fstRasterPhi = 0.0040906154,
                int  numFttToUse = 0,
                TString note = ""
            ) {
    // report all of the parameters passed in
    cout << "inFile = " << inFile << endl;
    cout << "nEvents = " << n << endl;
    TString mOutput = TString::Format( 
            "closure_PV_XY%dum_Z%dum_FST_R%.2fcm_PHI%0.3frad_NumFTT%d%s", 
            (int)(primaryVertexSigmaXY*1e4),
            (int)(primaryVertexSigmaZ*1e4),
            fstRasterR,
            fstRasterPhi,
            numFttToUse,
            note.Data()
        );
    // replace all "." with "p" in the output file name
    mOutput.ReplaceAll(".", "p");
    mOutput += ".root";
    cout << "Output file = " << mOutput.Data() << endl;

    // Setup the chain for reading an FZD
    TString _chain;
    _chain = "fzin sdt20211016 MakeEvent bigbig evout cmudst tree";
    

    gSystem->Load( "libStarRoot.so" );
    gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
    gROOT->LoadMacro("bfc.C");
    bfc(-1, _chain, inFile);

    gSystem->Load( "libStFttSimMaker" );
    gSystem->Load( "libStFcsTrackMatchMaker" );

    gSystem->Load( "libMathMore.so" );
    gSystem->Load( "libStarGeneratorUtil" );
    
    gSystem->Load("libXMLIO.so");
    gSystem->Load("libgenfit2.so");
    gSystem->Load("libKiTrack.so");
    gSystem->Load("StarGeneratorUtil");
    // gSystem->Load("libMathMore.so");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StEpdUtil");
    gSystem->Load("StFwdTrackMaker");

    gSystem->Load("StFwdUtils.so");
    

    // Configure the Forward Tracker
    StFwdClosureMaker * fwdClosure = new StFwdClosureMaker();
    fwdClosure->SetDebug(1);
    fwdClosure->mMaxIt = 4;
    
    fwdClosure->mBlowUp = 1e3;
    fwdClosure->mPVal = 1e-3;
    fwdClosure->mRelChi2 = 1e-3;

    fwdClosure->mFttMode = StFwdClosureMaker::kStrip;

    fwdClosure->mPrimaryVertexSigXY = primaryVertexSigmaXY;
    fwdClosure->mPrimaryVertexSigZ = primaryVertexSigmaZ;
    fwdClosure->mRasterR = fstRasterR;
    fwdClosure->mRasterPhi = fstRasterPhi;
    fwdClosure->mNumFttToUse = numFttToUse;
    fwdClosure->mOutFile = mOutput;
    fwdClosure->SetDebug(1);
    
    chain->AddBefore("MuDst", fwdClosure);


    StMuDstMaker * muDstMaker = (StMuDstMaker*)chain->GetMaker( "MuDst" );
    
    // if (muDstMaker){
    //     StFwdQAMaker *fwdQA = new StFwdQAMaker();
    //     fwdQA->SetDebug(2);
    //     chain->AddAfter("MuDst", fwdQA);
    // }

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
