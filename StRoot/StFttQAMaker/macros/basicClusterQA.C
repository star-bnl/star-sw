

const Int_t mMax = 10000;
struct FttData
{
    // event information
    Int_t    EVT;
    Int_t    N;

    //channel information
    Int_t    sec[mMax];
    Int_t    rdo[mMax];
    Int_t    plane[mMax];
    Int_t    quad[mMax];
    Int_t    feb[mMax];
    Int_t    febvmm[mMax];
    Int_t    vmm[mMax];
    Int_t    ch[mMax];
    Int_t    bcid[mMax];
    Int_t    adc[mMax];
    Int_t    tb[mMax];
    Int_t    row[mMax];
    Int_t    strip[mMax];
    Int_t    dir[mMax];

    Int_t    cN;
    Int_t    cplane[mMax];
    Int_t    cquad[mMax];
    Int_t    crow[mMax];
    Int_t    cdir[mMax];
    Int_t    csumadc[mMax];
    Float_t  cx[mMax];
    Float_t  csigma[mMax];
    Float_t  cnstrips[mMax];

};

FttData mFttData;

void printHit( FttData &data, int i){
    cout    << "hit(";
    cout    << "\tsec=" << data.sec[i] << endl;
    cout    << "\trdo=" << data.rdo[i] << endl;
    cout    << "\tplane=" << data.plane[i] << endl;
    cout    << "\tquad=" << data.quad[i] << endl;
    cout    << "\tfeb=" << data.feb[i] << endl;
    cout    << "\tfebvmm=" << data.febvmm[i] << endl;
    cout    << "\tvmm=" << data.vmm[i] << endl;
    cout    << "\tch=" << data.ch[i] << endl;
    cout    << "\tbcid=" << data.bcid[i] << endl;
    cout    << "\tadc=" << data.adc[i] << endl;
    cout    << "\ttb=" << data.tb[i] << endl;
    cout    << "\trow=" << data.row[i] << endl;
    cout    << "\tstrip=" << data.strip[i] << endl;
    cout    << "\tdir=" << data.dir[i] << endl;
    cout    << ")" << endl;
}

void basicClusterQA(){


    TFile * tf = new TFile("fttQA.root");
    TTree * mFttTree = (TTree*)tf->Get("ftt");


    // Event information
    mFttTree->SetBranchAddress("EVT"      , &mFttData.EVT     );
    mFttTree->SetBranchAddress("N"        , &mFttData.N       );

    // Channel information
    mFttTree->SetBranchAddress("sec"      , mFttData.sec      );
    mFttTree->SetBranchAddress("rdo"      , mFttData.rdo      );
    mFttTree->SetBranchAddress("plane"    , mFttData.plane    );
    mFttTree->SetBranchAddress("quad"     , mFttData.quad     );
    mFttTree->SetBranchAddress("feb"      , mFttData.feb      );
    mFttTree->SetBranchAddress("febvmm"   , mFttData.febvmm   );
    mFttTree->SetBranchAddress("vmm"      , mFttData.vmm      );
    mFttTree->SetBranchAddress("ch"       , mFttData.ch       );
    mFttTree->SetBranchAddress("bcid"     , mFttData.bcid     );
    mFttTree->SetBranchAddress("adc"      , mFttData.adc      );
    mFttTree->SetBranchAddress("tb"       , mFttData.tb       );
    mFttTree->SetBranchAddress("row"      , mFttData.row      );
    mFttTree->SetBranchAddress("strip"    , mFttData.strip    );
    mFttTree->SetBranchAddress("dir"      , mFttData.dir      );

    mFttTree->SetBranchAddress("cN"       , &mFttData.cN      );
    mFttTree->SetBranchAddress("cplane"   , mFttData.cplane   );
    mFttTree->SetBranchAddress("cquad"    , mFttData.cquad    );
    mFttTree->SetBranchAddress("crow"     , mFttData.crow     );
    mFttTree->SetBranchAddress("cdir"     , mFttData.cdir     );
    mFttTree->SetBranchAddress("cx"       , mFttData.cx       );
    mFttTree->SetBranchAddress("csigma"   , mFttData.csigma   );
    mFttTree->SetBranchAddress("cnstrips" , mFttData.cnstrips );
    mFttTree->SetBranchAddress("csumadc"  , mFttData.csumadc  );



    

    // for ( int iPlane = 0; iPlane < 4; iPlane++ ){
    //     for ( int iQuad = 0; iQuad < 4; iQuad++ ){
    //         for ( int iRow = 0; iRow < 5; iRow++ ){
    //             for ( int iDir = 0; iDir < 4; iDir++ ){
    //                 string name = TString::Form( "hP%dQ%dR%dD%d", iPlane, iQuad, iRow, iDir );
    //                 hist[ name ] = new TH1F(  )
    //             }
    //         }
    //     }
    // }

    size_t nEvents = mFttTree->GetEntries();
    for ( size_t i = 0; i < nEvents; i++ ){
        mFttTree->GetEntry(i);
        if ( mFttData.N == 0 ) continue;

        for ( int j = 0; j < mFttData.N; j++ ){
            int index = mFttData.dir[j] + 5 * ( mFttData.row[j] + 5 * ( mFttData.quad[j] + 4 * mFttData.plane[j] ));
            printHit( mFttData, j );
            cout << "index: " << index << "sec=" << mFttData.sec[j] << endl;    
        }
        

    }
}