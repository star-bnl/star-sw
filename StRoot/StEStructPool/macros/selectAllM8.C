void selectAllM8(const char* dirname, const char *fileBase ){

    // -- example for recombining histograms by selection in new root file 
    //
    // root.exe -q -b selectASNS.C'("dirContainingFinal.rootFile")
    //

    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();
    TString inFile(dirname);
    inFile += "/";
    inFile += fileBase;
    inFile += ".root";
    gSystem->Load("StEStructPoolSupport.so");

    StEStructHAdd adder;

    TFile *tf = new TFile(inFile.Data());

    if (!tf) {
        cout<<"error opening file "<<endl;
        return ;
    };
    StEStructCutBin* cb = StEStructCutBin::Instance();
    cb->setMode(8);

    //--> do all of the following
    const char* oname[] = {"all","soft","neck","hard","softNeck","softHar","neckHard"};

    const int _map[6] = {0, 1, 2, 3, 4, 5};

    int parentSum[3][2]  = { 0,0, 1,1, 2,2 };
    int nParentSum[2];

    cout << "all" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="all.root";
    parentSum[0][0] = 0;  parentSum[0][1] = 0;
    parentSum[1][0] = 1;  parentSum[1][1] = 1;
    parentSum[2][0] = 2;  parentSum[2][1] = 2;
    int ndata = 0;
    nParentSum[0] = 3;
    nParentSum[1] = 3;
    adder.addCuts(fname.Data(),tf,_map,6,parentSum,nParentSum,1);

    cout << "soft" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="soft.root";
    parentSum[0][0] = 0;  parentSum[0][1] = 0;
    int ndata = 0;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);

    cout << "neck" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="neck.root";
    parentSum[0][0] = 1;  parentSum[0][1] = 1;
    int ndata = 1;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);

    cout << "hard" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="hard.root";
    parentSum[0][0] = 2;  parentSum[0][1] = 2;
    int ndata = 2;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);

    cout << "soft-neck" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="softNeck.root";
    parentSum[0][0] = 0;  parentSum[0][1] = 1;
    int ndata = 3;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);

    cout << "soft-hard" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="softHard.root";
    parentSum[0][0] = 0;  parentSum[0][1] = 2;
    int ndata = 4;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);

    cout << "neck-hard" << endl;
    TString fname(dirname);  fname += "/";  fname += fileBase;  fname+="neckHard.root";
    parentSum[0][0] = 1;  parentSum[0][1] = 2;
    int ndata = 5;
    nParentSum[0] = 1;
    nParentSum[1] = 1;
    adder.addCuts(fname.Data(),tf,&ndata,1,parentSum,nParentSum);


    for(int k=1;k<7;k++) {
        int nin = num[k];
        int *ndata = &_map[k];
    }

    TString fname(dirname);
    fname+="/";
    fname+=fileBase;
    fname+="_pairDensities.root";
    adder.addDensities(fname.Data(),tf);
};


