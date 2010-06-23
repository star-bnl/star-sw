void selectAllM0(const char* dirname, const char *fileBase ){

    // -- example for recombining histograms by selection in new root file 
    //
    // root.exe -q -b selectAllM0.C'("dirContainingRootFile","fileBase")
    //   fileBase is the file name without the .root extension.
    //

    gROOT->LoadMacro("load2ptLibs.C");
    load2ptLibs();
    gSystem->Load("StEStructPoolSupport.so");

    TString inFile(dirname);
    inFile+="/";
    inFile+=fileBase;
    inFile+=".root";

    StEStructHAdd adder;

    TFile * tf=new TFile(inFile.Data());
    if(!tf){
        cout<<"error opening file "<<endl;
        return ;
    };

    TString fname(dirname);
    fname+="/";
    fname+=fileBase;
    fname+="Symm.root";

    int ndata[] = {0};
    int parentSum[1][2] = {0, 0};
    int nPararentSum[] = {1,1};
    adder.addCuts(fname.Data(),tf,ndata,1,parentSum,nParentSum);

    TString fname(dirname);
    fname+="/";
    fname+=fileBase;
    fname+="_pairDensities.root";
    adder.addDensities(fname.Data(),tf);
};


