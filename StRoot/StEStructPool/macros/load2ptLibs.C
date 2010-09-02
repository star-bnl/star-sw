void load2ptLibs(){


    gSystem->Load("libTable.so");
    gSystem->Load("libPhysics.so");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    //gSystem->Load("StMagF");
    gSystem->Load("StarMagField");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("StEStructPoolEventMaker");
    gSystem->Load("StEStructPoolAnalysisMaker");
    gSystem->Load("StEStructPoolCorrelations");
    gSystem->Load("StEStructPoolFluctuations");
    gSystem->Load("StEStructPoolPhiWeight");
    gSystem->Load("StEStructPoolPileup");
    gSystem->Load("StEStructPoolTSP");


};
