void runTrgTree(char* file="test.lis", Int_t nevt=1000){  
    
    gROOT->LoadMacro("bfc.C");
    bfc(-2,"StEvent,RMudst");

    StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 1000, "MuDst");

    int n=muDstMaker->tree()->GetEntries();
    printf("Found %d entries in Mudst\n",n);
    int start=0, stop=n;
    if(nevt>=0 && nevt<n){
	stop=nevt;
    }
    printf("Doing Event=%d to %d\n",start,stop);
    
    gSystem->Load("StTriggerDataMaker");
    StTrgTreeMaker* trgtree = new StTrgTreeMaker("trgTree");  

    chain->Init();
    chain->EventLoop(start,stop);
    chain->Finish();
}
