//RunChainMerger.C
#include <string>

void RunChainMerger(const char* dir, const char* outfile)
{
    cout <<"Loading Libraries"<<endl;
    
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMagF");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities"); 
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("JetFinder");
    cout <<"Done Loading Libraries"<<endl;

    cout <<"Make ChainMerger, watch it go!"<<endl;

    cout <<"dir:\t"<<dir<<"\toutfile:\t"<<outfile<<endl;
    
    ChainMerger myMerger(dir,outfile);
    cout <<"All done"<<endl;
}
