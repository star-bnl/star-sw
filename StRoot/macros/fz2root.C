class St_db_Maker;
St_db_Maker *dbMk =0;
// Conver Zebra file into ROOT format
void fz2root(const char *file=
            "star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd"
            , unsigned int nEvent=1) 
{
   gROOT->Macro("Load.C"); 
   gSystem->Load("StDetectorDbMaker");
   gROOT->LoadMacro("bfc.C");
   gSystem->Load("StTreeMaker");
    bfc(0,"fzin",file);
    // WriteOut 
     TString basename = gSystem->BaseName(file);
     basename.ReplaceAll(".fzd","");
     char *outName  = gSystem->ConcatFileName("t",basename);
     dbMk = (St_db_Maker *)chain->Maker("db");
     dbMk->SetDateTime(20090312, 94451);

     StTreeMaker *outMk = new StTreeMaker("EvOut",outName,"bfcTree");
        outMk->SetIOMode("w");
        outMk->IntoBranch("geantBranch","geant");
      outMk->SetDebug();
      outMk->Init();
      chain->EventLoop(nEvent);
      chain->Finish();
}
