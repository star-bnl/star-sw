//wrapper to makeMuDstQA.C
//for ACLiC mode
void runmakeMuDstQA(TString InputFileList,TString OutputDir)
{
   gROOT->Reset();
   gROOT->Macro("loadMuDst.C");
   gROOT->LoadMacro("makeMuDstQA.C+");
   makeMuDstQA(InputFileList,1,0,OutputDir); //one file per job
}
