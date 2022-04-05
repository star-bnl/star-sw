//wrapper to makeMuDstQA.C
//for ACLiC mode
void runmakeMuDstQA(TString InputFileList,Int_t nfiles,Int_t nevents,TString OutputFile)
{
   gROOT->Reset();
   gROOT->Macro("loadMuDst.C");
   gROOT->LoadMacro("makeMuDstQA.C+");
   makeMuDstQA(InputFileList,nfiles,nevents,OutputFile); 
}
