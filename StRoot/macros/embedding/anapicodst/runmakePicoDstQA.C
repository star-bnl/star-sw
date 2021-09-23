//wrapper to makePicoDstQA.C
//for ACLiC mode
void runmakePicoDstQA(TString InputFileList,Int_t nfiles,Int_t nevents,TString OutputFile)
{
   gROOT->Reset();
   gROOT->Macro("loadMuDst.C");
   gSystem->Load("StPicoEvent");
   gSystem->Load("StPicoDstMaker");
   gROOT->LoadMacro("makePicoDstQA.C+");
   makePicoDstQA(InputFileList,nfiles,nevents,OutputFile); 
}
