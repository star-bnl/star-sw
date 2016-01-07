#include "StGmtQAPlotter.h"
#include <TChain.h>
#include <TSystemDirectory.h>
#include <TList.h>
#include <TString.h>
#include <TFile.h>
#include <iostream>

Int_t addfiles(TChain *ch, const Char_t *dirname=".", const Char_t *ext=".root");

//_________________
void GmtChain(const Char_t *dirname, const Char_t *ext, const Char_t *oFileName="test") {

  //gROOT->LoadMacro("StGmtQAPlotter.C+");
  //gInterpreter->ProcessLine(".L StGmtQAPlotter.C+");
  int mFilesNum = 0;
  TChain* mChain = new TChain("T");

  mFilesNum = addfiles(mChain, dirname, ext);
  std::cout << "Files in chain: " << mFilesNum << std::endl;
  TFile* oFile = new TFile(oFileName,"recreate");
  StGmtQAPlotter myPlotter(mChain,"","");
  //mChain->Init();
  myPlotter.Loop();

  oFile->Write();
  myPlotter.cUAdcDVsVAdcDCanvas->Write();
  myPlotter.cDuAdcDVsDvAdcDCanvas->Write();
  myPlotter.cDvAdcDOverDuAdcDCanvas->Write();

  myPlotter.cUdCanvas->Write();
  myPlotter.cUpCanvas->Write();
  myPlotter.cVdCanvas->Write();
  myPlotter.cVpCanvas->Write();
  myPlotter.cUdVsUpCanvas->Write();
  myPlotter.cVdVsVpCanvas->Write();
  myPlotter.cUdVsVdCanvas->Write();
  myPlotter.cUdiffVsInversePtCanvas->Write();
  myPlotter.cFitsUdiffVsInvPt->Write();
  myPlotter.cVdiffVsInversePtCanvas->Write();
  myPlotter.cFitsVdiffVsInvPt->Write();
  myPlotter.cUandVdiffVsBarrel->Write();

  //Derivatives
  myPlotter.cUdiffVsTuP->Write();
  myPlotter.cVdiffVsTvP->Write();
  myPlotter.cUdiffVsVp->Write();
  myPlotter.cVdiffVsUp->Write();
  myPlotter.cUdiffOverTuPVsVp->Write();
  myPlotter.cVdiffOverTvPVsVp->Write();
  myPlotter.cUdiffOverTuPVsUp->Write();
  myPlotter.cVdiffOverTvPVsUp->Write();

  myPlotter.cUdiffVsTuPSlices->Write();
  myPlotter.cVdiffVsTvPSlices->Write();
  myPlotter.cUdiffVsVpSlices->Write();
  myPlotter.cVdiffVsUpSlices->Write();
  myPlotter.cUdiffOverTuPVsVpSlices->Write();
  myPlotter.cVdiffOverTvPVsVpSlices->Write();
  myPlotter.cUdiffOverTuPVsUpSlices->Write();
  myPlotter.cVdiffOverTvPVsUpSlices->Write();

  
  oFile->Close();
  mChain->Delete();
}

//_________________
Int_t addfiles(TChain *ch, const char *dirname, const char *ext) {
  
  Int_t mFilesNum = 0;
  TSystemDirectory dir(dirname, dirname);
  TList *files = dir.GetListOfFiles();
  
  if (files) {
    TSystemFile *file;
    TString fname;
    TIter next(files);
    while ((file=(TSystemFile*)next())) {
      TString fullFileName;
      fname = file->GetName();
      if (!file->IsDirectory() && fname.EndsWith(ext)) {
	fullFileName.Append(dirname);
	fullFileName.Append(fname);
	cout << "Adding file: " << fullFileName << " Number: " << mFilesNum << endl;
	ch->Add(fullFileName); // or call your function on this one file
	++mFilesNum;
      }
    }
  }

  return mFilesNum;
}
