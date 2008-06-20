// -*- mode: c++;-*-
// $Id: LoadJetLibraries.C,v 1.4 2008/06/20 18:59:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

void LoadJetLibraries()
{
  setLibraryPath();

  TString LoadJetLibraries_STAR_VERSION = TString("LoadJetLibraries_") + gSystem->Getenv("STAR_VERSION") + "()";

  gROOT->ProcessLine(LoadJetLibraries_STAR_VERSION.Data());

}

void LoadJetLibraries_DEV()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("StTriggerFilterMaker");
  gSystem->Load("StarMagField.so");
  gSystem->Load("StMagF");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("geometry");
  gSystem->Load("St_g2t");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StChargedPionAnalysisMaker");
  gSystem->Load("StSkimPionMaker");
}

void LoadJetLibraries_SL08b()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("StTriggerFilterMaker");
  gSystem->Load("StarMagField.so");
  gSystem->Load("StMagF");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("geometry");
  gSystem->Load("St_g2t");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StChargedPionAnalysisMaker");
  gSystem->Load("StSkimPionMaker");
}


void setLibraryPath()
{
  TString path(gSystem->GetDynamicPath());
  TString pwd(gSystem->pwd());
  if(TString("StRoot") == gSystem->BaseName(pwd)) {
    path = TString("../.") + gSystem->Getenv("STAR_HOST_SYS") + "/lib:" + path;
  } else if("StRoot/StJetMaker" == pwd(pwd.Length() - TString("StRoot/StJetMaker").Length(), TString("StRoot/StJetMaker").Length())) {
    path = TString("../../.") + gSystem->Getenv("STAR_HOST_SYS") + "/lib:" + path;
  } else if("StRoot/StJetMaker/macros" == pwd(pwd.Length() - TString("StRoot/StJetMaker/macros").Length(), TString("StRoot/StJetMaker/macros").Length())) {
    path = TString("../../../.") + gSystem->Getenv("STAR_HOST_SYS") + "/lib:" + path;
  } else if("StRoot/StJetMaker/examples" == pwd(pwd.Length() - TString("StRoot/StJetMaker/examples").Length(), TString("StRoot/StJetMaker/examples").Length())) {
    path = TString("../../../.") + gSystem->Getenv("STAR_HOST_SYS") + "/lib:" + path;
  }
  path = ".:" + path;
  gSystem->SetDynamicPath(path);
}

