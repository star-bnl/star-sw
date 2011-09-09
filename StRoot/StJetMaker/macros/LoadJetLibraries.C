// -*- mode: c++;-*-
// $Id: LoadJetLibraries.C,v 1.15 2011/09/09 16:44:13 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

void LoadJetLibraries()
{
  setLibraryPath();

  TString LoadJetLibraries_STAR_VERSION = TString("LoadJetLibraries_") + gSystem->Getenv("STAR_VERSION") + "()";

  gROOT->ProcessLine(LoadJetLibraries_STAR_VERSION.Data());

  setIncludePath();

  gROOT->ProcessLine(".L StjDict.C+");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("libfastjet.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL08c()
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJetMaker");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJets");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL08a()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL07e()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL07d()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL07c()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
}


void LoadJetLibraries_SL07b()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
}

void LoadJetLibraries_SL07a()
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
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
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetMaker");
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

void setIncludePath()
{
  TString path(gSystem->GetIncludePath());
  path = TString("-I/afs/rhic.bnl.gov/star/packages/") + gSystem->Getenv("STAR_VERSION") + "/." + gSystem->Getenv("STAR_HOST_SYS") + "/include " + path;
  path = "-I./StRoot/StJetMaker/base " + path;
  path = "-I./StRoot/StJetMaker/emulator " + path;
  path = "-I./StRoot/StJetMaker/misc " + path;
  path = "-I./StRoot/StJetMaker/mudst " + path;
  path = "-I./StRoot/StJetMaker/trigger " + path;
  path = "-I./StRoot/StJetMaker/tree " + path;
  path = "-I./StRoot/StJetMaker/vertex " + path;
  path = "-I./StRoot/StJetMaker/mckin " + path;
  path = "-I./StRoot/StJetMaker/jets " + path;
  path = "-I./StRoot/StJetMaker/dijets " + path;
  path = "-I./StRoot/StJetMaker/spin " + path;
  path = "-I./StRoot " + path;
  path = TString("-I./.") + gSystem->Getenv("STAR_HOST_SYS") + "/include. " + path;
  path = "-I. " + path;
  gSystem->SetIncludePath(path);
}

