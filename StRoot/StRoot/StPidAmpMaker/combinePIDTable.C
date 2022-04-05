void combinePIDTable(){


  TString eTableName;
  eTableName +="PiPIDTable.root";

  TString allTableName;
  allTableName += "PIDTableP02gd.root";

  TStringLong cpCMD;
  cpCMD +="cp ";
  cpCMD +=eTableName.Data();
  cpCMD +=" ";
  cpCMD +=allTableName.Data();


  //  gSystem->Exec("cp electronPIDTable_1.02_.root PIDTableP01he_1.02_.root");
  gSystem->Exec(cpCMD.Data());



  //production Tag

  TObjString* productionTag = new TObjString("P02gd");

  TFile* table = new TFile (allTableName.Data(),"UPDATE");
  //  table->SetFormat(1);

  TFile* piFile = new TFile("PiPIDTable.root","R");
  piFile->cd();

  TVectorD* piAmp = (TVectorD *) piFile->Get("PiAmp");
  TVectorD* piCenter = (TVectorD *) piFile->Get("PiCenter");
  TVectorD* piSigma = (TVectorD *) piFile->Get("PiSigma");

  TFile* eFile = new TFile("EPIDTable.root","R");
  eFile->cd();

  TVectorD* eAmp = (TVectorD *) eFile->Get("EAmp");
  TVectorD* eCenter = (TVectorD *) eFile->Get("ECenter");
  TVectorD* eSigma = (TVectorD *) eFile->Get("ESigma");


 TFile* kFile = new TFile("KPIDTable.root","R");
  kFile->cd();

  TVectorD* kAmp = (TVectorD *) kFile->Get("KAmp");
  TVectorD* kCenter = (TVectorD *) kFile->Get("KCenter");
  TVectorD* kSigma = (TVectorD *) kFile->Get("KSigma");

 TFile* pFile = new TFile("PPIDTable.root","R");
  pFile->cd();

  TVectorD* pAmp = (TVectorD *) pFile->Get("PAmp");
  TVectorD* pCenter = (TVectorD *) pFile->Get("PCenter");
  TVectorD* pSigma = (TVectorD *) pFile->Get("PSigma");


  table->cd();

  piAmp->Write("piAmp",TObject::kOverwrite | TObject::kSingleKey);
  piCenter->Write("piCenter",TObject::kOverwrite | TObject::kSingleKey);
  piSigma->Write("piSigma",TObject::kOverwrite | TObject::kSingleKey);
  
  kAmp->Write("kAmp",TObject::kOverwrite | TObject::kSingleKey);
  kCenter->Write("kCenter",TObject::kOverwrite | TObject::kSingleKey);
  kSigma->Write("kSigma",TObject::kOverwrite | TObject::kSingleKey);

  pAmp->Write("pAmp",TObject::kOverwrite | TObject::kSingleKey);
  pCenter->Write("pCenter",TObject::kOverwrite | TObject::kSingleKey);
  pSigma->Write("pSigma",TObject::kOverwrite | TObject::kSingleKey);


  eAmp->Write("eAmp",TObject::kOverwrite | TObject::kSingleKey);
  eCenter->Write("eCenter",TObject::kOverwrite | TObject::kSingleKey);
  eSigma->Write("eSigma",TObject::kOverwrite | TObject::kSingleKey);

  productionTag->Write("productionTag",TObject::kOverwrite | TObject::kSingleKey);

  table->Write();
  table->Close();

}
