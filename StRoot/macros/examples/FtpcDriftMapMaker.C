// $Id: FtpcDriftMapMaker.C,v 1.2 2001/01/19 15:00:07 jcs Exp $
// $Log: FtpcDriftMapMaker.C,v $
// Revision 1.2  2001/01/19 15:00:07  jcs
// replace the default option ("Normal") with "FullField"
//
// Revision 1.1  2001/01/02 13:02:06  jcs
// macro to run standalone maker StFtpcDriftMapMaker
//
//======================================================================
// owner:  Janet Seyboth  (jcs@mppmu.mpg.de)
// what it does: compute drift map for FTPCs
//
//   
//======================================================================

void FtpcDriftMapMaker(const Char_t *FieldType="FullField")
{
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StMagF");
    gSystem->Load("libftpc_Tables");
    gSystem->Load("StFtpcDriftMapMaker");
    gSystem->Load("StFtpcClusterMaker");

  //  Create the makers to be called by the current chain
  const char *mainDB =  "MySQL:StarDb";
  const char *userDB = "$STAR/StarDb";
  StChain *chain =  new StChain();
//St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  St_db_Maker *dbMk = new St_db_Maker("db",userDB);
//StMagFMaker *magFMk = new StMagFMaker("magF");
  Float_t Scale = 1.0;
  TString FieldName("STAR full field");
  if (FieldType == "FieldOff")     {Scale = 0.00002; FieldName = "STAR no field";}
  if (FieldType == "HalfField")    {Scale = 0.5;     FieldName = "STAR Normal field";}
  if (FieldType == "ReverseField") {Scale = - Scale; FieldName += " Reverse";}
  StMagFC *magFCMk = new StMagFC("field",FieldName.Data(),Scale);
  StFtpcDriftMapMaker *ftpcDriftMapMk = new StFtpcDriftMapMaker("ftpcDriftMap");
  dbMk->SetDebug();
//magFCMk->SetDebug();
  ftpcDriftMapMk->SetDebug();
  chain->SetDebug();
  chain->Init();
  chain->Make();
}

