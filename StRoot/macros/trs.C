// $Id: trs.C,v 1.4 1999/02/05 16:31:43 fine Exp $
// $Log: trs.C,v $
// Revision 1.4  1999/02/05 16:31:43  fine
// StarGeom.C macro has been improved
//
// Revision 1.3  1999/01/28 00:11:55  fisyak
// add g2r
//
// Revision 1.2  1999/01/23 18:38:53  fisyak
// Cleanup for SL98l
//
// Revision 1.1  1998/11/12 22:27:10  fisyak
// Add trs.C
//
// Revision 1.24  1998/11/07 02:45:05  fisyak
// cleanup analysis
//
// Revision 1.23  1998/11/06 01:42:18  fisyak
// Add ana.C
//
// Revision 1.22  1998/11/01 16:42:28  fisyak
// dst analysis
//
// Revision 1.21  1998/10/31 00:26:26  fisyak
// Makers take care about branches
//
// Revision 1.20  1998/10/21 20:30:56  fine
// makedoc macro creates "gif" directories and fill it up
//
// Revision 1.19  1998/10/18 21:20:49  fisyak
// typo
//
// Revision 1.18  1998/10/12 00:53:02  fisyak
// Add parameters for trs
//
// Revision 1.17  1998/09/27 01:24:22  fisyak
// trs.C for whole file
//
// Revision 1.16  1998/09/26 00:35:31  fisyak
// Add real files
//
// Revision 1.15  1998/09/26 00:17:27  fisyak
// Add SetWrite
//
// Revision 1.13  1998/09/23 20:23:23  fisyak
// Prerelease SL98h
//
// Revision 1.12  1998/09/18 14:35:33  fisyak
// Fix makers
//
// Revision 1.11  1998/09/15 20:55:35  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.10  1998/08/26 12:15:15  fisyak
// Remove asu & dsl libraries
//
// Revision 1.9  1998/08/20 12:33:32  fisyak
// Splitted base libraries
//
// Revision 1.8  1998/08/18 14:05:08  fisyak
// Add to trs dst
//
// Revision 1.7  1998/08/10 02:35:13  fisyak
// add laser
//
// Revision 1.6  1998/07/23 11:32:42  fisyak
// Small fixes
//
// Revision 1.5  1998/07/21 13:35:14  fine
// The new version of the macros: MakeHtmlTables and makedoc have been introduced
//
// Revision 1.4  1998/07/21 01:04:41  fisyak
// Clean up
//
// Revision 1.3  1998/07/21 00:36:49  fisyak
// tcl and tpt
//
// Revision 1.2  1998/07/20 15:08:19  fisyak
// Add tcl and tpt
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StSclRoot");
  gSystem->Load("St_params_Maker");
  gSystem->Load("geometry");
  gSystem->Load("g2r");
  gSystem->Load("St_g2r");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("StTrsMaker");
}
void trs(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  St_params_Maker  *params = new St_params_Maker("params","params");
  //  St_TLA_Maker       *geom = new St_TLA_Maker("geom","run/geant/Run");
  geant = new St_geant_Maker("geant","event/geant/Event");
  geant->SetNwGEANT(20 000 000);
  //  geant->SetNwPAW(1000000);
  geant->SetIwtype(1);
  geant->Do("gfile p /disk1/star/test/psc0049_08_40evts.fzd");
  //  geant->LoadGeometry("detp geometry field_only");
  StTrsMaker    *tpc_raw = new StTrsMaker("tpc_raw","event/raw_data/tpc");
  //  chain->PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("trs");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    St_DataSet *dst = chain->DataSet("dst");
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    gBenchmark->Print("trs");
  }
  else b = new TBrowser;
}
