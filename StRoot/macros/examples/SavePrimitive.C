// $Id: SavePrimitive.C,v 1.1 1998/12/12 02:38:42 fisyak Exp $
// $Log: SavePrimitive.C,v $
// Revision 1.1  1998/12/12 02:38:42  fisyak
// Clean up
//
// Revision 1.25  1998/11/25 21:58:36  fisyak
// Cleanup
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
// Add parameters for bfc
//
// Revision 1.17  1998/09/27 01:24:22  fisyak
// bfc.C for whole file
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
// Add to bfc dst
//
// Revision 1.7  1998/08/10 02:35:13  fisyak
// add laser
//
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("St_params_Maker");
  //  StChain chain("bfc");
  const Char_t *Path="/afs/rhic/star/packages/SL98k/StdB/";
  StChain chain("bfc");

//  Create the makers to be called by the current chain
  St_params_Maker params("params","run/params");
// Init the mai chain and all its makers
  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");
  St_DataSet *set=chain.DataSet("params");
  St_DataSetIter param(set);
  param.Cd("params");
  St_DataSet *t = 0;
  //  St_tss_tsspar *tsspar = (St_tss_tsspar *) param("params/tpc/tsspars/tsspar");
  //  tsspar->Print(0);
  TString path = Path;
  while (t = param()){
    path  = Path;
    path += t->Path();
    if (t->HasData()){ // Table
      path += ".C";
      ofstream *out = new ofstream(path.Data());
      cout << "Open " << path.Data() << endl;
      t->SavePrimitive(*out,"");
      delete out;
    }
    else { // DataSet
      if (gSystem->mkdir(path.Data())) {cout << "Directoty " << path << " creation failed" << endl;}
    }
  }
}
