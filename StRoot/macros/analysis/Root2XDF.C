// $Id: Root2XDF.C,v 1.1 1999/12/02 00:57:51 fine Exp $
// $Log: Root2XDF.C,v $
// Revision 1.1  1999/12/02 00:57:51  fine
// Macro to convert dst file from the ROOT to XDF format
//

class StChain;
StChain *chain;

//=======================================================================
// owner: Valery Fine
// what it does: Converts files from the ROOT format into the XDF format
//=======================================================================

void Root2XDF(
  const char *MainFile=
  "/star/rcf/test/dev/tfs_Linux/Wed/year_2a/psc0208_01_40evts.dst.root",
  Int_t nevents=99999) 
{
  cout << "Usage: root.exe -b -q Root2XDF.C(\"rootfile name.dst.root\")" << endl
       << "       Converts the input ROOT file into the XDF format."     << endl
       << "       The output XDF file bears the same name as the input root file" << endl
       << "       and it is created under the current working directory" << endl
       << "       Upon creation one may check this file with "           << endl 
       << "                    \"root.exe XDFBrowser.C\""                << endl
       << "       ROOT command"                                          << endl; 

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("xdf2root");

//  Setup top part of chain
  chain = new StChain("bfc");
  chain->SetDebug();

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch  
// --- now execute chain member functions
  chain->Init();

  St_DataSet *ds=0;

  // Create output file 
  TString xdfOut = gSystem->BaseName(MainFile);
  xdfOut.ReplaceAll(".root",".xdf");
  St_XDFFile *xdf = new St_XDFFile;
  if (xdf && !xdf->OpenXDF(xdfOut.Data(),"w")) {
    // Loop over events
    int iret=0,iev=0;
    EventLoop: if (iev<nevents && !iret) {         // goto loop code
       chain->Clear();
       iret = chain->Make();
       iev++;                                      // goto loop code
       if (!iret) {
         ds=chain->GetDataSet("dst");
         if (ds) xdf->NextEventPut(ds);
       }
    goto EventLoop; }
  }
  else {  
    cerr << " Can not open: <" << xdfOut.Data() << ">" << endl;
    chain->Finish();
    return;
  };
  if (xdf) { xdf->CloseXDF(); delete xdf;} 
  if (iev) { 
    cout << endl << " *** Total: " << iev << " events have been written out" << endl << endl; 
    TString lsOut = "ls -l ";
    lsOut += xdfOut;
    gSystem->Exec(lsOut);
    cout << endl;
   cout   << " You may try to check this file with ROOT as follows: " << endl
          << " root.exe \'XDFBrowser.C(\""<<  xdfOut.Data() << "\")\'" << endl << endl;
  }
  else {
   cout << "no event has been writen" << endl;
  }
  chain->Finish();   
}
