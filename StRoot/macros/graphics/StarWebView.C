//*-- Author :    Valery Fine   28/08/98  (E-mail: fine@bnl.gov)
{
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe StarWebView.C
 //
 
  gROOT->Reset();
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) NT=kTRUE; 
  TCanvas Star("Star","Star",400,600);
  // Pick the geomtery file from the remote Web site at CERN: http://root.cern.ch
  gBenchmark->Start("pick");
  TWebFile f("http://root.cern.ch/files/star.root");
  gBenchmark->Show("pick");
  // Create an TPad view of this geometry
  year2a_hadron.Draw();
  // Now create ROOT browser to inspect the geometry
  TBrowser b;
  // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/browser.gif"> </P> End_Html //
  //  Call TPad::x3d() method to create 3D view with the "default" 3D viewer
  //  Under UNIX it is X3D package
  //  Under Windows NT/95 - OpenGL
  if (NT) Star.x3d();
  //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/year2a_hadron2.gif"> </P> End_Html // 
}
