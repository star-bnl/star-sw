// $Id: StarWebView.C,v 1.4 2000/04/07 17:07:08 fine Exp $
// $Log: StarWebView.C,v $
// Revision 1.4  2000/04/07 17:07:08  fine
// adjusted to the ROOT 2.24
//
// Revision 1.3  1999/05/21 15:33:54  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
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
  if (NT) gSystem->Load("ROOT_STAR");
  else  gSystem->Load("libSTAR");
  if (!gGeometry) new TGeometry;
  TCanvas Star("Star","Star",400,600);
  // Pick the geomtery file from the remote Web site at CERN: http://root.cern.ch
  gBenchmark->Start("pick");
  TWebFile f("http://www.star.bnl.gov/~fine/star_year_2a.root");
  gBenchmark->Show("pick");
  // List first 3 levels of the geometry
  HALL->ls(3);
  // Create an TPad view of "ECAL" from this geometry if any
  HALL->FindByName("ECAL")->Draw();
  // Now create ROOT browser to inspect the geometry
  TBrowser b("STAR",HALL);
  // Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/browser.gif"> </P> End_Html //
  //  Call TPad::x3d() method to create 3D view with the "default" 3D viewer
  //  Under UNIX it is X3D package
  //  Under Windows NT/95 - OpenGL
  if (NT) Star.x3d();
  //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/year2a_hadron2.gif"> </P> End_Html // 
}
