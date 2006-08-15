// $Id: StarView.C,v 1.3 2006/08/15 21:43:15 jeromel Exp $
// $Log: StarView.C,v $
// Revision 1.3  2006/08/15 21:43:15  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.2  1999/05/21 15:33:54  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
//*-- Author :    Valery Fine   28/08/98  (E-mail: fine@bnl.gov)
{
 // To run this example one needs the access to the "rhic" node of afs (/afs/rhic.bnl.gov/star . . .)
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe StarView.C
 //
  gROOT->Reset();
  Bool_t NT=kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT") == 0 ) NT=kTRUE; 
  const Char_t *viewfile = 0;
  if (NT) viewfile = gSystem->ExpandPathName("$(AFS_RHIC)/star/packages/dev/params/geometry/year2a_hadron.root");
  else    viewfile = gSystem->ExpandPathName("/afs/rhic.bnl.gov/star/packages/dev/params/geometry/year2a_hadron.root");
  TCanvas Star("Star","Star",400,600);
  gBenchmark->Start("read");
  TFile f(viewfile);
  gBenchmark->Show("read");
  year2a_hadron.Draw();
// Now, create ROOT browser to inspect the geometry
  TBrowser b;
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/browser.gif"> </P> End_Html //
//  Call TPad::x3d() method to create 3D view with the "default" 3D viewer
//  Under UNIX it is X3D package
//  Under Windows NT/95 - OpenGL
  if (NT) Star->x3d();
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/year2a_hadron.gif"> </P> End_Html // 
}
