// $Id: Example_read_hist_file_to_ps.C,v 1.2 1999/05/21 15:33:48 kathy Exp $
// $Log: Example_read_hist_file_to_ps.C,v $
// Revision 1.2  1999/05/21 15:33:48  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: 
//=======================================================================
//read root histogram file and send histograms to a postscript file
// separate paper for each hist.
// in TPostScript set 
//  *-*     111 ps  Portrait
//  *-*     112 ps  Landscape
//  *-*     113 eps
//  *-*
//
{
  // open root file
TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root");  
  // list contents
file1.ls();
  // create canvas
TCanvas QACanvas("CanvasName","Canvas Title",800,600);
  // set statistics on
gStyle->SetOptStat(111111);
  // set paper size
Int_t width = 21;
Int_t height = 27;
gStyle->SetPaperSize(width, height);
  // define output ps file 
TPostScript ps("kathy.ps",111);
  //range of figures on ps page
ps.Range(20,26);
//ps.Range(16,24);
  // draw hist 
QaGlobtrkNPoint->Draw();
  // update (don't have to do if typing at command line(?)
gPad->Update();
  // draw hist
QaDstV0VertexK0Mass->Draw();
  // update 
gPad->Update();
  // close output file
ps.Close();
}


