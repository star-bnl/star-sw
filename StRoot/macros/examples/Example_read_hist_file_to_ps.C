// $Id: Example_read_hist_file_to_ps.C,v 1.4 1999/10/07 14:13:10 kathy Exp $
// $Log: Example_read_hist_file_to_ps.C,v $
// Revision 1.4  1999/10/07 14:13:10  kathy
// changes to Example macros to make them work in dev - mostly changes were correcting input file name
//
// Revision 1.3  1999/06/03 23:34:50  kathy
// got macros working with current files
//
// Revision 1.2  1999/05/21 15:33:48  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: 
//  read flat root histogram file and send histograms to a postscript file
//   - separate paper for each hist.
// 
//=======================================================================
//read root histogram file and send histograms to a postscript file
// separate paper for each hist.
// in TPostScript set 
//  *-*     111 ps  Portrait
//  *-*     112 ps  Landscape
//  *-*     113 eps
//  *-*
//
void Example_read_hist_file_to_ps(const char* filein="Kathy_hist.root")
{
  // open root file
  //TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root"); 
  //TFile file1("/disk00000/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/set0029_01_49evts.root");

TFile file1(filein);
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
TPostScript ps("kathynew.ps",111);
  //range of figures on ps page
ps.Range(20,26);
//ps.Range(16,24);
//.. must do the following 2 commands for each histogram:
  // draw hist 
h1->Draw();
//QaGlobtrkNPoint->Draw();
  // update (don't have to do if typing at command line(?)
gPad->Update();
//
  // close output file
ps.Close();
}


