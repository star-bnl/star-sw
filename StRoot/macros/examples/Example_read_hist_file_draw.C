// $Id: Example_read_hist_file_draw.C,v 1.3 1999/06/03 23:34:49 kathy Exp $
// $Log: Example_read_hist_file_draw.C,v $
// Revision 1.3  1999/06/03 23:34:49  kathy
// got macros working with current files
//
// Revision 1.2  1999/05/21 15:33:48  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//   You must first run  Example_read_xdffile_make_hist.C to create 
//     an output Kathy_hist.root file (flat file).  
//   This macro then reads in the histogram file and draws 
//     histogram h1.
//   This will not work for *.hist.root files produced from bfc.C since
//    they have a tree directory structure.
//=======================================================================
// read_hist_file_draw.C
//
// - reads in root histogram file (e.g. Kathy_hist.root) and draws hist h1
//
{
TFile f1("Kathy_hist.root");
f1.ls();
gStyle->SetOptStat(111111);
h1->Draw();
}
