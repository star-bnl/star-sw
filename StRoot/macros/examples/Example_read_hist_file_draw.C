// $Id: Example_read_hist_file_draw.C,v 1.2 1999/05/21 15:33:48 kathy Exp $
// $Log: Example_read_hist_file_draw.C,v $
// Revision 1.2  1999/05/21 15:33:48  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: 
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
