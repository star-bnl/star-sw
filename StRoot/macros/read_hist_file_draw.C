// root_read_hist_file_draw.C
//
// - first use root_read_xdffile_make_hist.C to make a hist file (*.root)
// - then use this macro to read it back in and draw it
//
{
TFile f1("Kathy_hist.root");
f1.ls();
gStyle->SetOptStat(111111);
h1->Draw();
}
