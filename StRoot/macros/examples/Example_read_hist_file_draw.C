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
