void  PedestalsRHICvsLocalClock() {
  const Char_t *Names[8] = {
    "st_pedestal_11055019_raw_1080001.root",  "RHIC  clock, with GG",
    "st_pedestal_11055020_raw_1080001.root",  "Local clock, with GG", 
    "st_pedestal_11055022_raw_1080001.root",  "RHIC  clock, no GG",       
    "st_pedestal_11055023_raw_1080001.root",  "Local clock, no GG"};
  TFile *files[4];
  for (Int_t f = 0; f < 4; f++) {
    files[f] = (TFile *) gROOT->GetListOfFiles()->FindObject(Names[2*f]);
    if (! files[f]) {
      files[f] = TFile::Open(Names[2*f]);
      cout << Names[2*f] << "\t" << Names[2*f+1];
      if (files[f]) cout << " has been opened" << endl;
      else          cout << " has not been opened" << endl;
    }
  }
  TLegend *leg = new TLegend(0.2,0.2, 0.6, 0.5);
  TNtuple  *Peds = 0;
  TProfile *InOut[2][4];
  gStyle->SetOptStat(0);
  const Char_t *InnerOuter[2] = {"Inner",  "Outer"};
  const Char_t *Cut[2]        = {"row<=13","row>13"};
  for (Int_t i = 0; i < 2; i++) {
    Int_t color = 1;
    for (Int_t f = 0; f < 4; f++) {
      files[f]->cd();
      InOut[i][f] = (TProfile *) gDirectory->Get(InnerOuter[i]);
      if (! InOut[i][f]) {
	Peds = (TNtuple *) gDirectory->Get("Peds");
	Peds->SetMarkerStyle(20+i);
	Peds->SetMarkerColor(color++);
	Peds->Draw(Form("ped:tb>>%s(512,-0.5,511.5);",InnerOuter[i]),Cut[i],"prof");
	InOut[i][f] = (TProfile *) gDirectory->Get(InnerOuter[i]);
      }
      leg->AddEntry(InOut[i][f],Form("%s %s",InnerOuter[i],Names[2*f+1]));
    }
  }
  TString same("");
  for (Int_t i = 0; i < 2; i++) {
    for (Int_t f = 0; f < 4; f++) {
      InOut[i][f]->Draw(same);
      same = "same";
    }
  }
  leg->Draw();
}
