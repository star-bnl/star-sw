void SecGain2(){
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t pad;
    Float_t Gain1;
    Float_t Gain2;
  };
  Gain_t G;
  gROOT->LoadMacro("Load.C");
  Load();
  TFile *f = new TFile("tpcGain.20030101.root");
  St_tpcGain *gain1 = (St_tpcGain *) gDirectory->Get("tpcGain");
  delete f;
  TFile *f = new TFile("tpcGain.20030129000001.root");
  St_tpcGain *gain2 = (St_tpcGain *) gDirectory->Get("tpcGain");
  delete f;
  f = new TFile("SecGain2.root","RECREATE");
  ntuple = new TNtuple("GainNt","TpcSecRow content","sector:row:pad:Gain1:Gain2");
  tpcGain_st *g1 = gain1->GetTable();
  tpcGain_st *g2 = gain2->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<gain1->GetNRows(); sector++,g1++,g2++) {
    for (int row = 0; row<45; row++) {
      for (int pad = 0; pad < 182; pad++) {
	G.sector = sector+1;
	G.row    = row+1;
	G.pad    = pad+1;
	G.Gain1   = g1->Gain[row][pad];
	G.Gain2   = g2->Gain[row][pad];
	ntuple->Fill(&G.sector);
	if (G.Gain1 == 0  && G.Gain2 == 0) continue;
	N++;
	if (G.Gain1 <= 0  || G.Gain2 <= 0) { 
	  NBad++;
	  printf("%10i/%10i sector %10.0f row %10.0f pad %10.0f gain1,2 %f\t%f\n",
		 N,NBad,G.sector,G.row,G.pad,G.Gain1,G.Gain2);
	}
      }
    }
  }
  f->Write();
  delete f;
}
