//#define SCALE
TFile *f = 0;

void SecGain(){
  struct Gain_t {
    Float_t sector;
    Float_t row;
    Float_t Gain;
  };
  Gain_t G;
  gROOT->LoadMacro("Load.C");
  Load();
#if 1  
  St_TpcSecRowCor *gain = (St_TpcSecRowCor *) gDirectory->Get("TpcSecRowB");
#else
  gROOT->LoadMacro("TpcSecRow.20000701.120000.C");
  St_TpcSecRowCor *gain = (St_TpcSecRowCor *) CreateTable();
#endif
#ifndef SCALE
  f = new TFile("SecRowGain.root","RECREATE");
  ntuple = new TNtuple("GainNt","TpcSecRow content","sector:row:Gain");
#else
  f = new TFile("./TpcSecRowTest.root","RECREATE");
#endif
  TpcSecRowCor_st *g = gain->GetTable();
  Int_t N=0, NBad = 0;
  for (int sector=0;sector<gain->GetNRows(); sector++,g++) {
    for (int row = 0; row<45; row++) {
#ifdef SCALE
      g->GainScale[row] /= 7.38371e-01;
#else
      G.sector = sector+1;
      G.row    = row+1;
      G.Gain   = g->GainScale[row];
      ntuple->Fill(&G.sector);
      N++;
      if (G.Gain < 0) { 
	NBad++;
	printf("%10i/%10i sector %10.0f row %10.0f gain %f\n",
	       N,NBad,G.sector,G.row,G.Gain);
      }
#endif
    }
  }
#ifdef SCALE
  gain->Write();
#endif
  f->Write();
  delete f;
}
