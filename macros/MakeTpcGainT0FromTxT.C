struct BPoint_t {
  Int_t sec, row, pad;
  Float_t gain, t0;
};
void MakeTpcGainT0FromTxT(const Char_t *fname = "tpc_gains.20071127.151801") {
  FILE *fp = fopen(fname,"r");
  if (! fp) {
    cout << "Can't open" << fname << endl;
    return;
  }
  TString TimeStamp(fname);
  TimeStamp.ReplaceAll("tpc_gains","");
  gSystem->Load("libStDb_Tables");
  St_tpcGain *G = new St_tpcGain("tpcGain",24);
  St_tpcT0   *T = new St_tpcT0("tpcT0",24);
  tpcGain_st  gain;
  tpcT0_st    t0;
  Char_t line[121];
  BPoint_t P;
  Int_t i = 0;
  Int_t secOld = -1;
  memset (&gain, 0, sizeof(gain));
  memset (&t0, 0, sizeof(t0));
  while (fgets(&line[0],120,fp)) {
    Int_t n = sscanf(&line[0],"%i %i %i %f %f",&P.sec, &P.row, &P.pad, &P.gain, &P.t0);
    i++;
    if (i %100 == 1) {
      printf("read %i %i %i %f %f",P.sec, P.row, P.pad, P.gain, P.t0);
      printf(": from %s",line);
    }
    if (secOld > 0 && P.sec != secOld) {
      G->AddAt(&gain,secOld-1);
      T->AddAt(&t0,secOld-1);
      memset (&gain, 0, sizeof(gain));
      memset (&t0, 0, sizeof(t0));
    }
    secOld = P.sec;
    gain.Gain[P.row-1][P.pad-1] = P.gain;
    t0.T0[P.row-1][P.pad-1] = P.t0;
  }
  G->AddAt(&gain,secOld-1);
  T->AddAt(&t0,secOld-1);
  G->Print(0,24);
  T->Print(0,24);
  TFile *f = new TFile(Form("tpcGain%s.root",TimeStamp.Data()),"recreate");
  G->Write();
  delete f;
  TFile *f = new TFile(Form("tpcT0%s.root",TimeStamp.Data()),"recreate");
  T->Write();
  delete f;
}
