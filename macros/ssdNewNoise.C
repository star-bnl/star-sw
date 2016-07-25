void ssdNewNoise(const Char_t *file = "$STAR/StarDb/Calibrations/ssd/ssdStripCalib.20070324.133453.root") {
  gSystem->Load("libStDb_Tables.so");
  TFile *f = new TFile(file);
  if (! f) return;
  St_ssdStripCalib *ssdStripCalib = (St_ssdStripCalib *) f->Get("ssdStripCalib");
  if (! ssdStripCalib) return;
  //  delete f;
  ssdNoise_st temp[320];
  memset(temp, 0, 320*sizeof(ssdNoise_st));
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t wafer  = 0;
  Char_t tempo = 0;
  ssdStripCalib_st *strip = ssdStripCalib->GetTable();
  Int_t N = ssdStripCalib->GetNRows();
  for (Int_t i=0; i< N; i++, strip++) {
    if (strip->id>0 && strip->id<=76818620) {
      nStrip  = (int)(strip->id/100000.);
      idWaf   = strip->id-10000*((int)(strip->id/10000.));
      iWaf    =  (int)((idWaf - 7000)/100 - 1);
      iLad    = (int)(idWaf - 7000 - (iWaf+1)*100 - 1);
      iSide   = (strip->id - nStrip*100000 - idWaf)/10000;
      wafer = iLad*16 +iWaf;
      if (iSide == 0) temp[iWaf].rmsp[nStrip-1] = strip->rms;
      if (iSide == 1) temp[iWaf].rmsn[nStrip-1] = strip->rms;
    }
  }
  St_ssdNoise *StripCal = new St_ssdNoise("ssdNoise",320);
  for(Int_t i=0;i<320;i++)    {
    temp[i].id = i;
    StripCal->AddAt(&temp[i]);
  }
  TString Out(gSystem->BaseName(file));
  Out.ReplaceAll("ssdStripCalib","ssdNoise");
  TFile f1(Out,"RECREATE");
  StripCal->Write();
  f1.Close();
}

