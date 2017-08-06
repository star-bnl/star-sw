/*
  root.exe -q -b -x geantHft.C >& geantHft.log &
 */
void geantHft(const Int_t Nevents=99999,
	      const Char_t *fileIn ="*.geant.root") {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in,sim_T,nodefault,quiet",fileIn);
  Int_t i=0;
  TFile *fOut = new TFile("geantHftL.root","recreate");
  TH2D *zI = new TH2D("zI","sector vs sensor (ladder == 1)",10,0.5,10.5,10,0.5,10.5);
  zI->SetXTitle("sensor");
  zI->SetYTitle("sector");
  TH2D *zO = new TH2D("zO","3*(sector-1)+(ladder-1) vs sensor",10,0.5,10.5,30,0.5,30.5);
  zO->SetXTitle("sensor");
  zO->SetYTitle("3*(sector-1)+(ladder-1)");
  for (Int_t i = 0; i < Nevents; i++){
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    St_g2t_pix_hit *pxlHit = (St_g2t_pix_hit *) chain->FindObject("g2t_pix_hit");
    if (! pxlHit) continue;
    g2t_pix_hit_st *hit = pxlHit->GetTable();
    for (Int_t j = 0; j < pxlHit->GetNRows(); j++,hit++) {
      Int_t volumeID = hit->volume_id;
      Int_t sector   =  volumeID/1000000;
      Int_t ladder   = (volumeID/10000  )%100;
      Int_t sensor   = (volumeID/100    )%100;
#if 0
      if (hit->volume_id <= 0 || hit->volume_id > 1000000) {
	cout << "volume_id: " << hit->volume_id << endl;
	continue;
      }
#endif
      Int_t sl = -1;
      if (ladder == 1) zI->Fill(sensor,sector);
      else             zO->Fill(sensor,3*(sector-1)+(ladder-1));
    }
    if (! i%100) cout << "=========================================== Done with Event no. " << i << endl;
  }
  fOut->Write();
}
