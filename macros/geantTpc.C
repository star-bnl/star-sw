// $Id: geantTpc.C,v 1.5 2015/09/21 20:36:58 fisyak Exp $
//=======================================================================
// owner: Pavel Nevski
// what it does: 
//=======================================================================
//
//#define gtrack
TBrowser *b = 0;
class St_geant_Maker;
St_geant_Maker *geant=0;
void geantTpc(const Int_t Nevents=99999,
	      const Char_t *fzfile ="/star/rcf/simu/UU200/hijing_382/minbias/b0_22/y2012/gheisha_on/gstardata/rcf12002_1_100evts.fzd") {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"fzin sim_T globT detDb gen_T nodefault -db",fzfile);
  Int_t i=0;
  TFile *fOut = new TFile("geantTpcL.root","recreate");
  TH2D *zR = new TH2D("zR","hit row versus z",440,-220,220,45,0.5,45.5);
  TH2D *tofR = new TH2D("tofR","prompt hit Log_{10} tof (ns) versus row",45,0.5,45.5,100,0,5);
  TProfile *io[2];
  io[0] = new TProfile("lgamInner","lgam for Inner Sector versus z",210,-210,210);
  io[1] = new TProfile("lgamOuter","lgam for Outer Sector versus z",210,-210,210);
  for (Int_t i =1; i <= Nevents; i++){
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    St_g2t_tpc_hit *TpcHit = (St_g2t_tpc_hit *) chain->FindObject("g2t_tpc_hit");
    if (! TpcHit) continue;
    g2t_tpc_hit_st *hit = TpcHit->GetTable();
    for (Int_t j = 0; j < TpcHit->GetNRows(); j++,hit++) {
      if (hit->volume_id <= 0 || hit->volume_id > 1000000) {
	cout << "volume_id: " << hit->volume_id << endl;
	continue;
      }
      if (TMath::Abs(hit->x[2]) < 1e-7) continue;
      zR->Fill(hit->x[2],hit->volume_id%100);
      if (hit->ds < 0 || hit->de < 0) continue;
      Int_t iop = 0;
      Int_t row = hit->volume_id%100;
      if (row > 13) iop = 1;
      io[iop]->Fill(hit->x[2],hit->lgam);
      if (TMath::Abs(hit->x[2]) > 200 && hit->tof>0) tofR->Fill(row,TMath::Log10(1e9*hit->tof));
    }
    //    printf ("=========================================== Done with Event no. %d\n",i);
  }
  fOut->Write();
}
