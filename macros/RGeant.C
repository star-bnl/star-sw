// read geant branch and build histogram for diffent particle types
TFile *f = 0;
void RGeant(Int_t Last=10000) {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in sim_T globT gen_T",
      "/star/data07/reco/auau128/hijing/b0_12/halffield/year_1e/hadronic_on/tfs_7/rcf0139_*evts.geant.root");
  const Int_t NHYPS = 5;
  Char_t *Names[NHYPS] = {"p",
			  "K",
			  "pi",
			  "e",
			  "all"
  };
  const Int_t Neta = 26;
  const Double_t MinEta = -2.6;
  const Double_t dEta = 0.2;
  
  TH1F *hist[NHYPS][Neta];
  f = new TFile("Hijing.3.root","RECREATE");
  Double_t xx[] = {0.000, 0.050, 0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400,
		   0.450, 0.500, 0.550, 0.600, 0.700, 0.800, 0.900, 1.000, 1.100, 
		   1.200, 1.400, 1.600, 1.800, 2.000, 3.000, 5.00};
  Int_t nx = sizeof (xx)/ sizeof(Double_t);
  const Int_t n2x = 2*nx-1;
  //  Double_t *x = new (Double_t *) [2*nx-1];
  Double_t x[n2x];
  for (Int_t i = 0; i<2*nx-1; i++) {
    if (i < nx) x[i] = - xx[nx-i-1];
    else        x[i] =   xx[i-nx+1];
    //    printf ("i = %i x = %f\n",i,x[i]); 
  }
  Char_t title[80], name[20];
  for (Int_t hyp = 0; hyp< NHYPS; hyp++){
    for (Int_t j=0; j<Neta; j++) {
      sprintf(name,"%s%i",Names[hyp],j);
      Double_t EtaMin = MinEta + dEta*j;
      Double_t EtaMax = EtaMin + dEta;
      sprintf(title,"Fraction of %s for %+-5.1f < Eta <= %+-5.1f",Names[hyp],EtaMin,EtaMax);
      hist[hyp][j] = new TH1F(name,title,2*nx-2,x);
    }
  }
  Int_t iMake = 0, i = 0, iTotal = 0; 
 EventLoop: if (i <= Last && iMake != kStEOF && iMake != kStFatal) {
   chain->Clear();
   iMake = chain->Make(i);
   Int_t iAll = NHYPS-1;
   if (iMake <kStEOF) {
     St_g2t_track *track = (St_g2t_track *) chain->FindObject("g2t_track");
     g2t_track_st *trk = track->GetTable();
     for (Int_t j = 0; j < track->GetNRows(); j++,trk++) {
       if (TMath::Abs(trk->charge) < 0.5) continue;
       if (! trk->eg_label) continue;
       if (trk->n_tpc_hit < 26) continue;
       hyp = -1;
       if (trk->ge_pid == 2 || trk->ge_pid == 3) hyp = 3;
       if (trk->ge_pid == 8 || trk->ge_pid == 9) hyp = 2;
       if (trk->ge_pid ==11 || trk->ge_pid ==12) hyp = 1;
       if (trk->ge_pid ==14 || trk->ge_pid ==15) hyp = 0;
       Double_t pT =  trk->charge*trk->pt;
       Double_t Eta = trk->eta;
       Int_t iEta = (Eta-MinEta)/dEta;
       if (iEta < 0 || iEta >= Neta) continue;
       //       printf("eg: %d ge: %d pT %f eta %f  iEta %i Ntpc %d\n",
       //       trk->eg_label,trk->ge_pid,pT,Eta,iEta,trk->n_tpc_hit);
       if (hyp >= 0) hist[hyp][iEta]->Fill(pT);
       hist[iAll][iEta]->Fill(pT);
     }
     iTotal++;
     printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d\n",
	     i,chain->GetRunNumber(),chain->GetEventNumber());
   }
   i++; goto EventLoop;
 }
 END:
  f->cd();
  f->Write();
  f->Flush();
  f->Close();
  f->Delete();
}
