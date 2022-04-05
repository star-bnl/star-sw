// $Id: geant.C,v 1.14 2001/05/30 18:24:59 perev Exp $
//=======================================================================
// owner: Pavel Nevski
// what it does: 
//=======================================================================
//
//#define gtrack
TBrowser *b = 0;
class St_geant_Maker;
St_geant_Maker *geant=0;
void geant(const Int_t Nevents=1,
	   const Char_t *fzfile ="/star/rcf/simu/cocktail/hadronic/default/lowdensity/year_1h/half_field/hadronic_on/Gstardata/hc_lowdensity.400_evts.fz")
{
  gROOT->LoadMacro("bfc.C");
  bfc(0,"fzin sim_T globT gen_T",fzfile);
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
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
       printf("eg: %d ge: %d pT %f eta %f  Ntpc %d\n",
       trk->eg_label,trk->ge_pid,pT,Eta,trk->n_tpc_hit);
     }
    printf ("=========================================== Done with Event no. %d\n",i);
  }
}
