void plTh1() {
  // char *nameUn="rb400_wp_unp_ct5m_pt25.root";
  //char *namePol="rb400_wp_pol_dssv08_pt25.root";
  char *nameUn="asyWALfinalAPS/deFlor_wp_unp_mrst02_pt25_full.root";
  char *namePol="asyWALfinalAPS/deFlor_wp_pol_dssv08_pt25_full.root";
  funp=new TFile(nameUn); assert(funp->IsOpen());
  fpol=new TFile(namePol); assert(fpol->IsOpen());

  hunp= (TH1F*)funp->Get("h3"); assert(hunp);

  hpol=(TH1F*) fpol->Get("h3"); assert(hpol);
  hal=(TH1F*) hpol->Clone(); hal->SetNameTitle("al","A_L vs. lepton eta");
  hal->Divide(hunp);//hal->Scale(-1.);
 c=new TCanvas();
  c->Divide(2,2);

  c->cd(1);hunp->Draw();
  c->cd(2);hpol->Draw();

  c->cd(3);hal->Draw(); hal->SetLineColor(kRed);;
  hal->SetMaximum(0.6);  hal->SetMinimum(-0.6);
}

