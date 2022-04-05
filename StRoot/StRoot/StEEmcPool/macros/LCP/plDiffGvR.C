// plot difference for Geant vs. MuDst reco LCP

plDiffGvR(TString coreName="bbb_cut2") {
 gStyle->SetPalette(1,0);

  gStyle->SetOptStat(1111111);


  fname=coreName+".hist.root";

  TFile *fd=new TFile(fname);
  assert(fd->IsOpen());

  //  fd->ls();

  //=====================  1D plot =============
  c1=new TCanvas(coreName,coreName,500,800);
  c1->Divide(2,4);

  c1->cd(1);
  GvR->Draw(); gPad->SetLogy();

  float sum0= dPhi->Integral();

  //................. phi ...........
  c1->cd(3);
  PhiR->Draw(); 
  c1->cd(4);
  dPhi->Draw(); gPad->SetLogy();

  hx=(TH1F*) dPhi->Clone();
  hx->SetAxisRange(-90,90);
  float phiS= hx->Integral();
   printf("<tr> <th> %s  <td>%d  <td>%.3f  ",coreName.Data(), phiS, 1.-phiS/sum0);
   //printf("## %s  %d  %.3f  ",coreName.Data(), phiS, 1.-phiS/sum0);
  
  //................. Eta ...........
  c1->cd(5);
  EtaR->Draw(); 
  c1->cd(6);
  dEta->Draw(); gPad->SetLogy();

  hx=(TH1F*) dEta->Clone();
  hx->SetAxisRange(-.5,.5);
  float etaS= hx->Integral();
  printf("<td>%d <td> %.3f  ", etaS, 1.-etaS/sum0);
  
  //................. Pt ...........
  c1->cd(7);
  PtR->Draw(); gPad->SetLogy(); gPad->SetGrid();
  c1->cd(8);
  dPt->Draw(); gPad->SetLogy();

  hx=(TH1F*) dPt->Clone();
  hx->SetAxisRange(-.5,.5);
  float ptS= hx->Integral();
  printf("<td>%d  <td>%.3f ", ptS, 1.-ptS/sum0);
  
  printf("<td> <a href=\"%s.ps\"> (PS)</a> , <a href=\"%s.gif\"> (GIF)</a> \n",coreName.Data(), coreName.Data());
  //=====================  2D plot =============
  c2=new TCanvas(coreName+"D",coreName+2,500,600);
  c2->Divide(1,3);

  c2->cd(1);
  dPhiPT->Draw("colz"); gPad->SetLogz(); 

  c2->cd(2);
  dEtaPT->Draw("colz"); gPad->SetLogz(); 

  c2->cd(3);
  dPtPT->Draw("colz"); gPad->SetLogz(); 

  return;
  c1->Print(coreName+".ps");
  c1->Print(coreName+".gif");
  c2->Print(coreName+"D.ps");
  c2->Print(coreName+"D.gif");

}

