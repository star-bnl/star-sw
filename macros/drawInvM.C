TLine* drawLine(Double_t xlow, Double_t ylow, Double_t xup, Double_t yup, Double_t lineWidth, Int_t lineColor)
{
  TLine *l1 = new TLine(xlow, ylow, xup, yup);
  l1->SetLineWidth(lineWidth);
  l1->SetLineColor(lineColor);
  l1->Draw("same");
  return l1;
}

void drawLines(Double_t xlow, Double_t ylow, Double_t xup, Double_t yup, Double_t lineWidth, Int_t lineColor)
{
  drawLine(xlow, ylow, xup, ylow, lineWidth, lineColor);
  drawLine(xlow, yup, xup, yup, lineWidth, lineColor);
  drawLine(xlow, ylow, xlow, yup, lineWidth, lineColor);
  drawLine(xup, ylow, xup, yup, lineWidth, lineColor);
}

void drawInvM()
{
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetOptDate(0);

  TCanvas* c1 = new TCanvas();
  c1->SetLeftMargin(0.14);
  c1->SetBottomMargin(0.14);
  c1->SetGrid(0,0);

  TFile *f1 = new TFile("mustafa_400_583.d0Hist.root");
  THnF* hD0CentPtEtaMDphi = (THnF*) f1->Get("hD0CentPtEtaMDphi");
  THnF* hD0CentPtEtaMDphiLikeSign = (THnF*) f1->Get("hD0CentPtEtaMDphiLikeSign");
  THnF* hD0CentPtEtaMDphiMixed = (THnF*) f1->Get("hD0CentPtEtaMDphiMixed");

  hD0CentPtEtaMDphi->GetAxis(1)->SetRangeUser(1. + 1e-6, 10. - 1e-6);
  hD0CentPtEtaMDphi->GetAxis(0)->SetRange(1, 9);
  hD0CentPtEtaMDphiLikeSign->GetAxis(1)->SetRangeUser(1. + 1e-6, 10. - 1e-6);
  hD0CentPtEtaMDphiLikeSign->GetAxis(0)->SetRange(1, 9);
  hD0CentPtEtaMDphiMixed->GetAxis(1)->SetRangeUser(1. + 1e-6, 10. - 1e-6);
  hD0CentPtEtaMDphiMixed->GetAxis(0)->SetRange(1, 9);

  TH1D* hInvM = hD0CentPtEtaMDphi->Projection(3);
  hInvM->Sumw2();
  hInvM->SetMarkerStyle(20);
  hInvM->SetLineWidth(2);
  hInvM->GetXaxis()->SetRangeUser(1.70001, 2.1-0.0001);
  hInvM->GetYaxis()->SetTitle("counts/(10 MeV/c^{2}) [#times10^{3}]");
  hInvM->GetXaxis()->SetTitle("m_{K#pi} (GeV/c^{2})");
  //  hInvM->GetXaxis()->SetTitleSize(0.08);
  //  hInvM->GetYaxis()->SetTitleSize(0.08);
  //  hInvM->GetXaxis()->SetLabelSize(0.08);
  //  hInvM->GetYaxis()->SetLabelSize(0.08);
  hInvM->GetYaxis()->SetTitleOffset(1.7);

  TH1D* hInvMLikeSign = hD0CentPtEtaMDphiLikeSign->Projection(3);
  hInvMLikeSign->Sumw2();
  hInvMLikeSign->SetLineWidth(2);
  hInvMLikeSign->SetLineColor(2);
  hInvMLikeSign->SetMarkerColor(2);

  TH1D* hInvMMixed = hD0CentPtEtaMDphiMixed->Projection(3);
  int bin1 = hInvM->GetXaxis()->FindBin(1.70001);
  int bin2 = hInvM->GetXaxis()->FindBin(2.1-0.0001);
  float scale = hInvMLikeSign->Integral(bin1, bin2) / hInvMMixed->Integral(bin1, bin2);
  hInvMMixed->Sumw2();
  hInvMMixed->Scale(scale);
  hInvMMixed->SetLineWidth(2);
  hInvMMixed->SetLineColor(4);
  hInvMMixed->SetMarkerColor(4);

  TH1D* hInvMSub = hInvM->Clone("hInvMSub");
  hInvMSub->Add(hInvMMixed, -1);
  hInvMSub->Draw("e");

  int minSignalMassBin = hInvMSub->GetXaxis()->FindBin(1.82+0.00001);
  int maxSignalMassBin = hInvMSub->GetXaxis()->FindBin(1.91-0.00001);
  TF1* fMass = new TF1("fMass", "[0]/sqrt(2.*TMath::Pi())/[2]*exp(-(x-[1])*(x-[1])/2./[2]/[2])+[3]+[4]*(x-1.865)", 1.71, 2.02);
  double roughCounts = hInvMSub->Integral(minSignalMassBin, maxSignalMassBin);
  fMass->SetParameter(0, roughCounts*hInvMSub->GetBinWidth(1));
  fMass->SetParameter(1, 1.86);
  fMass->SetParameter(2, 0.016);

  hInvMSub->Fit(fMass);

  float yield = fMass->GetParameter(0)/hInvMSub->GetBinWidth(1);
  float yieldError = fMass->GetParError(0)/hInvMSub->GetBinWidth(1);
  
  hInvM->GetYaxis()->SetRangeUser(0.0001, 2400);
  
  hInvM->Draw("pe");
  hInvMLikeSign->Draw("esame");
  hInvMMixed->Draw("histsame");
  hInvM->Draw("psame");

  char tex[256];
  TLatex tx;
  tx.SetNDC();
  tx.SetTextFont(42);
  //  tx.SetTextSize(0.08);
  tx.DrawLatex(0.17, 0.82, "Au+Au 200 GeV 0-80 % MB, 50 M events");
  tx.DrawLatex(0.17, 0.72, "1 < p_{T} < 10 GeV/c");
  sprintf(tex, "num. of D^{0}: %4.0f #pm %3.0f", yield, yieldError);
  tx.DrawLatex(0.52, 0.72, tex);
  sprintf(tex, "significance: %2.1f", yield/yieldError);
  tx.DrawLatex(0.52, 0.62, tex);
  drawLines(1.7, 0, 2.1, 2400, 2, 1);

  TLegend *leg = new TLegend(0.16, 0.14, 0.55, 0.38);
  leg->SetFillColor(10);
  leg->SetBorderSize(0);
  //  leg->SetTextFont(42);
  //  leg->SetTextSize(0.08);
  leg->AddEntry(hInvM, "unlike-sign, same event", "pl");
  leg->AddEntry(hInvMLikeSign, "like-sign, same event", "le");
  leg->AddEntry(hInvMMixed, "unlike-sign, mixed event", "l");
  leg->Draw("same");

  c1->SaveAs("invariantMass.eps");
  c1->SaveAs("invariantMass.gif");
}
