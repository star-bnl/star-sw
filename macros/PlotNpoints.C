void PlotSigma() {
  FPointsSigmaFG->SetXTitle("Total no. of points");
  FPointsSigmaFG->SetYTitle("Resolution ln(I_{meas}/I_{#pi})");
  FPointsSigmaFG->GetXaxis()->SetRange(10,45);
  FPointsSigmaFG->SetMinimum(0.06);
  FPointsSigmaFG->SetStats(0);
  FPointsSigmaFG->Draw();
  Points70SigmaFG->SetMarkerColor(2);
  Points70SigmaFG->Draw("same");
  Points60SigmaFG->SetMarkerColor(3);
  Points60SigmaFG->Draw("same");
}

void PlotMu() {
  FPointsMuFG->SetXTitle("Total no. of points");
  FPointsMuFG->SetYTitle("Shift ln(I_{meas}/I_{#pi})");
  FPointsMuFG->GetXaxis()->SetRange(10,45);
  FPointsMuFG->SetMinimum(-.35);
  FPointsMuFG->SetMaximum(.05);
  FPointsMuFG->SetStats(0);
  FPointsMuFG->SetMarkerStyle(20);
  FPointsMuFG->Draw();
  Points70MuFG->SetMarkerColor(2);
  Points70MuFG->SetMarkerStyle(20);
  Points70MuFG->Draw("same");
  Points60MuFG->SetMarkerColor(3);
  Points60MuFG->SetMarkerStyle(20);
  Points60MuFG->Draw("same");
}
