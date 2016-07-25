TGraphErrors *omegaTau() {
  struct Meas_t {Double_t x, dx, y, dy;};
  Meas_t meas[6] = {  // Inner /Outer
    {0.0,0.01, 521.52 , 0.37}, // #mum   FieldOff_Data :#sigma_{C} =   0.26 +/-   0.00 cm,    #sigma_{D} =
    {1.0,0.01, 198.01 , 0.20}, // #mum	  FullFieldA_DataS :#sigma_{C} =   0.05 +/-   0.00 cm, #sigma_{D} =
    {0.5,0.01, 314.10 , 0.28}, // #mum	  HalfField_DataS :#sigma_{C} =   0.07 +/-   0.00 cm,  #sigma_{D} =
    {0.0,0.01, 650.18 , 0.11}, // #mum	  FieldOff_Data :#sigma_{C} =   0.21 +/-   0.00 cm,    #sigma_{D} =
    {1.0,0.01, 214.41 , 0.17}, // #mum	  FullFieldA_DataS :#sigma_{C} =   0.07 +/-   0.00 cm, #sigma_{D} =
    {0.5,0.01, 331.69 , 0.15}  // #mum	  HalfField_DataS :#sigma_{C} =   0.06 +/-   0.00 cm,  #sigma_{D} =
  };
  TGraphErrors *gr = new TGraphErrors();
  for (int i = 0; i < 6; i++) {
    gr->SetPoint(i,meas[i].x,meas[i].y);
    gr->SetPointError(i,meas[i].dx,meas[i].dy);
  }
  TF1 *W = new TF1("W","[0]/sqrt(1.+([1]*x)**2)");
  W->SetParName(0,"#sigma_{D} (#mum/#sqrt{cm})");
  W->SetParName(1,"#omega#tau(5kG)");
  W->SetParameters(650.,2.34);
  gr->GetHistogram()->SetXTitle("B(1/5kG)");
  gr->GetHistogram()->SetYTitle("#sigma_{D}(#mum/#sqrt{cm}");
  gr->Fit("W");
  gr->GetHistogram()->Draw();
  gr->Draw("p");
  return gr;
}
