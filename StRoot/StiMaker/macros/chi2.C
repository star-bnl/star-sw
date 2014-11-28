
void chi2()
{
  ifstream input("chi1.txt");
  double x[100];
  double y[100];
  double xx,yy;
  for (int i=0;i<100;++i)
    {
      input >> xx >> yy;
      x[i] = xx;
      y[i] = yy;
      //cout << x[i]<< " " << y[i] << endl;
      y[i]*=200.;
    }
  TGraph * g = new TGraph(100,x,y);
  //TH1D * h = new TH1D("h","h",10,0.,4.);
  //h->SetMinimum(0.00000001);
  //h->SetMaximum(1000.);
  //h->Draw();
  //g->Draw();

  R_chi2_L0.GetXaxis()->SetLabelSize(0.1);  R_chi2_L0.GetYaxis()->SetLabelSize(0.1); 
  R_chi2_L1.GetXaxis()->SetLabelSize(0.1);  R_chi2_L1.GetYaxis()->SetLabelSize(0.1); 
  R_chi2_L2.GetXaxis()->SetLabelSize(0.1);  R_chi2_L2.GetYaxis()->SetLabelSize(0.1); 
  R_chi2_L3.GetXaxis()->SetLabelSize(0.1);  R_chi2_L3.GetYaxis()->SetLabelSize(0.1); 
  R_chi2_L4.GetXaxis()->SetLabelSize(0.1);  R_chi2_L4.GetYaxis()->SetLabelSize(0.1); 
  R_chi2_L5.GetXaxis()->SetLabelSize(0.1);  R_chi2_L5.GetYaxis()->SetLabelSize(0.1); 

  c1->Clear();
  c1->Divide(1,3);
  c1->cd(1);  c1.GetPad(1).SetLogy(1);  R_chi2_L0.Draw(); g->Draw();
  c1->cd(2);  c1.GetPad(2).SetLogy(2);  R_chi2_L1.Draw(); g->Draw();
  c1->cd(3);  c1.GetPad(3).SetLogy(3);  R_chi2_L2.Draw(); g->Draw();
  c1->Print("svtIncChi2a.gif");
  c1->Clear();
  c1->Divide(1,3);
  c1->cd(1);  c1.GetPad(1).SetLogy(1);  R_chi2_L3.Draw(); g->Draw();
  c1->cd(2);  c1.GetPad(2).SetLogy(1);  R_chi2_L4.Draw(); g->Draw();
  c1->cd(3);  c1.GetPad(3).SetLogy(1);  R_chi2_L5.Draw(); g->Draw();
  c1->Print("svtIncChi2b.gif");
}
