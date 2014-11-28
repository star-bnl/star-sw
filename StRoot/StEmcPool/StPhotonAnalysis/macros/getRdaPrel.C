void getRdaPrel()
{
  //frank's spin2006 pions:
  ifstream frank("./datapoints/frank_pp05.dat");
  Float_t frx[100];
  Float_t fry[100];
  Float_t frex[100];
  Float_t frey[100];
  Int_t ifr=0;
  while(ifr<12){
    if(!frank.good()) break;
    frank>>frx[ifr]>>fry[ifr]>>frey[ifr];
    fry[ifr]*=0.85/26.1;
    frey[ifr]*=0.85/26.1;
    frex[ifr]=0.;
    ifr++;
  }
  TGraphErrors *frank_pp=new TGraphErrors(ifr,frx,fry,frex,frey);
  frank_pp->SetName("frank");
  frank_pp->SetMarkerStyle(25);
  frank_pp->SetMarkerColor(4);

  TGraphErrors *andre_dau = new TGraphErrors(20);
  andre_dau->SetName("andre");
  andre_dau->SetFillColor(1);
  andre_dau->SetMarkerStyle(20);
  andre_dau->SetMarkerSize(1.4);
  andre_dau->SetPoint(1,1.75,0.00500784);
  andre_dau->SetPointError(1,0,0.000146705);
  andre_dau->SetPoint(2,2.25,0.00178272);
  andre_dau->SetPointError(2,0,5.55874e-05);
  andre_dau->SetPoint(3,2.75,0.000527415);
  andre_dau->SetPointError(3,0,2.38637e-05);
  andre_dau->SetPoint(4,3.25,0.000173036);
  andre_dau->SetPointError(4,0,1.17154e-05);
  andre_dau->SetPoint(5,3.75,7.47168e-05);
  andre_dau->SetPointError(5,0,1.44958e-05);
  andre_dau->SetPoint(6,4.25,2.65451e-05);
  andre_dau->SetPointError(6,0,4.05473e-06);
  andre_dau->SetPoint(7,4.75,1.0569e-05);
  andre_dau->SetPointError(7,0,2.9797e-06);
  andre_dau->SetPoint(8,5.25,4.08643e-06);
  andre_dau->SetPointError(8,0,1.23376e-06);
  andre_dau->SetPoint(9,6,1.29798e-06);
  andre_dau->SetPointError(9,0,4.30548e-07);
  andre_dau->SetPoint(10,6.5,7.02737e-07);
  andre_dau->SetPointError(10,0,1.00577e-07);
  andre_dau->SetPoint(11,7.5,2.8363e-07);
  andre_dau->SetPointError(11,0,5.20925e-08);
  andre_dau->SetPoint(12,8.5,1.22991e-07);
  andre_dau->SetPointError(12,0,2.57462e-08);
  andre_dau->SetPoint(13,9.5,5.68615e-08);
  andre_dau->SetPointError(13,0,6.45742e-09);
  andre_dau->SetPoint(14,10.5,2.77471e-08);
  andre_dau->SetPointError(14,0,3.97804e-09);
  andre_dau->SetPoint(15,11.5,1.61125e-08);
  andre_dau->SetPointError(15,0,4.34288e-09);
  andre_dau->SetPoint(16,12.5,9.93835e-09);
  andre_dau->SetPointError(16,0,2.42936e-09);
  andre_dau->SetPoint(17,13.5,5.44627e-09);
  andre_dau->SetPointError(17,0,3.26254e-09);
  andre_dau->SetPoint(18,14.5,3.15326e-09);
  andre_dau->SetPointError(18,0,2.1746e-09);
  andre_dau->SetPoint(19,15.5,1.56401e-09);
  andre_dau->SetPointError(19,0,1.13746e-09);

  TMultiGraph *mgfit=new TMultiGraph();
  mgfit->Add(frank_pp);
  TF1 *fitQCD=new TF1("fitQCD","[0]*pow(1.+x,[1])*([2]+[3]*x+[4]*x*x+[5]*x*x*x)",1.5,16.);
  fitQCD->SetParameters(600.,-9.5,1.,0.,0.,0.);
  mgfit->Fit(fitQCD,"R0");

  TMultiGraph *m=new TMultiGraph("m","none;p_{T} (GeV/c);1/(2#piNp_{T}) dN/dydp_{T}");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  m->Add(andre_dau);
  m->Add(frank_pp);
  m->SetMaximum(.1);
  m->SetMinimum(1.e-11);
  
  TCanvas *c=new TCanvas("c","c",400,400);
  gPad->SetLogy();
  m->Draw("ap");
  fitQCD->SetLineWidth(1);
  fitQCD->Draw("same");

  TLegend *legg=new TLegend(.4,.6,.8,.8);
  legg->AddEntry(andre_dau,"d+Au preliminary","p");
  legg->AddEntry(frank_pp,"p+p preliminary","p");
  legg->Draw("same");
  
  c->SaveAs("rda_prel.pdf");

  TCanvas *c2=new TCanvas("c2","c2",400,150);
  TGraphErrors *andre_dau2=new TGraphErrors();
  andre_dau2->SetMarkerStyle(20);
  andre_dau2->SetMarkerSize(1.4);
  TGraphErrors *frank_pp2=new TGraphErrors();

  for(int i=0;i<19;i++){
    double xx=0.;
    double yy=0;
    double exx=0.;
    double eyy=0.;
    if(i<12){
      frank_pp->GetPoint(i,xx,yy);
      eyy=frank_pp->GetErrorY(i);
      frank_pp2->SetPoint(i,xx,yy/fitQCD->Eval(xx));
      frank_pp2->SetPointError(i,exx,eyy/fitQCD->Eval(xx));
    }
    andre_dau->GetPoint(i,xx,yy);
    eyy=andre_dau->GetErrorY(i);
    andre_dau2->SetPoint(i,xx,yy/(7.5*fitQCD->Eval(xx)));
    andre_dau2->SetPointError(i,exx,eyy/(7.5*fitQCD->Eval(xx)));
  }
  frank_pp2->SetMarkerStyle(25);
  frank_pp2->SetMarkerColor(4);



  TMultiGraph *m_rda=new TMultiGraph();  
  m_rda->Add(frank_pp2);
  m_rda->Add(andre_dau2);
  m_rda->SetMinimum(0.01);
  m_rda->SetMaximum(3.);
  m_rda->Draw("ap");
  
  c2->SaveAs("rda_prel2.pdf");
}
