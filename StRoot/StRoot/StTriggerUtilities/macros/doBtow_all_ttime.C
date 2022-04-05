{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.
gROOT->Reset();
#include "Riostream.h"
 
ifstream in0;


Float_t x[50]={0.,5.,10.,15.,20.,25.,30.,35.,40.,45.,50.};
Float_t y[31][11],ey[31][11];
Float_t r;

East = new TH1D("East","East",50,30,50);
West = new TH1D("West","West",50,30,50);
 
in0.open("timing.tex");

for(Int_t is=0;is<11;is++) {
  x[is] += 12.;
for(Int_t ic=0;ic<30;ic++) {
  in0>>r>>r>>y[ic][is];
  ey[ic][is]=sqrt(y[ic][is]);
}
}

/*
 for(Int_t is=1;is<11;is++) {
   for(Int_t ic=0;ic<30;ic++) {
     y[ic][is]-=y[ic][0];
     ey[ic][is]=0.;
   }
 }

for(Int_t ic=0;ic<30;ic++) {
  y[ic][0]=0.;
}


  for(Int_t ic=0;ic<30;ic++) {
    r = y[ic][8];
    for(Int_t is=1;is<11;is++) {
    y[ic][is]/=r;
  }
}
*/


Float_t yf[5],ef[5],xf[5];

gStyle->SetOptStat(0);

c1 = new TCanvas("c1","Timing of BEMC Crate", 200, 10, 700, 500);
c1->Divide(4,4);
for(int j=0;j<15;j++){
  c1->cd(j+1);
  Float_t yy[11],eyy[11];
  c1->SetGridy(1);
  c1->SetGridx(1)  ;

  for(Int_t i=0;i<11;i++){
    yy[i]=y[j][i];
    eyy[i]=ey[j][i];
  }

  for(Int_t i=0;i<5;i++){
    yf[i]=yy[i+3];
    ef[i]=eyy[i+3];
    xf[i]=x[i+3];
  }

  graph1 = new TGraphErrors(11, x, yy,0,eyy);
  graph1->SetMarkerStyle(20);
  graph1->SetMarkerSize(1.0);
  graph1->SetMarkerColor(1);
  graph1->Draw("AP");

  graph2 = new TGraphErrors(5, xf, yf,0,ef);
  graph2->SetMarkerStyle(20);
  graph2->SetMarkerSize(1.0);
  graph2->SetMarkerColor(2);


  graph2->Fit("gaus");
  graph2->GetXaxis()->SetTitle("TCD Phase (ns)");
  graph2->GetYaxis()->SetTitle("Integral 100<ADC<2000");
  graph2->Draw("SP");
  East->Fill(gaus->GetParameter(1));

  TLine *tl = new TLine(36.,0.,36.,1.e+07);
  tl->SetLineColor(2);
  tl->SetLineWidth(3);
  tl->Draw();
  Float_t gl=gaus->GetParameter(1);
  TLine *t2 = new TLine(gl,0.,gl,1.e+07);
  t2->SetLineColor(3);
  t2->SetLineWidth(3);
  t2->Draw();


}
c1->cd(16);
gStyle->SetOptStat(1);
gStyle->SetOptStat(1);
East->Draw();


c2 = new TCanvas("c2","Timing of BEMC Crate", 200, 10, 700, 500);
gStyle->SetOptStat(0);

c2->Divide(4,4);
for(int j=15;j<30;j++){
  c2->cd(j-14);
  Float_t yy[11],eyy[11];
  c2->SetGridy(1);
  c2->SetGridx(1)  ;
                                                                                  
  for(Int_t i=0;i<11;i++){
    yy[i]=y[j][i];
    eyy[i]=ey[j][i];
  }

  for(Int_t i=0;i<5;i++){
    yf[i]=yy[i+3];
    ef[i]=eyy[i+3];
    xf[i]=x[i+3];
      }


  graph1 = new TGraphErrors(11, x, yy,0,eyy);
  graph1->SetMarkerStyle(20);
  graph1->SetMarkerSize(1.0);
  graph1->SetMarkerColor(1);
  graph1->Draw("AP");

  graph2 = new TGraphErrors(5, xf, yf,0,ef);
  graph2->SetMarkerStyle(20);
  graph2->SetMarkerSize(1.0);
  graph2->SetMarkerColor(2);

  graph2->Fit("gaus");
  graph2->GetXaxis()->SetTitle("TCD Phase (ns)");
  graph2->GetYaxis()->SetTitle("Integral 100<ADC<2000");
  graph2->Draw("SP");
  West->Fill(gaus->GetParameter(1));
                                                                                  
  TLine *tl = new TLine(36.,0.,36.,1.e+07);
  tl->SetLineColor(2);
  tl->SetLineWidth(3);
  tl->Draw();
  Float_t gl=gaus->GetParameter(1);
  TLine *t2 = new TLine(gl,0.,gl,1.e+07);
  t2->SetLineColor(3);
  t2->SetLineWidth(3);
  t2->Draw();

}
c2->cd(16);
gStyle->SetOptStat(1);
West->Draw();
}
