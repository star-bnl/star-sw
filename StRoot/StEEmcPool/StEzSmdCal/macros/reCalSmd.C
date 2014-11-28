//recalibrate SMD strips
reCalSmd() {
  float goalMipEne=1.3; // MeV

  // const int nSec=8;  char *secL[nSec]={"01V","02U","04V","05U","07V","08U","10V","11U"};    TString tit="smd-Layer-1";  char *fncN="pol4";
  // const int nSec=8;  char *secL[nSec]={"02V","03U","05V","06U","08V","09U","11V","12U"};    TString tit="smd-Layer-2";  char *fncN="pol4";
  const int nSec=8;  char *secL[nSec]={"03V","04U","06V","07U","09V","10U","12V","01U"};    TString tit="smd-Layer-3";  char *fncN="pol4";

  //  const int nSec=2;  char *secL[nSec]={"01U","01V"}; TString tit="sectXX";  char *fncN="pol3"; //test case
   
   TFile *fdA[nSec];
   TString iPath="/star/u/wissink/cal2006/"; //location of .hist.root files
 
  //.......... open files  
  int j;
  for(j=0;j<nSec;j++) {
    TString fname=iPath+"smd"+secL[j]+".hist.root";
    fdA[j]=new TFile(fname);
    assert(fdA[j]->IsOpen());
  }

  //  fdG=new TFile("R5112018.hist.root");
  fdG=new TFile("/star/u/wissink/cal2006/iter5-pp/R7089008.hist.root");
  assert(fdG->IsOpen());

  // ...... draw .........
  
  h=new TH1F(tit,tit+" Average recon MIP energy from pair of strips; strip ID; MIP energy [MeV]",290,.5,290.5);

  c=new TCanvas(tit,tit,600,400);
  
  h->Draw();
  h->SetMinimum(0.7);
  h->SetMaximum(1.8);
  gStyle->SetOptFit(0);
  gStyle->SetOptStat(0);

  lg=new TLegend(.2,.11,.35,.41);

  TGraphErrors *grS=new TGraphErrors;
  grS->SetMarkerStyle(3);

  for(j=0;j<nSec;j++) {
    //    if(j>1) break;
    TString grN="mpvN"; grN+=secL[j];
    TGraphErrors *gr=(TGraphErrors *)fdA[j]->Get(grN);
    assert(gr);
    int n=gr->GetN();
    Double_t* eyA=gr->GetEY();
    Double_t* yA=gr->GetY();
    Double_t* xA=gr->GetX();
    int i;
    for(i=0;i<n;i++){
      if(fabs(yA[i]-1.3)>0.5) continue;
      if(fabs(eyA[i])>0.5) continue;
      int nS=grS->GetN();      
      grS->SetPoint(nS,xA[i],yA[i]);
      grS->SetPointError(nS,0.,eyA[i]);
    }
    //    gr->Print();
    gr->Draw("P");
    gr->SetMarkerStyle(24+j);
    lg->AddEntry(gr,secL[j],"lpe");
  }

  lg->Draw();

  c2=new TCanvas();
  
  h->Draw();
  grS->Draw("P");
  //grS->Print();
  int col=kBlue;

  grS->Fit(fncN);
  TF1* f=grS->GetFunction(fncN); assert(f);
  f->SetLineColor(col);
  f->SetLineWidth(2);
  f->SetRange(0.5,288.);

  c->cd();
  f->Draw("same");

  //................calc new gains
  //  c->Print();
  c3=new TCanvas();
  c3->Divide(3,3);
  for(j=0;j<nSec;j++) {
    TString gnN="ug"; gnN+=secL[j];
    TH1F * hg=( TH1F *)fdG->Get(gnN);
    assert(hg);
    c3->cd(j+1); hg->Draw();
    int i;
    int ns=hg->GetNbinsX();

    TString ogN="gains"; ogN+=secL[j]; ogN+="smd.dat";
    FILE *fog=fopen(ogN.Data(),"w"); assert(fog);
    fprintf(fog,"# absolute gains for SMD plain %s,\n# stripName, gain[ch/GeV], erGain=dumm, correction, old gain\n",secL[j]);
    //  return;
    for(i=1;i<=ns;i++) {
      if(i>288) break;
      float g1=hg->GetBinContent(i);
      float gc=goalMipEne/f->Eval(i);
      float g2=g1/gc;
      
      //printf("str=%d %8.1f %.3f  -->%.1f  \n",i,g1,gc,g2);
      fprintf(fog,"%s%03d %8.1f 0.1 (corr= %.3f) old=%.1f \n",secL[j],i,g2,gc,g1);
    }
    fclose(fog);
  }
     

}






//................................................
//................................................
//................................................
// plot average MIP response of SMD plains on the same depth
avr2() {

  // const int nSec=3;  char *secL[nSec]={"05U","07V","08U"}; int symA[nSec]={24,28,29}; TString tit="SMD-L1";
  // const int nSec=3;  char *secL[nSec]={"06U","05V","08V"}; int symA[nSec]={26,24,29}; TString tit="SMD-L2";
     const int nSec=2;  char *secL[nSec]={"01U","01V"}; int symA[nSec]={26,28}; TString tit="SMD-L3";

  TFile *fdA[nSec];
  TString iPath="/star/u/wissink/cal2006/"; // location for output files

  //.......... open files  
  int j;
  for(j=0;j<nSec;j++) {
    TString fname=iPath+"smd"+secL[j]+".hist.root";
    fdA[j]=new TFile(fname);
    assert(fdA[j]->IsOpen());
  }

  // ...... draw .........

  h=new TH1F(tit,tit+" Average recon MIP energy from pair of strips; strip ID; MIP energy [MeV]",290,.5,290.5);

  c=new TCanvas(tit,tit,600,400);
  //  c->Divide(2,2); c->cd(1);

  h->Draw();
  h->SetMinimum(0.6);
  h->SetMaximum(1.6);
   gStyle->SetOptFit(0);
  gStyle->SetOptStat(0);

  lg=new TLegend(.2,.15,.4,.45);

  for(j=0;j<nSec;j++) {
    int col=kBlack+j+1;
    TString grN="mpvN"; grN+=secL[j];
    TGraphErrors *gr=(TGraphErrors *)fdA[j]->Get(grN);
    assert(gr);
    gr->Draw("P");
    gr->SetMarkerStyle(symA[j]);
    gr->SetLineColor(col);
    gr->SetMarkerColor(col);

   // change pol4 -> pol2 3/30/2007 - sww
    char *fncN="pol4";
    gr->Fit(fncN);
    TF1* f=gr->GetFunction(fncN); assert(f);
    f->SetLineColor(col);
    f->SetLineWidth(2);

    lg->AddEntry(gr,secL[j],"lpe");
  }

  lg->Draw();

  //  c->Print();


}
