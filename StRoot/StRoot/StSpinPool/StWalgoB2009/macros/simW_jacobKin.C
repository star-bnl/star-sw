int yellCol=kYellow;
TH2F *h2D=new TH2F("kin2D","W #rightarrow e + #nu, 3D isotropic in W CMS; electron P_{Z} (GeV/c); electron P_{T} (GeV/c)",100,-60,60,50,0,60);
TH1F *hmcPt,*hmcP;

TRandom3 *rnd=new TRandom3();

void simW_jacobKin() {
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1000000);
  
  //.... Pythia W events , Endcap ignored
  TFile *fdnEPW = TFile::Open("noEndcap/mcSetD1_ppWprod.wana.hist.root"); 
  assert(fdnEPW->IsOpen());
  hPW=(TH1F*)fdnEPW->Get("pubJoe1"); assert(hPW);
  hPW->SetTitle("accepted Pythia Ws");
  int nEve=1000;
  float sig=0.; // GeV

  ax=hPW->GetXaxis();
  ax->SetTitleSize(0.05); ax->SetTitleOffset(0.8);

  hmcPt=(TH1F *) hPW->Clone();
  hmcPt->Reset(); 
  hmcPt->SetNameTitle("mcPt","Isotropic W decay; electron P_{T} (GeV/c)");

  hmcP=(TH1F *) hPW->Clone();
  hmcP->Reset();
  hmcP->SetNameTitle("mcP","Isotropic W decay; electron P (GeV/c)");

  hPW->SetLineColor(kRed);
  hPW->Sumw2();

  //....... generate events
  for(int i=0;i<nEve;i++)    throwDecay(sig);

  float fac=hmcPt->Integral()/hPW->Integral();
  hPW->Scale(fac);

  float mxY=hPW->GetMaximum();
  if(mxY<mcPt->GetMaximum())mxY=mcPt->GetMaximum(); 
  hmcPt->SetMaximum(1.1*mxY);

  // plot raw spectra ..........  
  c=new TCanvas("aa2","aa2",1000,400);
  TPad *cL,*cR;   splitPadX(0.5,&cL,&cR);
  cL->cd();  h2D->Draw("colz"); 
  ln=new TLine(-55,15,55,15); ln->SetLineColor(kBlue); ln->Draw();
  tx=new TText(-28,10,"ET>15 GeV cut used in reco"); tx->Draw(); tx->SetTextColor(kBlue); tx->SetTextSize(0.04);
  char txt[1000]; sprintf(txt,"smear 1D #sigma=%.0f GeV",sig);
  tx=new TLatex(-50,55,txt); tx->Draw(); 

  cR->cd();
  cR->Divide(2,1); //c->cd(3);  hPW->Draw();  

  cR->cd(1);  hmcPt->Draw(); hmcPt->SetAxisRange(10,60); hPW->Draw("same"); 
  cR->cd(2);  hmcP->Draw(); hmcP->SetAxisRange(10,60); 
  
}

void throwDecay(float sig=3.) {// spread W momentum in GeV
  float Pmag=40; // energy avaliable to electron in GeV
  float phi=2*3.1416*rnd->Uniform();
  float cosTh=2*rnd->Uniform()-1.;
  float theta=acos(cosTh);
  TVector3 P(1,2,3); P.SetMag(Pmag); P.SetTheta(theta); P.SetPhi(phi); // mean
  TVector3 sigP(rnd->Gaus(0,sig),rnd->Gaus(0,sig),rnd->Gaus(0,sig));// spread
  P=P+sigP;
  h2D->Fill(P.z(), P.Pt());
  if( fabs(P.Pt())<15) continue; // minPt cut used in analysis
  hmcPt->Fill(P.Pt());  
  hmcP->Fill(P.Mag());
}

  //printf("%f %f %f \n", P.x(),P.y(), P.z());
  // printf("%f %f %f \n", sigP.x(),sigP.y(), sigP.z());


//------------------------
void splitPadX(float x, TPad **cL, TPad **cR) {
  (*cL) = new TPad("padL", "apdL",0.0,0.,x,0.95);
  (*cL)->Draw();
  (*cR) = new TPad("padL", "apdL",x+0.005,0.,1.0,0.95);
  (*cR)->Draw();
}

