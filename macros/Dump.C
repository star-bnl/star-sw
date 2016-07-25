TH3D *bichPhi = (TH3D *) gDirectory->Get("bichPhi");
TH3D *bichPhiE= (TH3D *) gDirectory->Get("bichPhiE");
Int_t nBinbg = 101; Double_t bgrange[2] = { -1., 4.};  //  Double_t bglow = -1.025, bgup = 4.025;
Double_t Dbg = (bgrange[1]-bgrange[0])/(nBinbg-1), bglow = bgrange[0] - Dbg/2., bgup  = bgrange[1] + Dbg/2.;
Int_t nBinxL = 11; Double_t xLrange[2] = {-0.3,3.};//  Double_t xLlow = -.45, xLup = 3.15;
Double_t DxL = (xLrange[1]-xLrange[0])/(nBinxL-1), xLlow = xLrange[0] - DxL/2., xLup  = xLrange[1] + DxL/2.;
Int_t nBinz  = 51;  Double_t zrange[2] = { -4., 6.};  //Double_t zlow = -4.05, zup = 6.05;
Double_t Dz  = (zrange[1]-zrange[0])/(nBinz-1), zlow = zrange[0] - Dz/2., zup  = zrange[1] + Dz/2.;
Double_t binWx = 0, binWy = 0, binWz = 0;
Int_t nbinx = 0, nbiny = 0, nbinz = 0;
TAxis *ax = 0, *ay = 0, *az = 0;
TFile *fCurrent = gDirectory;
//________________________________________________________________________________
void Dump(Char_t *name="BichselTN.root"){
  TNtuple  *dEdxS = fCurrent->Get("dEdxS");
  TNtuple  *dEdxP = fCurrent->Get("dEdxP");
  TFile *f = new TFile(name,"recreate");
  //  TFile *f = new TFile("P10T.root","recreate");
  TProfile2D *bichP = 
    new TProfile2D("bichP","zm: The most probable value of log(dE/dx) versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("zm:log(x)/log(2):log10(bg)>>bichP","","colz");
  TProfile2D *bichA = 
    new TProfile2D("bichA","mean_z: The average value of z = log(dE/dx) versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("mean_z:log(x)/log(2):log10(bg)>>bichA","","colz");
  TProfile2D *bichI70 = 
    new TProfile2D("bichI70","I70: The average value after 30% truncation versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("1.e-3*I70/x:log(x)/log(2):log10(bg)>>bichI70","","colz");
  TProfile2D *bichI60 = 
    new TProfile2D("bichI60","I60: The average value after 40% truncation versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("1.e-3*I60/x:log(x)/log(2):log10(bg)>>bichI60","","colz");
  TProfile2D *bichD = 
    new TProfile2D("bichD","Delta_P : The most probable dE/dx versus log10(beta*gamma) and log2(dx)",
		   nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("Delta_p:log(x)/log(2):log10(bg)>>bichD","","colz"); 
  TProfile2D *bichRms = 
    new TProfile2D("bichRms","sigma_z : The RMS value of z = log(dE/dx) versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("sigma_z:log(x)/log(2):log10(bg)>>bichRms","","colz");
  TProfile2D *bichW = 
    new TProfile2D("bichW","width : The width of z = log(dE/dx) distribution versus log10(beta*gamma) and log2(dx)",
		 nBinbg,bglow,bgup,nBinxL,xLlow,xLup);
  dEdxS->Draw("width:log(x)/log(2):log10(bg)>>bichW","","colz");
  bichPhi = 
    new TH3D("bichPhi","The Bichsel probability versus log10(beta*gamma) and log2(dx) and z",
	     nBinbg,bglow,bgup,nBinxL,xLlow,xLup,nBinz,zlow,zup);
  dEdxP->Draw("(z-zm)/sigma_z:log(x)/log(2):log10(bg) >> bichPhi","phi*sigma_z");
  bichPhiE = 
    new TH3D("bichPhiE","The Bichsel entires versus z log10(beta*gamma) and log2(dx) and ",
	     nBinbg,bglow,bgup,nBinxL,xLlow,xLup,nBinz,zlow,zup);
  dEdxP->Draw("(z-zm)/sigma_z:log(x)/log(2):log10(bg) >> bichPhiE");
  bichPhi->Divide(bichPhiE);
  delete bichPhiE;
#if 0
  TProfile   *prof = new TProfile("prof","Profile for give dx and bg",nBinz, zlow, zup);
  for (Int_t j=1; j<=nBinxL; j++) {
    Double_t xL  =      bichP->GetYaxis()->GetBinCenter(j);
    Double_t dxL = 0.5*(bichP->GetYaxis()->GetBinUpEdge(j) - bichP->GetYaxis()->GetBinLowEdge(j));
    for (Int_t i=1; i<=nBinbg; i++) {
      Double_t bg  =      bichP->GetXaxis()->GetBinCenter(i);
      Double_t dbg = 0.5*(bichP->GetXaxis()->GetBinUpEdge(i) - bichP->GetXaxis()->GetBinLowEdge(i));
      prof->Reset();
      TString Select("");
      Select += Form("log10(bg) > %f && log10(bg) <= %f",bg-dbg,bg+dbg);
      Select += Form(" && log(x)/log(2) > %f && log(x)/log(2) <= %f",xL-dxL,xL+dxL);
      cout << "Select: " << Select.Data() << endl;
      dEdxP->Draw("phi*sigma_z:(z-zm)/sigma_z>>prof",Select.Data());
      for (Int_t k = 1; k <= nBinz; k++) {
	if (prof->GetBinEntries(k)) bichPhi->SetBinContent(k,i,j,prof->GetBinContent(k));
	else                        bichPhi->SetBinContent(k,i,j,0.);
      }
    }
  }
#endif
  f->Write();
  //  delete f;
  //  fCurrent->cd();
}
