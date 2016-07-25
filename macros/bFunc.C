//#define PRINT 1
TCanvas *canvas = 0;
Double_t X, Y, Z;
TFile *newf = 0;
const Int_t NHYPS = 5;
const Char_t *Names[] = {"p",
			 "K",
			 "pi",
			 "e",
			 "d"};
Bichsel *gBichsel = Bichsel::Instance("pai");
//____________________________________
Double_t bdEdx(Double_t *xx, Double_t *par) {
  Double_t zz = TMath::Log(xx[0]);
  Double_t x = par[0]; // log10bg
  Double_t y = par[1]; // log2dx
  Double_t zprob = gBichsel->GetMostProbableZ(x,y)-5.07402529167365057e-01;
  Double_t sigma = gBichsel->GetRmsZ(x,y);
  Double_t z = (zz - zprob)/sigma;
  return gBichsel->GetProbability(x,y,z)/xx[0]/sigma;
}
//____________________________________
Double_t bFunc(Double_t *xx, Double_t *par) {
  Double_t z = xx[0];
  Double_t x = par[0];
  Double_t y = par[1];
  return gBichsel->GetProbability(x,y,z);
}
//____________________________________
Double_t bFuncPA(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t y = par[0];
  return TMath::Exp(gBichsel->GetMostProbableZ(x,y));
}
//____________________________________
Double_t bFuncP(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t y = par[0];
  return gBichsel->GetMostProbableZ(x,y);
}
//____________________________________
Double_t bFuncA(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t y = par[0];
  return TMath::Exp(gBichsel->GetAverageZ(x,y));
}
//____________________________________
Double_t bFunc70(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t y = par[0];
  return gBichsel->GetI70(x,y);
}
//____________________________________
Double_t bFunc60(Double_t *xx, Double_t *par) {
  Double_t x = xx[0];
  Double_t y = par[0];
  return gBichsel->GetI60(x,y);
}
//____________________________________
void DrawProb(Int_t iX = 21, Int_t iY = 5) {
  TH3D    *bichPhi = (TH3D *) gBichsel->Phi();
  bichPhi->ProjectionZ(Form("bin_%i_%i",iX,iY),iX,iX,iY,iY)->Draw();
  TF1 *func = new TF1("func",bFunc,
		      bichPhi->GetZaxis()->GetXmin(),
		      bichPhi->GetZaxis()->GetXmax(),2);
  Double_t pars[2];
  pars[0] = bichPhi->GetXaxis()->GetBinCenter(iX);
  pars[1] = bichPhi->GetYaxis()->GetBinCenter(iY);
  func->SetParameters(pars);
  func->Draw("same");
}
//____________________________________
void DrawMostProbZ(Int_t iY = 5) {
  gBichsel->GetMostProbableZ(0,0);
  TProfile2D  *bichP = (TProfile2D  *) gBichsel->P();
  bichP->ProjectionX(Form("bin_%i_%i",iY),iY,iY)->Draw();
  TF1 *func = new TF1("func",bFuncP,bichP->GetXaxis()->GetXmin(),bichP->GetXaxis()->GetXmax(),1);
  Double_t pars[2];
  pars[0] = bichP->GetYaxis()->GetBinCenter(iY);
  func->SetParameters(pars);
  func->Draw("same");
}
//____________________________________
Double_t fnc(Double_t *xx, Double_t *par) {
  Z = xx[0];
  Double_t zMostProb = gBichsel->GetMostProbableZ(X,Y) + par[0];
  Double_t sigma     = gBichsel->GetRmsZ(X,Y) + par[1];
  Double_t z = (Z - zMostProb)/sigma;
  
  Double_t Value = gBichsel->GetProbability(X,Y,z)/sigma;
#ifdef PRINT
  cout << "X/Y/Z =\t" << X << "/" << Y << "/" << Z 
       << "\tzMostProb =\t" << zMostProb
       << "\tzSigma = \t" << zSigma
       << "\tValue = \t" << Value << endl;
#endif
  return Value;
}
//____________________________________
Double_t fncMip(Double_t *xx, Double_t *par) {
  Z = xx[0];
  Double_t zMostProb = par[0];//gBichsel->GetMostProbableZ(X,Y) + par[0];
  Double_t sigma     = gBichsel->GetRmsZ(X,Y) + par[1];
  //  sigma *= sigma;
  //  sigma += par[1];
  //  sigma = TMath::Sqrt(sigma);
  //  Double_t sigma     = 1. +  par[1];
  Double_t z = (Z - zMostProb)/sigma;
  //  Double_t z  = par[0] + (1. + par[1])*zz;
  //  Double_t Value = gBichsel->GetProbability(X,Y,z)/sigma*(1+par[1]);
  Double_t Value = gBichsel->GetProbability(X,Y,z)/sigma;
#ifdef PRINT
  cout << "X/Y/Z =\t" << X << "/" << Y << "/" << Z 
       << "\tzMostProb =\t" << zMostProb
       << "\tzSigma = \t" << zSigma
       << "\tValue = \t" << Value << endl;
#endif
  return Value;
}
//____________________________________
Double_t fncX(Double_t *x, Double_t *par) {
  Double_t xx[3];
  xx[0] = x[0];
  xx[1] = X;
  xx[2] = Y;
  return gBichsel->GetFunction(xx,par);
}
//____________________________________
void bFit(const Int_t hyp=0,const Int_t iX = 8,const Int_t iY=8) {
  if (hyp < 0 || hyp >= NHYPS) {cout << "Hyp = " << hyp << " is out of range" << endl; return;}
  TString name3D("zbgx");
  name3D += Names[hyp];
  //  if (iX != 0 && iY != 0) 
  canvas = new TCanvas("BinFit","Fit parameters");
  TDirectory *dir = gDirectory; cout << "Directory: " << dir->GetName() << endl;
  TH3 *hist = (TH3 *) gDirectory->Get(name3D);
  if (!hist) return;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  struct Fit_t {
    Float_t x;
    Float_t y;
    Float_t p0;
    Float_t p1;
    Float_t p2;
    Float_t ep0;
    Float_t ep1;
    Float_t ep2;
    Float_t sum;
    Float_t chisq;
    Float_t prob;
    Float_t hyp;
  };
  Fit_t Fit;
  TString tFName("zbgx5");
  tFName += Names[hyp];
  tFName += gDirectory->GetName();
  if (! newf) newf = new TFile(tFName.Data(),"update");
  TNtuple *FitP = (TNtuple *) newf->Get("FitP");
  if (! FitP) FitP = new TNtuple("FitP","Fit results",
				 "x:y:p0:p1:p2:ep0:ep1:ep2:sum:chisq:prob:hyp");
  TH2D *p0 = (TH2D *)  newf->Get("p0");
  if (! p0) p0 = new TH2D("p0","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  TH2D *p1 = (TH2D *)  newf->Get("p1");
  if (! p1) p1 = new TH2D("p1","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
#if 0
  TH2D *p2 = (TH2D *)  newf->Get("p2");
  if (! p2) p2 = new TH2D("p2","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
#endif
  TH2D *chisq = (TH2D *)  newf->Get("chisq");
  if (! chisq) chisq = new TH2D("chisq","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  TF1 *func = new TF1("func",fnc,hist->GetZaxis()->GetXmin(),hist->GetZaxis()->GetXmax(),2);
  //  func->SetParLimits(1,0.,1.);
  Double_t params[4];
  Int_t i1 = 1, i2 = nx;  if (iX > 0) {i1 = iX; i2 = iX;}
  Int_t j1 = 1, j2 = ny;  if (iY > 0) {j1 = iY; j2 = iY;}
  for (Int_t i=i1; i<=i2; i++) {
    for (Int_t j=j1; j<=j2; j++) {
      newf->cd();
      TString projName(Form("%s_%i_%i",Names[hyp],i,j));
      TH1 *proj = (TH1 *) newf->Get(projName.Data());
      if (! proj) proj = hist->ProjectionZ(projName.Data(),i,i,j,j);
      if (! proj) continue;
      X = hist->GetXaxis()->GetBinCenter(i);
      Y = hist->GetYaxis()->GetBinCenter(j);
      Double_t sum = proj->Integral();
      memset (&Fit.x, 0, sizeof(Fit));
      Fit.sum = sum;
      Fit.x = X;
      Fit.y = Y;
      if (sum < 1.e2) {delete proj; continue;}
      cout << "Projection:\t" 
	   << proj->GetName() 
	   << "\ti/j\t" << i << "/" << j 
	   << "\tX/Y\t" << X << "/" << Y 
	   << "\tbg/dx\t" << pow(10.,X) << "/" << pow(2.,Y)
	   << "\tIntegral = \t" << sum << endl;
      proj->SetTitle(Form("bg = %6.2f dx = %6.2f", pow(10.,X), pow(2.,Y)));
      Double_t bw = proj->GetBinWidth(1);
      Int_t nb = proj->GetNbinsX();
      Int_t l1 = 999, l2 = 0;
      for (int l=1; l<=nb; l++) {
	Double_t val = proj->GetBinContent(l);
	if (val <= 0.0) continue;
	if (l < l1 ) l1 = l;
	l2 = l;
	val = val/sum;
	Double_t err = TMath::Sqrt(val*(1.-val)/2./sum);
	proj->SetBinContent(l,val/bw);
	proj->SetBinError(l,err/bw);
      }
      if (l1 < l2) proj->GetXaxis()->SetRange(l1,l2);
      //      sum *= proj->GetBinWidth(1);
      //      proj->Scale(1./sum);
      Int_t lx = proj->GetMaximumBin();
      params[0] =  proj->GetBinCenter(lx);// - gBichsel->GetMostProbableZ(X,Y);
      params[1] =  0;
      func->SetParameters(params);
      func->FixParameter(0,params[0]);
      func->FixParameter(1,params[1]);
#if 1
      proj->Fit("func");
      Fit.p0 = func->GetParameter(0);
      Fit.ep0 = func->GetParError(0);
      p0->SetBinContent(i,j,func->GetParameter(0));
      p0->SetBinError(i,j,func->GetParError(0));
      Fit.p1 = func->GetParameter(1);
      Fit.ep1 = func->GetParError(1);
      p1->SetBinContent(i,j,func->GetParameter(1));
      p1->SetBinError(i,j,func->GetParError(1));
      Fit.chisq = func->GetChisquare();
      chisq->SetBinContent(i,j,func->GetChisquare());
x[      Fit.prob = func->GetProb();
      Fit.hyp = hyp;
      FitP->Fill(&Fit.x);
      proj->Draw();
      if (canvas) canvas->Update();
#else 
      Double_t zz = 0.1303 + gBichsel->GetMostProbableZ(X,Y);
      func->Eval(zz);
#endif
    }
  }  
  newf->cd();
  if (iX == 0 && iY == 0) {newf->Write(); delete newf;}
}
//____________________________________
void bFitSR(const Int_t iX = 8,const Int_t iY=8, Int_t bin=4) {
  const Int_t Nlog2dx = 15;
  const Double_t log2dxLow = 0.15, log2dxHigh = 3.15;
  Int_t k1 = 1, k2 = Nlog2dx; if (bin > 0) {k1 = bin; k2 = bin;}
  canvas = new TCanvas("BinFit","Fit parameters");
  TDirectory *dir = gDirectory; cout << "Directory: " << dir->GetName() << endl;
  struct Fit_t {
    Float_t x;
    Float_t y;
    Float_t z;
    Float_t p0;
    Float_t p1;
    Float_t p2;
    Float_t ep0;
    Float_t ep1;
    Float_t ep2;
    Float_t sum;
    Float_t chisq;
    Float_t prob;
  };
  Fit_t Fit;
  TString tFName(Form("SecRow_%i_",bin));
  tFName += gDirectory->GetName();
  if (! newf) newf = new TFile(tFName.Data(),"update");
  TNtuple *FitP = (TNtuple *) newf->Get("FitP");
  if (! FitP) FitP = new TNtuple("FitP","Fit results",
				 "row:sector:dx:p0:p1:p2:ep0:ep1:ep2:sum:chisq:prob");
  TF1 *func = 0;
  //  func->SetParLimits(1,0.,1.);
  Double_t params[4];
  Int_t i1 = iX, i2 = iX;
  Int_t j1 = iY, j2 = iY;
  for (Int_t k=k1; k<=k2; k++) {
    TString name3D(Form("SecRow3Mip_%i",k-1));
    Y = (bin-0.5)*(log2dxHigh-log2dxLow)/Nlog2dx + log2dxLow;
    TH3 *hist = (TH3 *) dir->Get(name3D);
    if (!hist) continue;
    if (! func) func = new TF1("func",fnc,hist->GetZaxis()->GetXmin(),hist->GetZaxis()->GetXmax(),2);
    Int_t nx = hist->GetNbinsX();
    Int_t ny = hist->GetNbinsY();
    if (iX <= 0) {i1 = 1; i2 = nx;}
    if (iY <= 0) {j1 = 1, j2 = ny;}
    for (Int_t i=i1; i<=i2; i++) {
      for (Int_t j=j1; j<=j2; j++) {
	newf->cd();
	TString projName(Form("%s_%i_%i",name3D.Data(),i,j));
	TH1 *proj = (TH1 *) newf->Get(projName.Data());
	if (! proj) proj = hist->ProjectionZ(projName.Data(),i,i,j,j);
	if (! proj) continue;
	X = TMath::Log10(1.5/0.13956995);//hist->GetXaxis()->GetBinCenter(i);
	//      Y = bin; //hist->GetYaxis()->GetBinCenter(j);
	Double_t sum = proj->Integral();
	memset (&Fit.x, 0, sizeof(Fit));
	Fit.sum = sum;
	Fit.x = i;
	Fit.y = j;
	Fit.z = Y;
	if (sum < 1.e2) {delete proj; continue;}
	cout << "Projection:\t" 
	     << proj->GetName() 
	     << "\ti/j\t" << i << "/" << j 
	     << "\tX/Y\t" << X << "/" << Y 
	     << "\tbg/dx\t" << pow(10.,X) << "/" << pow(2.,Y)
	     << "\tIntegral = \t" << sum << endl;
	proj->SetTitle(Form("bg = %6.2f dx = %6.2f", pow(10.,X), pow(2.,Y)));
	Double_t bw = proj->GetBinWidth(1);
	Int_t nb = proj->GetNbinsX();
	Int_t l1 = 999, l2 = 0;
	for (int l=1; l<=nb; l++) {
	  Double_t val = proj->GetBinContent(l);
	  if (val <= 0.0) continue;
	  if (l < l1 ) l1 = l;
	  l2 = l;
	  val = val/sum;
	  Double_t err = TMath::Sqrt(val*(1.-val)/2./sum);
	  proj->SetBinContent(l,val/bw);
	  proj->SetBinError(l,err/bw);
	}
	if (l1 < l2) proj->GetXaxis()->SetRange(l1,l2);
	//      sum *= proj->GetBinWidth(1);
	//      proj->Scale(1./sum);
	Int_t lx = proj->GetMaximumBin();
	params[0] =  proj->GetBinCenter(lx) - gBichsel->GetMostProbableZ(X,Y);
	params[1] =  0;
	func->SetParameters(params);
	//     func->FixParameter(0,params[0]);
	//      func->FixParameter(1,params[1]);
	proj->Fit("func");
	Fit.p0 = func->GetParameter(0);
	Fit.ep0 = func->GetParError(0);
	Fit.p1 = func->GetParameter(1);
	Fit.ep1 = func->GetParError(1);
	Fit.chisq = func->GetChisquare();
	Fit.prob = func->GetProb();
	FitP->Fill(&Fit.x);
	proj->Draw();
	if (canvas) canvas->Update();
      }
    }  
  }
  newf->cd();
  if (iX == 0 && iY == 0) {newf->Write(); delete newf;}
}
//________________________________________________________________________________
void bFunc() {}
//____________________________________
void bDraw(const Int_t hyp=0,const Int_t iX = 8,const Int_t iY=8) {
  if (hyp < 0 || hyp >= NHYPS) {cout << "Hyp = " << hyp << " is out of range" << endl; return;}
  TString name3D("zbgx");
  name3D += Names[hyp];
  canvas = new TCanvas("BinDraw","Compare with Bichsel function");
  TDirectory *dir = gDirectory; cout << "Directory: " << dir->GetName() << endl;
  TH3 *hist = (TH3 *) gDirectory->Get(name3D);
  if (!hist) return;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  TString tFName("zbgxDraw");
  tFName += Names[hyp];
  tFName += gDirectory->GetName();
  if (! newf) newf = new TFile(tFName.Data(),"update");
  Double_t params[3];
  Int_t i1 = 1, i2 = nx;  if (iX > 0) {i1 = iX; i2 = iX;}
  Int_t j1 = 1, j2 = ny;  if (iY > 0) {j1 = iY; j2 = iY;}
  for (Int_t i=i1; i<=i2; i++) {
    for (Int_t j=j1; j<=j2; j++) {
      newf->cd();
      TString projName(Form("%s_%i_%i",Names[hyp],i,j));
      TH1 *proj = (TH1 *) newf->Get(projName.Data());
      if (! proj) proj = hist->ProjectionZ(projName.Data(),i,i,j,j);
      if (! proj) continue;
      X = hist->GetXaxis()->GetBinCenter(i);
      Y = hist->GetYaxis()->GetBinCenter(j);
      Double_t y = Y;
      if (hyp==3) y = Y+log10(0.51099907e-3/0.13956995);
      if (hyp==4) y = Y+log10(1.87561339/0.13956995);
      if (hyp==0) y = Y+log10(0.93827231/0.13956995);
      if (hyp==1) y = Y+log10(0.493677/0.13956995);
      cout << "hyp = " << hyp << " y = " << y << endl;
      if (  hyp==3 && y <-0.2 ||
	    hyp==4 && y < 0.8 ||
	    hyp==0 && y < 0.7 ||
	    hyp==1 && y < 0.4
	  ) continue;
      Double_t sum = proj->Integral();
      if (sum < 1.e2) {delete proj; continue;}
      cout << "Projection:\t" 
	   << proj->GetName() 
	   << "\ti/j\t" << i << "/" << j 
	   << "\tX/Y\t" << X << "/" << Y 
	   << "\tbg/dx\t" << pow(10.,X) << "/" << pow(2.,Y)
	   << "\tIntegral = \t" << sum << endl;
      proj->SetTitle(Form("bg = %6.2f dx = %6.2f", pow(10.,X), pow(2.,Y)));
      Double_t bw = proj->GetBinWidth(1);
      Int_t nb = proj->GetNbinsX();
      Int_t l1 = 999, l2 = 0;
      for (int l=1; l<=nb; l++) {
	Double_t val = proj->GetBinContent(l);
	if (val <= 0.0) continue;
	if (l < l1 ) l1 = l;
	l2 = l;
	val = val/sum;
	Double_t err = TMath::Sqrt(val*(1.-val)/2./sum);
	proj->SetBinContent(l,val/bw);
	proj->SetBinError(l,err/bw);
      }
      if (l1 < l2) proj->GetXaxis()->SetRange(l1,l2);
      TF1 *func = new TF1("func",fnc1,hist->GetZaxis()->GetXmin(),hist->GetZaxis()->GetXmax(),0);
      proj->GetListOfFunctions()->Add(func);
      proj->SetStats(0);
      proj->Draw();
      if (canvas) canvas->Update();
    }
  }  
  newf->cd();
  if (iX == 0 && iY == 0) {newf->Write(); delete newf;}
}
//____________________________________
void bFitMip(const Int_t iX = 8,const Int_t iY=8) {
  TString name3D("SecRow3Mip");
  canvas = new TCanvas("BinFit","Fit parameters");
  TDirectory *dir = gDirectory; cout << "Directory: " << dir->GetName() << endl;
  TH3 *hist = (TH3 *) gDirectory->Get(name3D);
  if (!hist) return;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  Int_t hyp = 0;
  struct Fit_t {
    Float_t i;
    Float_t j;
    Float_t x;
    Float_t y;
    Float_t mean;
    Float_t rms;
    Float_t peak
    Float_t p0;
    Float_t p1;
    Float_t p2;
    Float_t ep0;
    Float_t ep1;
    Float_t ep2;
    Float_t sum;
    Float_t chisq;
    Float_t prob;
    Float_t hyp;
  };
  Fit_t Fit;
  TString tFName("SecRowMipFit");
  tFName += Names[hyp];
  tFName += gSystem->BaseName(gDirectory->GetName());
  if (! newf) newf = new TFile(tFName.Data(),"update");
  TNtuple *FitP = (TNtuple *) newf->Get("FitP");
  if (! FitP) FitP = new TNtuple("FitP","Fit results",
				 "i:j:x:y:mean:rms:peak:mu:sigma:p2:emu:esigma:ep2:sum:chisq:prob:hyp");
  TH2D *p0 = (TH2D *)  newf->Get("p0");
  if (! p0) p0 = new TH2D("p0","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  TH2D *p1 = (TH2D *)  newf->Get("p1");
  if (! p1) p1 = new TH2D("p1","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
#if 0
  TH2D *p2 = (TH2D *)  newf->Get("p2");
  if (! p2) p2 = new TH2D("p2","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
#endif
  TH2D *chisq = (TH2D *)  newf->Get("chisq");
  if (! chisq) chisq = new TH2D("chisq","shift of most probable value",
			  nx,hist->GetXaxis()->GetXmin(),hist->GetXaxis()->GetXmax(),
			  ny,hist->GetYaxis()->GetXmin(),hist->GetYaxis()->GetXmax());
  TF1 *func = new TF1("func",fncMip,hist->GetZaxis()->GetXmin(),hist->GetZaxis()->GetXmax(),2);
  //  func->SetParLimits(1,0.,0.);
  func->FixParameter(1,0);
  Double_t params[4];
  Int_t i1 = 1, i2 = nx;  if (iX > 0) {i1 = iX; i2 = iX;}
  Int_t j1 = 1, j2 = ny;  if (iY > 0) {j1 = iY; j2 = iY;}
  for (Int_t i=i1; i<=i2; i++) {
    for (Int_t j=j1; j<=j2; j++) {
      newf->cd();
      TString projName(Form("%s_%i_%i",Names[hyp],i,j));
      TH1 *proj = (TH1 *) newf->Get(projName.Data());
      if (! proj) proj = hist->ProjectionZ(projName.Data(),i,i,j,j);
      if (! proj) continue;
      double xx = hist->GetXaxis()->GetBinCenter(i);
      double yy = hist->GetYaxis()->GetBinCenter(j);
      Y = yy;
#if 0  
      double dy = hist->GetYaxis()->GetBinWidth(j);
      double b = 7.5;
      if (xx >=14) b = 4;
      Double_t xm = yy + 1./b - 0.5*dy/TMath::TanH(0.5*dy*b);
      Y = xm;
#endif
      Double_t sum = proj->Integral();
      memset (&Fit.x, 0, sizeof(Fit));
      Fit.sum = sum;
      Fit.i = i;
      Fit.j = j;
      Fit.mean = proj->GetMean();
      Fit.rms  = proj->GetRMS();
      Fit.x = xx;
      Fit.y = Y;
      Int_t Row = xx;
      //      X = TMath::Log10(1.5/0.13956995);
      X = TMath::Log10(0.448/0.14);
      if (sum < 1.e2) {delete proj; continue;}
      cout << "Projection:\t" 
	   << proj->GetName() 
	   << "\ti/j\t" << i << "/" << j 
	   << "\tX/Y\t" << Row << "/" << Y 
	   << "\tRow/dx\t" << Row << "/" << pow(2.,Y) << "/" << pow(2.,yy) 
	   << "\tIntegral = \t" << sum << endl;
      proj->SetTitle(Form("Row = %i dx = %6.2f", Row, pow(2.,Y)));
      Double_t bw = proj->GetBinWidth(1);
      Int_t nb = proj->GetNbinsX();
      Int_t l1 = 999, l2 = 0;
      for (int l=1; l<=nb; l++) {
	Double_t val = proj->GetBinContent(l);
	val = val/sum;
	Double_t err = TMath::Sqrt(val*(1.-val)/2./sum);
	proj->SetBinContent(l,val/bw);
	proj->SetBinError(l,err/bw);
	if (val <= 0.0) continue;
	if (l < l1 ) l1 = l;
	l2 = l;
      }
      if (l1 < l2) proj->GetXaxis()->SetRange(l1,l2);
      //      sum *= proj->GetBinWidth(1);
      //      proj->Scale(1./sum);
      Int_t lx = proj->GetMaximumBin();
      Fit.peak = proj->GetBinCenter(lx);
      params[0] =  0; // - gBichsel->GetMostProbableZ(X,Y);
      params[1] =  0;
      func->SetParameters(params);
//       func->FixParameter(0,params[0]);
//       func->FixParameter(1,params[1]);
#if 1
      proj->Fit("func","lm");
      Fit.p0 = func->GetParameter(0);
      Fit.ep0 = func->GetParError(0);
      p0->SetBinContent(i,j,func->GetParameter(0));
      p0->SetBinError(i,j,func->GetParError(0));
      Fit.p1 = func->GetParameter(1);
      Fit.ep1 = func->GetParError(1);
      p1->SetBinContent(i,j,func->GetParameter(1));
      p1->SetBinError(i,j,func->GetParError(1));
      Fit.chisq = func->GetChisquare();
      chisq->SetBinContent(i,j,func->GetChisquare());
      Fit.prob = func->GetProb();
      Fit.hyp = hyp;
      FitP->Fill(&Fit.i);
      proj->Draw();
      if (canvas) canvas->Update();
#else 
      Double_t zz = 0.1303 + gBichsel->GetMostProbableZ(X,Y);
      func->Eval(zz);
#endif
    }
  }  
  newf->cd();
  if (iX == 0 && iY == 0) {newf->Write(); delete newf;}
}
