#include "dEdxHist.h"
Int_t Hists3D::NtotHist = 9;
Hists3D::Hists3D(const Char_t *Name, const Char_t *Title,
		 const Char_t *TitleX, const Char_t *TitleY,
		 Int_t nXBins, 
		 Int_t nYBins,  Double_t ymin, Double_t ymax,
		 Int_t nZBins, Double_t ZdEdxMin, Double_t ZdEdxMax,
		 Double_t xmin, Double_t xmax, Int_t nh) : fNHist(nh) {
  const Char_t *Names[9] = {"","C","N","Ne","Npi","NK","NP","Nd","dX"};
  const Char_t *Titles[9] = {"uncorrected", "correctred","nP measured","nP for e","nP for pi","nP for K","nP for P","nP for d","dX"};
  memset(hists, 0, 9*sizeof(TH1*));
  Int_t nx = nXBins;
  if (xmin >= xmax) {
    xmin = 0.5;
    xmax = TMath::Abs(nXBins)+0.5;
  }
  if (nx < 0) {
    xmin = - xmax;
    nx = 2*TMath::Abs(nXBins) + 1;
  }
  if (ymin >= ymax) {
    ymin = 0.5;
    ymax = nYBins+0.5;
  }
  for (Int_t j = 0; j < fNHist; j++) {
    TString name(Name); 
    name += Names[j];
    TString title(Title); 
    title += "(";  title += Titles[j]; title += ") versus "; title += TitleX; title += " and "; title += TitleY;
    if (j < 8) {
      Int_t    nz   = nZBins; 
      Double_t zmin = ZdEdxMin;
      Double_t zmax = ZdEdxMax;
      if (j > 2) {
	nz   =  40;
	zmin = 1.4;
	zmax = 3.4;
      }
      hists[j] = (TH1 *) new TH3F(name,title,
				  nx,xmin, xmax, nYBins,ymin, ymax,nz, zmin, zmax);
    } else {
      hists[j] = (TH1 *) new TProfile2D(name,title,
					nx,xmin, xmax, nYBins,ymin, ymax, "S");
    }
    hists[j]->SetXTitle(TitleX);
    hists[j]->SetYTitle(TitleY);
  }
}
//________________________________________________________________________________
void Hists3D::Fill(Double_t x, Double_t y, Double_t *z) {
  for (Int_t i = 0; i < fNHist; i++) {
    if (hists[i]) {
      if (i < 8) ((TH3F *) hists[i])->Fill(x,y,z[i]);
      else       ((TProfile2D *) hists[i])->Fill(x,y,z[i]);
    }
  }
}
//________________________________________________________________________________
Hists2D::Hists2D(const Char_t *Name) {
  memset(dev, 0, 3*KPidParticles*sizeof(TH2F*));
  TString nameP;
  TString title;
  const Char_t *Charge[3] = {"P","N","A"};
  const Char_t *ChargeT[3] = {"+","-","All"};
  for (Int_t hyp=0; hyp<KPidParticles;hyp++) {
    for (Int_t sCharge = 0; sCharge < 3; sCharge++) {
      nameP = Name;
      nameP += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      nameP += Charge[sCharge];
      nameP.ReplaceAll("-","");
      title = Name; title += " Log(dE/dx_{Meas}/dE/dx_{Pred}) for ";
      title += StProbPidTraits::mPidParticleDefinitions[hyp]->name().data();
      title.ReplaceAll("-","");
      title += " "; title += ChargeT[sCharge];
      title += " versus log10(p/m)";
      dev[hyp][sCharge]  = new TH2F(nameP.Data(),title.Data(),280,-1,6,2000,-5,5);
      dev[hyp][sCharge]->SetMarkerColor(hyp+2);
      dev[hyp][sCharge]->SetXTitle("log_{10}(p/m)");
      title += " Unique";
      nameP += "T";
      devT[hyp][sCharge]  = new TH2F(nameP.Data(),title.Data(),280,-1,6,2000,-5,5);
      devT[hyp][sCharge]->SetMarkerColor(hyp+2);
      devT[hyp][sCharge]->SetXTitle("log_{10}(p/m)");
    }
  }
}
