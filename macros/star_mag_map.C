#include "Riostream.h"
#include <string>
#include "TFile.h"
#include "TH2.h"
#include "TString.h"
#include "TCanvas.h"
#include "TROOT.h"
#include "TDirectory.h"
#include "TStyle.h"
using namespace std;
void star_mag_map() {
  const Char_t *files[7] = {
    "star_field_dz=0.map",  "star_field_dz=5cm.map",  "star_field_dz=10cm.map",  "star_field_dz=15cm.map",
    "star_field_plate_mu=1.map","star_field_plate_mu=1p2.map","star_field_plate_mu=1p5.map"
  };
  TFile *fOut = new TFile("StarFieldZ.root","update");
  Float_t R, Z, Br, Bz;
  char line[121];
  for (Int_t f = 4; f < 7; f++) {
    FILE *fp = fopen(files[f],"r");
    if (! fp) {
      cout << "Can't open" << files[f] << endl;
      continue;
    }
    TString name(files[f]);
    name.ReplaceAll("star_field_dz=","");
    name.ReplaceAll("star_field_plate_mu=","mu");
    name.ReplaceAll(".map","");
    TString Name("Br"); Name += name;
    TString Title("Br "); Title += files[f];
    TH2F *br = new TH2F(Name,Title,57,-285,285, 83,14-1.25,222+1.25);
    Name = "Bz"; Name += name;
    Title = "Bz "; Title += files[f];
    TH2F *bz = new TH2F(Name,Title,57,-285,285, 83,14-1.25,222+1.25);
    while (fgets(&line[0],120,fp)) {
      //      cout << line;
      if (line[0] == 'X') continue;
      Int_t n = sscanf(&line[0],"%f %f %f %f",&R, &Z, &Br, &Bz);
      if (n != 4) {
	cout << "Failed to read " << line;
	continue;
      }
      //      cout << "R " << R << "\tZ " << Z << "\tBr " << Br << "\tBz " << Bz << endl;
      Int_t bin = br->FindBin(Z,R);
      br->SetBinContent(bin,-1e-4*Br);
      bz->SetBinContent(bin,-1e-4*Bz);
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void Plot(Int_t i1 = 0, Int_t i2 = 7) {
  const Char_t *RZ[2] = {"r","z"};
  const Char_t *dZ[7] = {"0","5cm","10cm","15cm","mu1","mu1p2","mu1p5"};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  gStyle->SetOptStat(0);
  gStyle->SetTitleSize(1);
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1");
  c1->Divide(2,i2-i1);
  TH2F *brz0[2] = {0, 0};
  for (Int_t irz = 0; irz < 2; irz++) {
    for (Int_t idz = i1; idz < i2; idz++) {
      Int_t k = 1 + irz + 2*(idz-i1);
      c1->cd(k);
      TH2F *hist = (TH2F *) gDirectory->Get(Form("B%s%s",RZ[irz],dZ[idz]));
      if (! hist) continue;
        Double_t zmax = 200;
	Int_t i1 = hist->GetXaxis()->FindBin(-zmax);
	Int_t i2 = hist->GetXaxis()->FindBin( zmax);
	hist->GetXaxis()->SetRange(i1,i2);
	Int_t j2 = hist->GetYaxis()->FindBin(200.);
	hist->GetYaxis()->SetRange(1,j2);
      if (idz == 0 || idz == 4) {
	brz0[irz] = hist;
	hist->Draw("colz");
      } else {
	TH2F *newhist = new TH2F(*hist);
	newhist->SetName(Form("%s_%s",hist->GetName(),brz0[irz]->GetName()));
	newhist->SetTitle(Form("%s - %s",hist->GetTitle(),brz0[irz]->GetTitle()));
	newhist->Add(brz0[irz],-1.);
	newhist->SetXTitle("Z (cm)");
	newhist->SetYTitle("R (cm)");
#if 0
	if (idz < 4) {
	  if (irz == 0) {
	    newhist->SetMaximum(5);
	    newhist->SetMinimum(-25);
	  } else {
	    newhist->SetMaximum(50);
	    newhist->SetMinimum(0);
	  }
	}
#endif
	newhist->Draw("colz");
      }
    }
  }
}
