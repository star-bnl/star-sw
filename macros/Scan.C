#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "TGeoShape.h"
#include "TCanvas.h"
#include "TH2.h"
#include "TObjString.h"
#include "TFile.h"
#include "TLegend.h"
TString Vonew("TPCE,TIFC,TpcGas,TOFC,TpcInnerSectorAssembly,TpcOuterSectorAssembly,TpcWheelAssembly,");//TpcWheelRib,CoolingTube,FEE,FEEitself,TRIB,TpcRDOAssembly");
TString Voold("TPCE,TIFC,TPGV,TOFC,TPCW,TSEC,TSE1,TWGI,TWG1");//TRDV,TFGI,TPIP,TRDC");
//________________________________________________________________________________
void Scan(Char_t *VList = "new",
	  Int_t neta=125, Double_t etamin = 0, Double_t etamax= 2.5,
	  Int_t nphi=120, Double_t phimin=0, Double_t phimax=30, 
	  Option_t *option="etaphi") {
  TString Volist(VList);
  if (Volist == "") return;
  TFile *fOut = new TFile(Form("%s.root",VList),"recreate");
  if (Volist == "new") 
    Volist = Vonew;
  if (Volist == "old") 
    Volist = Voold;
  TObjArray *obj = Volist.Tokenize(" ;,+");
  Int_t N = obj->GetEntries();
  TH2F **hists = new TH2F*[N];
  for (Int_t i = 0; i < N; i++) {
    const Char_t *name = ((TObjString *) obj->At(i))->GetName();
    hists[i] = new TH2F(name, Form("Radiation length in %s for %s",name,option), nphi, phimin, phimax, neta, etamin, etamax);
    hists[i]->SetStats(0);
  }
  if (! gGeoManager) return;
// Generate a lego plot fot the top volume, according to option.
  Double_t degrad = TMath::Pi()/180.;
  Double_t eta, theta, phi, step, matprop, x, y;
  Double_t start[3];
  Double_t dir[3];
  TGeoNode *startnode, *endnode, *node;
  Int_t i;  // loop index for phi
  Int_t j;  // loop index for eta
  Int_t ntot = neta * nphi;
  Int_t n10 = ntot/10;
  Int_t igen = 0, iloop=0;
  for (i=1; i<=nphi; i++) {
    for (j=1; j<=neta; j++) {
      igen++;
      if (n10) {
	if ((igen%n10) == 0) printf("%i percent\n", Int_t(100*igen/ntot));
      }  
      x = 0;
      y = hists[0]->GetYaxis()->GetBinCenter(j);
      eta = y;
      theta = TMath::ACos(TMath::TanH(eta));
      phi   = hists[0]->GetXaxis()->GetBinCenter(i);
      start[0] = start[1] = start[2] = 1E-3;
      dir[0]=TMath::Sin(theta)*TMath::Cos(phi*degrad);
      dir[1]=TMath::Sin(theta)*TMath::Sin(phi*degrad);
      dir[2]=TMath::Cos(theta);
      node = gGeoManager->InitTrack(&start[0], &dir[0]); 
      //      cout << "Init node\t"; node->Print(); 
      
      startnode = gGeoManager->GetCurrentNode(); // cout << "Start node\t"; startnode->Print();
      if (gGeoManager->IsOutside()) startnode=0;
      if (startnode) {
	matprop = startnode->GetVolume()->GetMaterial()->GetRadLen();
      } else {
	matprop = 0.;
      }      
      TString path(gGeoManager->GetPath());
      //      cout << "Starting:" << path.Data() <<  endl;
      //         gGeoManager->IsStepEntering();
      // find where we end-up
      step = 0;
      endnode = startnode;
      TString pathO = path;
      for (;;) {
	startnode = endnode;    
	if (startnode) {
	  matprop = startnode->GetVolume()->GetMaterial()->GetRadLen();
	} else {
	  matprop = 0.;
	}      
	if (gGeoManager->IsOutside()) break;
	path = gGeoManager->GetPath();
	gGeoManager->FindNextBoundary(TGeoShape::Big());
	step = gGeoManager->GetStep();
	if (step>TGeoShape::Big()) break;
	endnode = gGeoManager->Step(); // cout << "End node\t"; endnode->Print();
	if (endnode==0) break;
	//	cout << "Stepping:" << path.Data() << " step = " << step << endl;
	iloop=0;
	while (!gGeoManager->IsEntering()) {
	  path = gGeoManager->GetPath();
	  iloop++;
	  //	  cout << "Stepping:" << path.Data() << " loop " << iloop <<  endl;
	  gGeoManager->SetStep(1E-3);
	  step += 1E-3;
	  endnode = gGeoManager->Step(); // cout << "End node\t"; endnode->Print();
	  if (iloop == 10) cout << "Looping in " << path.Data() << endl;
	}
	if (iloop>10) printf("%i steps\n", iloop);   
	//	TString path(startnode->GetName());
	path = gGeoManager->GetPath();
#if 0
	const Double_t *xyz = gGeoManager->GetCurrentPoint();
	cout << "Entering:" << path.Data() << "\t x = " << xyz[0] << "\t y = " << xyz[1] << "\t z = " << xyz[2] 
	     << "\t step = " << step << "\tdRL = " << x << "\tx0 = " << matprop <<  endl;
#endif
	if (matprop>0 and step > 0) {
	  x = step/matprop;
	  for (Int_t k = 0; k < N; k++) {
	    if (pathO.Contains(hists[k]->GetName())) {
	      hists[k]->Fill(phi, y, x);
	      //	      cout << "========================================== filled " << hists[k]->GetName() << endl;
	    }
	  }
	}   
	pathO = path;
      }
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void Draw(const Char_t *Vlist = "new") {
  TString Volist(Vlist);
  if (Volist == "") return;
  if (Volist == "new")     Volist = Vonew;
  if (Volist == "old")     Volist = Voold;
  TObjArray *obj = Volist.Tokenize(" ;,+");
  Int_t N = obj->GetEntries();
  TCanvas *c1 = new TCanvas();
  TH1F *frame = c1->DrawFrame(0,0,2.5,3);
  frame->SetTitle(Form("Radiation length in TPC %s",Vlist));
  frame->SetXTitle("#eta");
  frame->SetYTitle("X_{0}");
  TLegend *leg = new TLegend(0.1,0.4,0.35,0.9);
  TH1D **hists = new TH1D*[N];
  TH2F *hist = 0;
  for (Int_t i = 0; i < N; i++) {
    hist = (TH2F *) gDirectory->Get(((TObjString *) obj->At(i))->GetName());
    if (! hist) continue;
    TH1D *proj = hist->ProjectionY();
    Int_t nx = hist->GetNbinsX();
    proj->Scale(1./nx);
    proj->SetMarkerStyle(20);
    proj->SetMarkerColor(i+1);
    proj->SetLineColor(i+1);
    proj->SetLineWidth(2);
    proj->SetFillColor(i+1);
    if (i < 2) {
      if (i == 0) proj->Draw("same");
      hists[i] = proj;
      leg->AddEntry(proj,hist->GetName());
    }
    else {
      proj->Add(hists[i-1]); 
      hists[i] = proj;
      leg->AddEntry(proj,Form("+%s",hist->GetName()));
    }
  }
  for (Int_t i = N-1; i > 0; i--) {
    hists[i]->Draw("same");
  }
  leg->Draw();
}
