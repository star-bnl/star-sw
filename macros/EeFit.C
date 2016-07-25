#define EquidistantBins
class StEeFit;
StEeFit *fit = 0;
void Load() {
  gROOT->LoadMacro("bfc.C");
  gSystem->Load("libPhysics");
  bfc(-2,"NoInput NoDb StEvent flow NoDefault -db -StDbT -tpcDB -MagF -xdf2root -geant");
  gSystem->Load("StBichsel.so");
  gSystem->Load("StEeFit");

}
//________________________________________
void EeFit(Int_t Mode = 0, Int_t Charge=-1, Int_t NEvents=0, 
	   Int_t Centrality=-1,
	   Int_t Nhyps = 6,
	   Int_t NEta =  40, Double_t EtaMin=-1., Double_t EtaMax=1)
{ // Mode <= 0 Ifit; Mode > 0 Chisq
  if (gClassTable->GetID("StEeFit") < 0) {
    Load();
  }
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(gROOT->GetListOfFiles());
  TChain *fChain = new TChain("FlowTree");
  TFile *f = 0;
  UInt_t nFiles = 0;
  TString Out("");
  while ( (f = (TFile *) next()) ) {           
    if (!f) continue;
    if (!f->Get("FlowTree")) {delete f; continue;}
    fChain->Add(f->GetName());
    cout << "chained " << f->GetName() << endl;
    nFiles++;
    if (NEvents > 0 &&  fChain->GetEntries() > NEvents) break;
    if (Out == "") {
      TString o(gSystem->BaseName(f->GetName()));
      Int_t indx = o.Index("_");
      if (indx > 0) Out = TString(o,indx);
      else {o.ReplaceAll(".root",""); Out = o;}
    }
  }
  Out += Form("_Frac%i",Mode);
  UInt_t nEvents = fChain->GetEntries();
  cout << "chained "<< nFiles << " files with " << nEvents << " events\n" << endl;
  TString Name("p_");
  if (Centrality >=0) Name += Form("%02i_",Centrality);
  Name += Form("%i_%02i",Nhyps,NEta);
  TString Title(Form("Centrality = %i, Charge = %+1i Hyps=%i",Centrality, Charge,Nhyps));
  if (Charge == -1) Name += "N";
  else              Name += "P";
  Out += Name;
  Out += ".root";
  TFile *out = new TFile(Out.Data(),"update");
  fit = new StEeFit(fChain,out, 0, Mode);
  fit->SetCentrality(Centrality);
  fit->SetNHyps(Nhyps); // 6
  fit->SetCharge(Charge);
  TH2F *total2D = (TH2F *)out->Get(Name.Data());
  if (! total2D) {
#ifndef EquidistantBins
#if 1
    //   pT
//   Double_t  x[] = {0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400,
// 		   0.450, 0.500, 0.550, 0.600, 0.650, 0.700, 0.800,
// 		   0.900, 1.000, 1.100, 1.200, 1.400, 1.600, 1.800, 
// 		   2.000, 3.000, 5.000,10.000};
    Double_t  x[] = {
        0.10,  0.12,  0.14,  0.16,  0.18,  0.20,  0.22,  0.24,  0.26,
	0.28,  0.30,  0.32,  0.34,  0.36,  0.38,  0.40,
	0.42,  0.44,  0.46,  0.48,  0.50,  0.52,  0.54,  0.56,  0.58,
	0.60,  0.62,  0.64,  0.66,  0.68,  0.70,  0.73,  0.76,  0.80,  0.84,  0.88,
	0.92,  0.97,  1.02,  1.09,  1.16,  1.24,  1.35,  1.50,  1.70,  2.00,  2.50,
	5.00,10.};
else
  Double_t  x[] = {0.100,  0.500, 1.000, 2.000, 10.000};
#endif
  Int_t nx = sizeof (x)/ sizeof(Double_t) - 1;
  total2D = new TH2F(Name.Data(),Title.Data(),nx,x,NEta,EtaMin,EtaMax);
#else
  Int_t nx = 200;
  Float_t xmin = 0.05;
  Float_t xmax = 2.05;
  total2D = new TH2F(Name.Data(),Title.Data(),nx,xmin,xmax,NEta,EtaMin,EtaMax);
#endif
  }
  if (NEvents >= 0) {
    for (Int_t iy = 1; iy <= NEta; iy++) { 
      TAxis *y = total2D->GetYaxis();
      Double_t ymin = y->GetBinLowEdge(iy);
      Double_t ymax = y->GetBinUpEdge(iy); 
      cout << "Bin iy = " << iy << "\t Ymin/max = " << ymin << "/" << ymax << endl;
      fit->SetEtaRange(ymin,ymax);
      TString Name1D(Form("%s_%02i",Name.Data(),iy));
      TString Title1D(total2D->GetTitle());
#ifndef EquidistantBins
      TH1F *total = new TH1F(Name1D.Data(),
			     Form("%s; %5.2f < Eta < %5.2f",total2D->GetTitle(),ymin,ymax),nx,x);
#else
      TH1F *total = new TH1F(Name1D.Data(),
			     Form("%s; %5.2f < Eta < %5.2f",total2D->GetTitle(),ymin,ymax),nx,xmin,xmax);
#endif
      fit->SetTotal(total);
      fit->Init();
      if (fit->Loop(NEvents) < 1000) continue;
      fit->Fit();
    }
  }
  fit->Finish();
}

