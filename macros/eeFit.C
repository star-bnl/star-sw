class StEeFit;
StEeFit *fit = 0;
class BetheBloch;
void Load() {
  gROOT->LoadMacro("bfc.C");
  gSystem->Load("libPhysics");
  //#endif
  //  bfc(-2,"NoInput NoDb dEdx event flow -tpcDB",0,0,0);
  bfc(-2,"NoInput NoDb StEvent flow NoDefault -db -StDbT -tpcDB -MagF -xdf2root -geant");
  //  bfc(-2,"NoInput NoDb event NoDefault");
  gSystem->Load("StBichsel.so");
  gSystem->Load("StEeFit");

}
//________________________________________
void eeFit(Int_t Mode = 0, Int_t NEvents=0,
	   Int_t Centrality=-1, Int_t Charge=-1,
	   Int_t Nhyps = 6,
	   Int_t NEta =  16, Double_t EtaMin=-1.6, Double_t EtaMax=1.6)
{

  if (gClassTable->GetID("StEeFit") < 0) {
    Load();
//     gSystem->Load("libTable");
//     gSystem->Load("libStar2Root");
//     gSystem->Load("St_base");
//     gSystem->Load("StUtilities");
//     gSystem->Load("StChain");
//     gSystem->Load("StBFChain");
//     gSystem->Load("StarClassLibrary");
//     gSystem->Load("StEvent");
//     gSystem->Load("StFlowMaker");
//    
//     gSystem->Load("St_base");
//     gSystem->Load("StarClassLibrary");
//     gSystem->Load("StEvent");
//     gSystem->Load("StBichsel");
//     gSystem->Load("StEeFit");
  }
  TList *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(gROOT->GetListOfFiles());
  TChain *fChain = new TChain("FlowTree");
  TFile *f = 0;
  UInt_t nFiles = 0;
  while ( (f = (TFile *) next()) ) {           
    if (!f) continue;
    if (!f->Get("FlowTree")) {delete f; continue;}
    fChain->Add(f->GetName());
    printf("chained %s\n",f->GetName());
    nFiles++;
    if (NEvents > 0 &&  fChain->GetEntries() > NEvents) break;
  }
  UInt_t nEvents = fChain->GetEntries();
  printf("chained %i files with %i events\n",nFiles,nEvents);
#if 0
  Double_t  x[] = {0.100, 0.150, 0.200, 0.250, 0.300, 0.350, 0.400,
		   0.450, 0.500, 0.550, 0.600, 0.650, 0.700, 0.800,
		   0.900, 1.000, 1.100, 1.200, 1.400, 1.600, 1.800, 
		   2.000, 3.000, 5.000,10.000};
#else
  Double_t  x[] = {0.100,  0.500, 1.000, 2.000, 10.000};
#endif
  Int_t nx = sizeof (x)/ sizeof(Double_t) - 1;
  Char_t title[80], name[4];
  sprintf(title,"Centrality = %i, Charge = %+1i Hyps=%i",Centrality, Charge,Nhyps);
  if (Centrality < 0) sprintf(name,"pT_%i_%02i",Nhyps,NEta);
  else                sprintf(name,"pT%02i_%i_%02i",Centrality,Nhyps,NEta);
  TString Title(title);
  TString Name(name);
  if (Charge == -1) Name += "N";
  else              Name += "P";
  TString Out(Form("Frac%i",Mode));
  Out += Name;
  Out += ".root";
  TFile *out = new TFile(Out.Data(),"update");
  fit = new StEeFit(fChain,out,Mode);
  fit->SetCentrality(Centrality);
  fit->SetNHyps(Nhyps); // 6
  fit->SetCharge(Charge);
  TH2F *total2D = (TH2F *)out->Get(Name.Data());
  if (! total2D) total2D = new TH2F(Name.Data(),Title.Data(),nx,x,NEta,EtaMin,EtaMax);
  if (NEvents >= 0) {
    Char_t name1[20];
    for (Int_t iy = 1; iy <= NEta; iy++) { printf("Bin iy = %i\n",iy);
      TAxis *y = total2D->GetYaxis();
      Double_t ymin = y->GetBinLowEdge(iy);
      Double_t ymax = y->GetBinUpEdge(iy); printf(" Ymin/max = %f/%f\n",ymin,ymax);
      fit->SetEtaRange(ymin,ymax);
      TString Name1D(Form("%s_%02i",Name.Data(),iy));
      TString Title1D(total2D->GetTitle());
      Char_t lname[80];
      sprintf(lname,"%s; %5.2f < Eta < %5.2f",total2D->GetTitle(),ymin,ymax);
      TH1F *total = new TH1F(Name1D.Data(),
			     Form("%s; %5.2f < Eta < %5.2f",total2D->GetTitle(),ymin,ymax),nx,x);
      fit->SetTotal(total);
      fit->Init();
      if (fit->Loop(NEvents) < 1000) continue;
      fit->Fit();
    }
  }
  fit->Finish();
}

