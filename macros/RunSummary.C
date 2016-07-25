/* 
   root.exe -q -b lBichsel.C dEdxFit.C+ RunSummary.C+ 
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
#include "TDirIter.h"
#endif
TF1 *FitGF(TH1 *proj, Option_t *opt="");
class Hist_t {
public:
  Hist_t (const Char_t *name = "", const Int_t Opt = 0) : Name(name), opt(Opt) {}
  //  virtual ~Hist_t() {}
  const Char_t *Name; 
  const Int_t opt;
};

static const Hist_t Histos[] = {
#if 1
  Hist_t("BarPressure",2),           //       TProfile 
  Hist_t("inputTPCGasPressure",2),   //       TProfile 
  Hist_t("nitrogenPressure",2),      // 	TProfile 
  Hist_t("gasPressureDiff",2),       // 	TProfile 
  Hist_t("inputGasTemperature",2),   //	TProfile 
  //  Hist_t("outputGasTemperature",2),  //	TProfile 
  Hist_t("flowRateArgon2",2),        // 	TProfile 
  Hist_t("flowRateMethane",2),       // 	TProfile 
  Hist_t("percentMethaneIn",2),      // 	TProfile 
  Hist_t("ppmOxygenIn",2),           // 	TProfile 
  Hist_t("flowRateExhaust",2),       // 	TProfile 
  Hist_t("ppmWaterOut",2),           // 	TProfile 
  Hist_t("ppmOxygenOut",2),          // 	TProfile 
  Hist_t("flowRateRecirculation",2), //       TProfile 
  //  Hist_t("Zdc",1),           	       // 	TH2D     
  //  Hist_t("Zdc3C",2),          	       // 	TH3F     
  Hist_t("Multiplicity",1),          //	TH1D     
  Hist_t("BBC",1),           	       // 	TH1D     
  //  Hist_t("MultiplicityPI",1),        // 	TH2D     
  //  Hist_t("MultiplicityPO",1),        // 	TH2D     
  Hist_t("ZdcCP",1),                 // 	TH2D     
  Hist_t("BBCP",1),                  // 	TH2D     
  //    Hist_t("L0P",1),             // 	TH2D     
  Hist_t("bbcYellowBkg",1),          //       TH2F
  Hist_t("bbcBlueBkg",1),            //       TH2F
  //  Hist_t("MulRow",1),                // 	TH3D     
  //  Hist_t("MulRowC",1),               // 	TH3D     
  Hist_t("inputTPCGasPressureP",1),  // 	TH2D     
  Hist_t("nitrogenPressureP",1),     // 	TH2D     
  //  Hist_t("gasPressureDiffP",1),  // 	TH2D     
  Hist_t("inputGasTemperatureP",1),  // 	TH2D     
  //  Hist_t("outputGasTemperatureP",1), // 	TH2D     
  //  Hist_t("flowRateArgon1P",1),   // 	TH2D     
  //  Hist_t("flowRateArgon2P",1),   // 	TH2D     
  Hist_t("flowRateMethaneP",1),      // 	TH2D     
  Hist_t("percentMethaneInP",1),     // 	TH2D     
  Hist_t("percentMethaneInPC",1),    // 	TH2D     
  Hist_t("percentMethaneInPA",1),    // 	TH2D     
  //  Hist_t("ppmOxygenInP",1),      // 	TH2D     
  //  Hist_t("flowRateExhaustP",1),  // 	TH2D     
  Hist_t("ppmWaterOutP",1),          // 	TH2D     
  Hist_t("ppmWaterOutPC",1),         // 	TH2D     
  Hist_t("ppmWaterOutPA",1),         // 	TH2D     
  //  Hist_t("ppmOxygenOutP",1),         // 	TH2D     
  Hist_t("flowRateRecirculationP",1),//	TH2D     
  Hist_t("Pressure",2), 	       //	TH3D     
  //    Hist_t("PressureA",1),       //      	TH3D     
  //    Hist_t("PressureC",4),       //     	TH3D     < ProjectionZ
  Hist_t("Time",4),                  //    	THnSparseF
  //    Hist_t("TimeP",4),	       //	THnSparseF
  Hist_t("TimeC",4),                  //	THnSparseF
  //  Hist_t("VoltageI",13),             //       TH3F  Voltage->ProjectionY("VoltageI",1,13)
  //  Hist_t("VoltageO",45),             //       TH3F  Voltage->ProjectionY("VoltageO",14,45)  
  //  Hist_t("AcChargeI",13),            //       TH3F  AcCharge->ProjectionY("AcChargeI",1,13)
//   Hist_t("AcChargeO",45),            //       TH3F  AcCharge->ProjectionY("AcChargeO",14,45)  
//   Hist_t("AvCurrentI",13),           //       TH3F  AvCurrent->ProjectionY("AvCurrentI",1,13)
//   Hist_t("AvCurrentO",45),           //       TH3F  AvCurrent->ProjectionY("AvCurrentO",14,45)  
  Hist_t("SecRow3CI",113),            //      TH3F  SecRow3C->ProjectionZ("SecRow3CI",0,-1,1,13)
  Hist_t("SecRow3CO",145),            //      TH3F  SecRow3C->ProjectionZ("SecRow3CO",0,-1,14,45)  
#endif
//   Hist_t("SecRow3CB", 1445),      //       TH3F  SecRow3C->ProjectionZ("SecRow3CO",14,14,14,45)  
//   Hist_t("SecRow3C_5_21", 521),   // 
//   Hist_t("SecRow3C_11_1_2", 1101),   // 
//   Hist_t("SecRow3C_12_9", 1209),   // 
//   Hist_t("SecRow3C_13_4_6", 1304),   // 
//   Hist_t("SecRow3C_14_1_2", 1401),   // 
//   Hist_t("SecRow3C_21_3_9", 2103),   // 
//   Hist_t("SecRow3C_22_1_9", 2201),   // 
  Hist_t("TracklengthInTpcTotal",1), //       TH1F
  Hist_t("TracklengthInTpc",1)       //       TH1F
}; 
//________________________________________________________________________________
void RunSummary(const Char_t *files = "*.root") {
  const Int_t N = sizeof(Histos)/sizeof(Hist_t); cout << " N " << N << endl;
#if 0
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    //    m_Bichsel = Bichsel::Instance();
  }
  gROOT->LoadMacro("dEdxFit.C+");
#endif
  TFile *fout = new TFile("RunSummaryN.root","recreate");
  TString Tuple("Run:run");
  for (Int_t i = 0; i < N; i++) {Tuple += ":"; Tuple += Histos[i].Name;} 
  cout << "Tuple: \t" << Tuple << endl;
  TNtuple *SumT = new TNtuple("SumT","Summary of dE/dx information for each run",Tuple.Data()); 
  Int_t cachesize = 10000000; //this is the default value: 10 MBytes
  SumT->SetCacheSize(cachesize);
  Float_t *params = new Float_t[N+2]; 
  TDataSet *set = 0; 
  Int_t run = 0;
  Int_t Run = 0;
  Int_t nFile = 0;
  TString title;
  TDirIter Dir(files);
  const Char_t *file = 0;
  while ( (file = Dir.NextFile()) ) {
    TString File = file;
    cout << "File::Name:" << File.Data() << endl;
    if (! File.EndsWith(".root")) continue;
    if ( File.BeginsWith("All") || File.BeginsWith("SecRow") ) continue;
    if ( File.BeginsWith("RunSummary") ) continue;
    cout << "Open " <<  File;
    TFile *f = new TFile(File.Data());
    if (! f) {cout << "====================== failed " << endl; return; continue;}
    TString F = gSystem->BaseName(File);
    F.ReplaceAll("adc_","");
    F.ReplaceAll("st_W_",""); 
    sscanf(F.Data(),"%i",&run); 
    Run = run%1000000;
    cout << " for Run " << run << "/" << Run << "\t" << File << "\t" << nFile++ << endl;
    params[0] = run;
    params[1] = Run;
    Int_t p = 2;
    for (Int_t i = 0; i < N; i++) {
      TH1 *hist = 0;
      TString Name(Histos[i].Name);
      Int_t opt = Histos[i].opt;
      TString NameP = Name;
      if (Name.BeginsWith("SecRow3C")) NameP = "SecRow3C";
      TObject *obj = f->Get(NameP);
      if (! obj) {
	cout << "Histogram : " << N << " for " << Name << "\t" << opt << "\t missing" << endl; 
	continue;
      }
      if         (obj->IsA()->InheritsFrom( "TH3" )) {
	TH3 *h = (TH3 *) obj;
	if      (opt ==1445) {hist = h->ProjectionZ(Name,14,14,14,45); opt = 3;}
	else if (opt == 521) {hist = h->ProjectionZ(Name, 5, 5,21,21); opt = 3;}
	else if (opt ==1101) {hist = h->ProjectionZ(Name,11,11, 1, 2); opt = 3;}
	else if (opt ==1209) {hist = h->ProjectionZ(Name,12,12, 9, 9); opt = 3;}
	else if (opt ==1304) {hist = h->ProjectionZ(Name,13,13, 4, 6); opt = 3;}
	else if (opt ==1401) {hist = h->ProjectionZ(Name,14,14, 1, 2); opt = 3;}
	else if (opt ==2103) {hist = h->ProjectionZ(Name,21,21, 3, 9); opt = 3;}
	else if (opt ==2201) {hist = h->ProjectionZ(Name,22,22, 1, 9); opt = 3;}
	else if (opt ==  13) {hist = h->ProjectionY(Name,1,13); opt = 1;}
	else if (opt ==  45) {hist = h->ProjectionY(Name,14,45); opt = 1;}
	else if (opt == 113) {hist = h->ProjectionZ(Name,0,-1,1,13); opt = 3;}
	else if (opt == 145) {hist = h->ProjectionZ(Name,0,-1,14,45); opt = 3;}
	else {
	  if      (opt == 1) hist = h->ProjectionX();
	  else if (opt == 2) hist = h->ProjectionY();
	  else if (opt == 3) hist = h->ProjectionZ();
	}
      } else if (obj->IsA()->InheritsFrom( "TH2" )) {
	TH2 *h = (TH2 *) obj;
	hist = h->ProjectionX();
      } else if (obj->IsA()->InheritsFrom( "TH1" )) {
	hist = (TH1 *) obj;
      } else if (obj->IsA()->InheritsFrom( "TProfile" )) {
	hist = (TProfile *) obj;
      } else if (obj->IsA()->InheritsFrom( "THnSparseT<TArrayF>" )) {
	THnSparseF *h = (THnSparseF *) obj;
	hist = h->Projection(1);
      } else {
	hist = 0;
      }
      if (! hist) {
	cout << "Histogram : " << Name << "\t" << opt << "\t missing" << endl; 
	continue;
      }
      if (opt <= 2) {
	params[p] = 0;
	params[p] = hist->GetMean(opt);
	cout << "\t" << Name << "\t" << params[p] << endl;
	p++;
      } else {
	TF1 *f1 = 0;
	params[p] = -9999.;
	if (hist->GetEntries() > 1000) {
	  f1 = FitGF(hist,"q");
	}
	
	if (f1) {
	  params[p] = f1->GetParameter(1);
	  cout << "\t" << Name << "\t" << params[p] << endl;
	}
	p++;
      }
      delete hist;
    }
    delete f;
    SumT->Fill(&params[0]);
  }
  fout->cd();
  SumT->Write();
  fout->Close();
  delete fout;
}
