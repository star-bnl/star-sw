struct Hist_t {
  const Char_t *Name; 
  const Int_t opt;
};

static const Hist_t Histos[] = {
  {"BarPressure",2},           //       TProfile 
  {"inputTPCGasPressure",2},   //       TProfile 
  {"nitrogenPressure",2},      // 	TProfile 
  {"gasPressureDiff",2},       // 	TProfile 
  {"inputGasTemperature",2},   //	TProfile 
  {"outputGasTemperature",2},  //	TProfile 
  {"flowRateArgon2",2},        // 	TProfile 
  {"flowRateArgon2",2},        // 	TProfile 
  {"flowRateMethane",2},       // 	TProfile 
  {"percentMethaneIn",2},      // 	TProfile 
  {"ppmOxygenIn",2},           // 	TProfile 
  {"flowRateExhaust",2},       // 	TProfile 
  {"ppmWaterOut",2},           // 	TProfile 
  {"ppmOxygenOut",2},          // 	TProfile 
  {"flowRateRecirculation",2}, //       TProfile 
  //  {"Zdc",1},           	       // 	TH2D     
  {"Zdc3C",2},          	       // 	TH3F     
  {"Multiplicity",1},          //	TH1D     
  {"BBC",1},           	       // 	TH1D     
  {"MultiplicityPI",1},        // 	TH2D     
  {"MultiplicityPO",1},        // 	TH2D     
  {"ZdcCP",1},                 // 	TH2D     
  {"BBCP",1},                  // 	TH2D     
  //    {"L0P",1},             // 	TH2D     
  {"bbcYellowBkg",1},          //       TH2F
  {"bbcBlueBkg",1},            //       TH2F
  {"MulRow",1},                // 	TH3D     
  {"MulRowC",1},               // 	TH3D     
  {"inputTPCGasPressureP",1},  // 	TH2D     
  {"nitrogenPressureP",1},     // 	TH2D     
  //  {"gasPressureDiffP",1},  // 	TH2D     
  {"inputGasTemperatureP",1},  // 	TH2D     
  {"outputGasTemperatureP",1}, // 	TH2D     
  //  {"flowRateArgon1P",1},   // 	TH2D     
  //  {"flowRateArgon2P",1},   // 	TH2D     
  {"flowRateMethaneP",1},      // 	TH2D     
  {"percentMethaneInP",1},     // 	TH2D     
  {"percentMethaneInPC",1},    // 	TH2D     
  {"percentMethaneInPA",1},    // 	TH2D     
  //  {"ppmOxygenInP",1},      // 	TH2D     
  //  {"flowRateExhaustP",1},  // 	TH2D     
  {"ppmWaterOutP",1},          // 	TH2D     
  {"ppmWaterOutPC",1},         // 	TH2D     
  {"ppmWaterOutPA",1},         // 	TH2D     
  //  {"ppmOxygenOutP",1},         // 	TH2D     
  {"flowRateRecirculationP",1},//	TH2D     
  {"Pressure",2}, 	       //	TH3D     
  //    {"PressureA",1},       //      	TH3D     
  //    {"PressureC",4},       //     	TH3D     < ProjectionZ
  {"Time",1},                  //    	TH2D     
  //    {"TimeP",1},	       //	TH2D     
  {"TimeC",3}                  //	TH2D     < ProjectionY
  {"VoltageI",13},             //       TH3F  Voltage->ProjectionY("VoltageI",1,13)
  {"VoltageO",45},             //       TH3F  Voltage->ProjectionY("VoltageO",14,45)  
  {"AcChargeI",13},            //       TH3F  AcCharge->ProjectionY("AcChargeI",1,13)
  {"AcChargeO",45},            //       TH3F  AcCharge->ProjectionY("AcChargeO",14,45)  
  {"AvCurrentI",13},           //       TH3F  AvCurrent->ProjectionY("AvCurrentI",1,13)
  {"AvCurrentO",45},           //       TH3F  AvCurrent->ProjectionY("AvCurrentO",14,45)  
  {"SecRow3CI",113},            //      TH3F  SecRow3C->ProjectionZ("SecRow3CI",0,-1,1,13)
  {"SecRow3CO",145},            //      TH3F  SecRow3C->ProjectionZ("SecRow3CO",0,-1,14,45)  
  {"TracklengthInTpcTotal",1}, //       TH1F
  {"TracklengthInTpc",1}       //       TH1F
}; 
//________________________________________________________________________________
void RunSummary() {
  const Int_t N = sizeof(Histos)/sizeof(Hist_t); cout << " N " << N << endl;
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    //    m_Bichsel = Bichsel::Instance();
  }
  gROOT->LoadMacro("dEdxFit.C+");
  TFileSet dirs("./");
  TDataSetIter next(&dirs,0);
  TString Tuple("Run:run");
  for (Int_t i = 0; i < N; i++) {Tuple += ":"; Tuple += Histos[i].Name;} 
  cout << "Tuple: \t" << Tuple << endl;
  TNtuple *SumT = new TNtuple("SumT","Summary of dE/dx information for each run",Tuple.Data()); 
  Float_t *params = new Float_t[N+2]; 
  TDataSet *set = 0; 
  Int_t run = 0;
  Int_t Run = 0;
  Int_t nFile = 0;
  TString dir(".");
  TString Name;
  TString title;
  while ( (set = next()) ) {
    cout << set->GetName() << "\t" << set->GetTitle() << endl;
    Name = set->GetName();
    title = set->GetTitle();
    cout << "File::Name:" << Name.Data() << "\tTitle:" << title.Data() << endl;
    if (title == "directory") {dir = Name;  cout << "Set dir " << dir << endl; continue;}
    if (title != "file") continue;
    if (! Name.EndsWith(".root")) continue;
    if ( Name.BeginsWith("All") || Name.BeginsWith("SecRow") ) continue;
    TString File(dir);
    File += "/";
    File += Name;
    cout << "Open " <<  File;
    TFile *f = new TFile(File.Data());
    if (! f) {cout << "====================== failed " << endl; return; continue;}
    Name.ReplaceAll("adc_",""); 
    sscanf(Name.Data(),"%i",&run); 
    Run = run%1000000;
    cout << " for Run " << run << "/" << Run << "\t" << nFile++ << endl;
    params[0] = run;
    params[1] = Run;
    Int_t p = 2;
    for (Int_t i = 0; i < N; i++) {
      TH1 *hist = 0;
      TString Name(Histos[i].Name);
      Int_t opt = Histos[i].opt;
      if (opt%100 == 13 || opt%100 == 45) {
	TString NameP(Name,Name.Length()-1);
	TH3F *h = (TH3F *) f->Get(NameP);
	if (! h) {
	  cout << "Histogram : " << N << " for " << Name << "\t" << opt << "\t missing" << endl; 
	  continue;
	}
	if (opt == 13) {hist = h->ProjectionY(Name,1,13); opt = 1;}
	if (opt == 45) {hist = h->ProjectionY(Name,14,45); opt = 1;}
	if (opt == 113) {hist = h->ProjectionZ(Name,0,-1,1,13); opt = 3;}
	if (opt == 145) {hist = h->ProjectionZ(Name,0,-1,14,45); opt = 3;}
      } else {
	hist = (TH1 *) f->Get(Name);
      }
      if (! hist) {cout << "Histogram : " << Name << "\t" << opt << "\t missing" << endl; continue;}
      if (opt <= 2) {
	params[p] = 0;
	params[p] = hist->GetMean(opt);
	cout << "\t" << Name << "\t" << params[p] << endl;
	p++;
      }
      else {
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
    }
    delete f;
    SumT->Fill(&params[0]);
  }
  TFile *fout = new TFile("RunSummaryN.root","recreate");
  SumT->Write();
  fout->Close();
  delete fout;
}
