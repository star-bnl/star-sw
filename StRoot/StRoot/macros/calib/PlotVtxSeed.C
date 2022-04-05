//////////////////////////////////////////////////////////////////////////
//                                                                      //
// PlotVtxSeed.C macro                                                  //
// Author: G. Van Buren, BNL                                            //
// Description: uses output ntuples from StVertexSeedMaker to display   //
//              results of BeamLine calibration                         //
// Usage: root PlotVtxSeed.C                                            //
//        ...without arguments, will attempt to use ntuples in current  //
//           directory with automatic plot ranges                       //
//        argument   : purpose                                          //
//        dataName   : histogram title, saves graphics to file          //
//        dir        : directory where to look for ntuples              //
//        do_stats   : show bars representing statistics for each point //
//        ilow,ihigh : define vertical range for intercept plot         //
//        slow,shigh : define vertical range for slope plot             //
//        dlow,dhigh : define horizontal range for both plots           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


// globals
const float NA = -99.;
TTree* BLpars = 0;
TGraphErrors* gr_x0 = 0;
TGraphErrors* gr_y0 = 0;
TGraphErrors* gr_dxdz = 0;
TGraphErrors* gr_dydz = 0;
TGraphErrors* gr_stats = 0;
TCanvas* c1 = 0;
TPad* ints = 0;
TPad* slos = 0;
TPad* legs = 0;
TLegend* leg = 0;
TText* t1 = 0;

float int_lo = NA;
float int_hi = NA;
float slo_lo = NA;
float slo_hi = NA;
float day_lo = NA;
float day_hi = NA;

///////////////////////////////
// function declarations

// main function
TTree* PlotVtxSeed(const char* dataName=0,
                   const char* dir=0,
                   int do_stats=0,
                   float ilow=NA, float ihigh=NA,
                   float slow=NA, float shigh=NA,
                   float dlow=NA, float dhigh=NA);

// user functions
void draw(const char* dataName=0, int do_stats=0);
void setInt(float lo,float hi) {int_lo = lo; int_hi = hi;}
void setSlo(float lo,float hi) {slo_lo = lo; slo_hi = hi;}
void setDay(float lo,float hi) {day_lo = lo; day_hi = hi;}
void unsetInt()                {int_lo = NA; int_hi = NA;}
void unsetSlo()                {slo_lo = NA; slo_hi = NA;}
void unsetDay()                {day_lo = NA; day_hi = NA;}

// helper functions
void init(const char* dir);
void prep();
void roundIt(float& min, float& max, float r);
void Waiting();

///////////////////////////////
// function implementations

TTree* PlotVtxSeed(const char* dataName,
                   const char* dir,
                   int do_stats,
                   float ilow, float ihigh,
                   float slow, float shigh,
                   float dlow, float dhigh) {
  init(dir);
  setInt(ilow,ihigh);
  setSlo(slow,shigh);
  setDay(dlow,dhigh);
  draw(dataName,do_stats);
  return BLpars;
}

void init(const char* dir) {
  TString dirStr = (dir ? dir : ".");
  cout << "In directory: " << dirStr << endl;
  cout << "Looking for BLpars.root first..." << endl;
  TFile* ff = TFile::Open(Form("file:%s/BLpars.root",dirStr.Data()));
  if (ff) {
    TKey* BLkey = ff->GetKey("BLpars");
    if (BLkey) BLpars = (TTree*) (BLkey->ReadObj());
    else cerr << "Found file, but no BLpars key!" << endl;
  } else {
    cout << "Looking for vertexseedhist.*.root second..." << endl;
    TChain* BLchain = new TChain("BLpars");
    if (BLchain->Add(Form("%s/vertexseedhist.*.root",dirStr.Data())))
      BLpars = (TTree*) BLchain;
  }
  if (!BLpars) {
    cerr << "No luck finding BeamLine ntuples!" << endl;
    exit(-1);
  }
  BLpars->SetMarkerStyle(22);
  c1 = new TCanvas("BeamLine","BeamLine Constraint",600,600);
  gStyle->SetGridColor(kGray);
}

void prep() {
  c1->Clear();
  c1->Draw();
  c1->cd();
  ints = new TPad("ints","Intercepts",0.01,0.468,0.99,0.9);
  ints->SetBottomMargin(0.);
  c1->cd();
  slos = new TPad("slos","Slopes",0.01,0.04,0.99,0.472);
  slos->SetTopMargin(0.);
  c1->cd();
  legs = new TPad("legs","Legends",0.84,0.87,0.99,0.99);
  legs->SetBottomMargin(0.);
  legs->SetTopMargin(0.);
  legs->SetLeftMargin(0.);
  legs->SetRightMargin(0.);

  ints->Draw();
  slos->Draw();
  legs->Draw();
}

void draw(const char* dataName, int do_stats) {

  prep();

  //Declaration of leaves types
  Double_t         days;
  Double_t         x0;
  Double_t         err_x0;
  Double_t         y0;
  Double_t         err_y0;
  Double_t         dxdz;
  Double_t         err_dxdz;
  Double_t         dydz;
  Double_t         err_dydz;
  Double_t         stats;
  Double_t         date;
  Double_t         fill;
  Double_t         zdc;

  // Set branch addresses.
  BLpars->SetBranchAddress("days",&days);
  BLpars->SetBranchAddress("x0",&x0);
  BLpars->SetBranchAddress("err_x0",&err_x0);
  BLpars->SetBranchAddress("y0",&y0);
  BLpars->SetBranchAddress("err_y0",&err_y0);
  BLpars->SetBranchAddress("dxdz",&dxdz);
  BLpars->SetBranchAddress("err_dxdz",&err_dxdz);
  BLpars->SetBranchAddress("dydz",&dydz);
  BLpars->SetBranchAddress("err_dydz",&err_dydz);
  BLpars->SetBranchAddress("stats",&stats);
  BLpars->SetBranchAddress("date",&date);
  BLpars->SetBranchAddress("fill",&fill);
  BLpars->SetBranchAddress("zdc",&zdc);

  Int_t nentries = (Int_t) (BLpars->GetEntries());
  gr_x0 = new TGraphErrors(nentries);
  gr_y0 = new TGraphErrors(nentries);
  gr_dxdz = new TGraphErrors(nentries);
  gr_dydz = new TGraphErrors(nentries);
  gr_stats = new TGraphErrors(nentries);

  gr_x0->SetMarkerColor(2);
  gr_y0->SetMarkerColor(4);
  gr_dxdz->SetMarkerColor(6);
  gr_dydz->SetMarkerColor(kGreen+2);
  gr_x0->SetMarkerStyle(25);
  gr_y0->SetMarkerStyle(27);
  gr_dxdz->SetMarkerStyle(25);
  gr_dydz->SetMarkerStyle(27);
  gr_x0->SetMarkerSize(0.4);
  gr_y0->SetMarkerSize(0.7);
  gr_dxdz->SetMarkerSize(0.4);
  gr_dydz->SetMarkerSize(0.7);
  gr_stats->SetFillColor(41);

  legs->cd();
  leg = new TLegend(0.001,0.04,0.960,0.999);
  leg->AddEntry(gr_x0,"x0","p");
  leg->AddEntry(gr_y0,"y0","p");
  leg->AddEntry(gr_dxdz,"dxdz","p");
  leg->AddEntry(gr_dydz,"dydz","p");
  leg->SetFillColor(42);

  float min_day = 366.0;
  float max_day = 0;
  float min_int = 0;
  float max_int = 0;
  float min_slo = 0;
  float max_slo = 0;


  for (Int_t i=0; i<nentries;i++) {
    BLpars->GetEntry(i);
    gr_x0->SetPoint(i,days,x0);
    gr_x0->SetPointError(i,0,err_x0);
    gr_y0->SetPoint(i,days,y0);
    gr_y0->SetPointError(i,0,err_y0);
    gr_dxdz->SetPoint(i,days,dxdz);
    gr_dxdz->SetPointError(i,0,err_dxdz);
    gr_dydz->SetPoint(i,days,dydz);
    gr_dydz->SetPointError(i,0,err_dydz);
    gr_stats->SetPoint(i,days,days);
    if (stats!=0)
      gr_stats->SetPointError(i,0.1,TMath::Sqrt(10.0/stats)); // stats=1000 -> err=0.1

    min_day = TMath::Min(min_day,days);
    max_day = TMath::Max(max_day,days);
    min_int = TMath::Min(min_int,TMath::Min(x0-err_x0,y0-err_y0));
    max_int = TMath::Max(max_int,TMath::Max(x0+err_x0,y0+err_y0));
    min_slo = TMath::Min(min_slo,TMath::Min(dxdz-err_dxdz,dydz-err_dydz));
    max_slo = TMath::Max(max_slo,TMath::Max(dxdz+err_dxdz,dydz+err_dydz));
  }

  float day_round = 5.0;
  float int_round = 0.05;
  float slo_round = 0.001;

  if (day_lo == NA) roundIt(min_day,max_day,day_round);
  else { min_day = day_lo; max_day = day_hi; }
  if (int_lo == NA) roundIt(min_int,max_int,int_round);
  else { min_int = int_lo; max_int = int_hi; }
  if (slo_lo == NA) roundIt(min_slo,max_slo,slo_round);
  else { min_slo = slo_lo; max_slo = slo_hi; }

  for (Int_t i=0; i<nentries;i++) {
    double d1,d2;
    gr_stats->GetPoint(i,d1,d2);
    gr_stats->SetPoint(i,d1,min_int);
  }

  ints->cd();
  TH1F* h_ints = ints->DrawFrame(min_day,min_int,max_day,max_int);
  h_ints->SetTitleSize(0.05,"Y");
  h_ints->SetYTitle("Intercepts [cm]          ");
  if (do_stats) gr_stats->Draw("2");
  gr_x0->Draw("P");
  gr_y0->Draw("P");


  slos->cd();
  TH1F* h_slos = slos->DrawFrame(min_day,min_slo,max_day,max_slo);
  h_slos->SetTitleSize(0.05,"Y");
  h_slos->SetYTitle("Slopes          ");
  //slos->Clear();
  slos->Draw();
  slos->cd();
  h_slos->Draw("Y+");
  gr_dxdz->Draw("P");
  gr_dydz->Draw("P");

  legs->cd();
  leg->Draw();

  c1->cd();
  TText* t1 = new TText;
  t1->SetTextAlign(32);
  t1->SetTextSize(0.035);
  t1->DrawText(0.5,0.02,"Day");
  t1->SetTextAlign(12);
  t1->SetTextSize(0.020);
  t1->DrawText(0.55,0.02,"[1.0,2.0] = Jan 1 EDT");
  t1->SetTextAlign(22);
  t1->SetTextSize(0.045);
  t1->DrawText(0.4,0.97,"BeamLine Constraint");
  if (dataName) t1->DrawText(0.4,0.925,dataName);

  c1->Update(); c1->Draw();

  if (dataName) c1->SaveAs(Form("%s.gif",dataName));
}

void roundIt(float& min, float& max, float r) {
  float pad = 2.0;
  float near = 0.5;
  max = r * ( ((int) (max/r - near + pad)) + ( (max>0) ? 1. : 0. ) + near );
  min = r * ( ((int) (min/r + near - pad)) - ( (min<0) ? 1. : 0. ) - near );
}

void Waiting() {
  c1->Update(); c1->Draw();
  int temp;
  printf("Waiting...\n");
  cin >> temp;
}

///////////////////////////////
// $Id: PlotVtxSeed.C,v 2.1 2012/08/22 22:31:56 genevb Exp $
// $Log: PlotVtxSeed.C,v $
// Revision 2.1  2012/08/22 22:31:56  genevb
// Place into CVS
//
// 
