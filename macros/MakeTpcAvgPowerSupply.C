/* 
   root.exe -b -q  lmysql.C 'MakeTpcAvgPowerSupply.C+(2017)'
   put2DB.pl 'StarDb/Calibrations/tpc/TpcAvg*.root'
   root.exe  'Db.C("StarDb/Calibrations/tpc/TpcAvgPowerSupply")'
Test
   root.exe 'Db.C("StarDb/Calibrations/tpc/TpcAvgPowerSupply",20160509,114213)'
TH2F *voltP = new TH2F("VoltP","Voltage versus sector and row, TpcAvgPowerSupplyC",24,0.5,24.5,45,0.5,45.5);
TH2F *voltHV = new TH2F("VoltHV","Voltage versus sector and row, tpcAnodeHVC",24,0.5,24.5,45,0.5,45.5);
TH2F *voltHVA = new TH2F("VoltHVA","Voltage versus sector and row, tpcAnodeHVavgC",24,0.5,24.5,45,0.5,45.5);
for (Int_t s = 1; s <= 24; s++) {for (Int_t r = 1; r <=45; r++) {Double_t V = St_TpcAvgPowerSupplyC::instance()->voltagePadrow(s,r); cout << "s/r=" << s << "/" << r << " V = " << V; if (r <=13 && TMath::Abs(V-1100)>5 || r > 13 &&TMath::Abs(V-1390)>5) cout << " =========================="; cout << endl; voltP->Fill(s,r,V);}}

for (Int_t s = 1; s <= 24; s++) {for (Int_t r = 1; r <=45; r++) {Double_t V = St_tpcAnodeHVC::instance()->voltagePadrow(s,r); voltHV->Fill(s,r,V);}}
for (Int_t s = 1; s <= 24; s++) {for (Int_t r = 1; r <=45; r++) {Double_t V = St_tpcAnodeHVavgC::instance()->voltagePadrow(s,r); voltHVA->Fill(s,r,V);}}
 */
#include <stdio.h>
#include "Riostream.h"
#include "TSystem.h"
#include "tables/St_TpcAvgPowerSupply_Table.h"
#include "TUnixTime.h"
#include "TString.h"
#include "TMath.h"
#include "Riostream.h"
#include "TString.h"
#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TDatime.h"
#include "TCernLib.h" 
#include "TFile.h"
#include "TLinearFitter.h"
#include "Ask.h"
#include "TGraph.h"
#include "TArrayD.h"
#include "TArrayF.h"
#include "TROOT.h"
#include "TF1.h"
#include "TNtuple.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include <vector>
static TLinearFitter *lf = 0;
static Int_t _debug = 1;
class Run_t {
public:
  Run_t(Int_t r, const Char_t *Start,const Char_t *Stop) : run(r), start(Start), stop(Stop) {/* start.Print(); stop.Print(); */}
  virtual ~Run_t() {}
  Int_t run;
  TDatime start;
  TDatime stop;
  void Print() {
    cout << "Run\t" << run;
    cout << " start " << start.AsSQLString();
    cout << " stop " << stop.AsSQLString() << endl;
  }
};
struct M_t {
  M_t(Int_t x, Float_t C, Float_t V) : _x(x), _C(C), _V(V) {};
  Int_t   _x; // utime
  Float_t _C; // current
  Float_t _V; // Voltage
};
/* online/RTS/src/TPC_ANODE/tpc_anode.h
There are following status bits (I got this from 1471 module manual):
Reason:
0 Channel is enabled
1 Output is ramping to a higher absolute value.
2 Output is ramping to a lower absolute value or zero
5 Trip for violation of supply limits
6 Trip for violation user's current limit
7 Trip for voltage error
8 Trip for violation of voltage limit
9 Thermal Overload
10 Trip for violation user's Peak current limit
11 Trip for ARC
12 reserved
13 reserved
14 reserved
*/   
#define NUM_CARDS       12
#define NUM_CHANNELS    8
/*
extern    float tpcVoltages[2][NUM_CARDS][NUM_CHANNELS];
extern    float tpcCurrents[2][NUM_CARDS][NUM_CHANNELS];
extern    short tpcStatus[2][NUM_CARDS][NUM_CHANNELS];
 */
//________________________________________________________________________________
Int_t sector(Int_t module = 0, Int_t channel = 0) {
  return 1 + 2*module + channel/4;
}
//________________________________________________________________________________
Int_t Cut3Rms(TGraph *graph) {
  Double_t mean = graph->GetMean(2);
  Double_t rms  = graph->GetRMS(2);
  Int_t N  = graph->GetN();
  Int_t NI = N - 3;
  if (NI > 5) NI = 5;
  Int_t iter = 0;
  for (; iter < NI; iter++) {
    Int_t worst = -1;
    Double_t dYworst = -1;
    Double_t rms  = graph->GetRMS(2);
    if (rms < 1e-7) return iter;
    Double_t mean = graph->GetMean(2);
    if (rms < 0.5*(TMath::Abs(mean) + 0.001)) return iter;
    for (Int_t i = 0; i < N; i++) {
      Double_t dY = TMath::Abs(graph->GetY()[i] - mean);
      if ((rms > 0.2*mean || dY > 3*rms) && dY > dYworst) {
	worst = i;
	dYworst = dY;
      }
    }
    if (worst < 0) return 0;
    graph->RemovePoint(worst); N--;
  }
  return iter+1;
}
//________________________________________________________________________________
Int_t ParseString (const Char_t *string, TObjArray &Opt) {
  Opt.Clear();
  TString tChain(string);
  TObjArray *obj = tChain.Tokenize("[^ ;,]+");
  Opt = *obj;
  Int_t nParsed = Opt.GetEntries();
  if (_debug > 4) {
    for (Int_t k = 0; k < nParsed; k++) {
      if (Opt.At(k)) {
	cout << k << "\t" << ((TObjString *) Opt.At(k))->GetName() << endl;
      }
    }
  }
  return nParsed;
}
//________________________________________________________________________________
Int_t readF(const Char_t *string, Float_t *array) {
  TObjArray Opt;
  Int_t N = ParseString(string,Opt);
  for (Int_t k = 0; k < N; k++) {
    array[k] = TString(((TObjString *) Opt.At(k))->GetName()).Atof();
  }
  return N;
}
//________________________________________________________________________________
Int_t FitGraph(TGraph *graph = 0, Int_t iv = 0) {
  Int_t iok = 0;
  if (! graph) return iok;
  //  graph->Print("");
  if (! lf) {
    lf = new TLinearFitter(2);
    TF1 *f1 = new TF1("f1","1++x",-500,500);
    lf->SetFormula(f1);
    lf->SetUserFunc(f1);
  }
  lf->ClearPoints();
  Int_t n = graph->GetN();
  if (n < 1) {
    cout << n << " points is not enought to fit" << endl;
    return iok;
  }
  TArrayD X(n,graph->GetX()); Double_t *x = X.GetArray();
#if 0
  Double_t xav = 0;
  for (Int_t l = 0; l < n; l++) {
    xav += x[l];
  }
  xav /= n;
  for (Int_t l = 0; l < n; l++) {
    x[l] -= xav;
  }
#endif  
  TArrayD Y(n,graph->GetY()); Double_t *y = Y.GetArray();
  TArrayD E(n); Double_t *e = E.GetArray();
  
  for (Int_t i = 0; i < n; i++) if (!iv ) e[i] = 0.01; else e[i] = 0.1; // Current or Voltage
  lf->AssignData(n, 1, x, y, e);
  //Perform the fitting and look at the results
  Double_t h = 0.80;
  TBits bits(n);
  if (n < 6) lf->Eval();
  else      {lf->EvalRobust(h);   lf->GetFitSample(bits);}
  TVectorD params(2);
  TVectorD errors(2);
  lf->GetParameters(params);
  //  lf->GetErrors(errors);
#if 0
  for (Int_t i = 0; i < 2; i++) {
    cout << Form("par[%d]=%f ", i, params(i));
  }
  Double_t chisquare=lf->GetChisquare();
  cout << Form(" chisq = %f", chisquare) << endl;
#endif  
  //  bits.Print();
  Int_t j = 0;
  TF1 *func = (TF1*) lf->GetUserFunc();
  Double_t yav = 0;
  TArrayD Xold(X);
  TArrayD Yold(Y);
  TArrayD Eold(E);
  for (Int_t l = 0; l < n; l++) {
    Double_t res = y[l] - func->Eval(x[l]);
    Double_t dev = res/e[l];
    yav     += y[l];
    if (_debug > 1) {
      cout << Form("%2i x %8.3f y %8.3f +/- %8.2f res = %8.0f dev = %7.2f Bit: %1i",
		   l,x[l],y[l],e[l],1e4*res,dev,bits.TestBitNumber(l)) << endl;
    }
    if (n >= 4 && ! bits.TestBitNumber(l) && TMath::Abs(dev) > 5)  continue;
    x[j]   = x[l];
    y[j]   = y[l];
    e[j]   = e[l];
    if (e[j] < 1e-7) e[j] = 0.01;
    j++;
  }
  if (j >= 3) {
    yav /= n;
    lf->ClearPoints();
    lf->AssignData(j, 1, x, y, e);
    lf->Eval();
    lf->GetParameters(params);
    lf->GetErrors(errors);
    if (_debug > 1) {
      if (! iv) cout << "<Current> ";
      else      cout << "<Voltage> ";
      for (Int_t i=0; i<2; i++)   cout << Form("par[%d]=%8.5f +/- %8.5f\t", i, params(i), errors(i));
      Double_t chisquare=lf->GetChisquare();
      cout << Form("chisq = %f", chisquare) << endl;
    }
  }
  if (! gROOT->IsBatch() && Ask()) return 1;
  return iok;
}
#if 0
//________________________________________________________________________________
Int_t readI(const Char_t *string, Int_t *array) {
  TObjArray Opt;
  Int_t N = ParseString(string,Opt);
  for (Int_t k = 0; k < N; k++) {
    array[k] = TString(((TObjString *) Opt.At(k))->GetName()).Atoi();
  }
  return N;
}
#endif
#include "OnDb.h"
//________________________________________________________________________________
TSQLServer *OnlDbServer(const Char_t *dataset = "Conditions_rich", Int_t year = 2013) {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database = OnDb(year,dataset);
  TString dbT("mysql://"); dbT += database; dbT += "/"; dbT += dataset; //Conditions_rich";
  TSQLServer *db = TSQLServer::Connect(dbT.Data(),"", ""); if (! db) {cout << "Can't connect " << dbT.Data() << endl; return db;}
  return db;
}
//________________________________________________________________________________
Int_t LastProcessedRun(TpcAvgPowerSupply_st *avgI, Double_t AcCharge[2]) {
  Int_t LastRun = -1;
  TFileSet *dir = new TFileSet(".");
  TDataSetIter next(dir);
  TDataSet *s = 0;
  TDataSet *slast = 0;
  Int_t dOld = 0, tOld = 0;
  Int_t d = 0, t = 0;
  while ((s = next())) {
    if (TString(s->GetTitle()) != "file") continue;
    TString name(s->GetName());
    if (! name.Contains("TpcAvgPowerSupply") || ! name.Contains(".root")) continue;
    Int_t n = sscanf(name.Data(),"TpcAvgPowerSupply.%d.%d.root",&d,&t);
    if (n != 2) continue;
    if (d < dOld) continue;
    if (d == dOld && t < tOld) continue;
    dOld = d;
    tOld = t;
    slast = s;
  }
  if (slast) {
    TString name(slast->GetName());
    TFile *f = new TFile(name.Data());
    if (f) {
      St_TpcAvgPowerSupply *p = (St_TpcAvgPowerSupply *) f->Get("TpcAvgPowerSupply");
      if (p) {
	memcpy(avgI, p->GetTable(), sizeof(TpcAvgPowerSupply_st));
	LastRun = avgI->run;
	for (Int_t io = 0; io < 2; io++) {
	  for (Int_t module = 0; module < NUM_CARDS; module++) {
	    for (Int_t channel = 0; channel < NUM_CHANNELS; channel++) {
	      Int_t sec  = sector(module,channel);
	      Int_t socket  = channel%4 + 4*io + 1;
	      Int_t l    = 8*(sec-1)+socket-1;
	      AcCharge[io] += avgI->Charge[l];
	    }
	  }
	}
      }
      delete f;
    }
  }
  return LastRun;
}
//________________________________________________________________________________
void MakeTpcAvgPowerSupply(Int_t year = 2017) {
  TDatime d(10000*(year-1) + 1201,0);
  Int_t u95 = d.Convert();
  TDatime nextyear(10000*(year+1) + 101, 0);
  Int_t uNext = nextyear.Convert();
  struct FitP_t {
    Float_t run, uBegin, uStop, uEnd, channel, module, io, sector, socket, meanV, rmsV, Vfit, meanC, rmsC, Cfit, Charge, FitStatus, AcChargeI, AcChargeO, np;
  };
  TFile *fSumF = new TFile(Form("MakeTpcAvgPowerSupply.%i.root",year),"update");
  TNtuple *FitP = 0;
  if (fSumF) FitP = (TNtuple *) fSumF->Get("FitP");
  if (!FitP) {
    delete fSumF;
    fSumF = new TFile(Form("MakeTpcAvgPowerSupply.%i.root",year),"recreate");
    FitP = new TNtuple("FitP","Fit results",
		       "run:uBegin:uStop:uEnd:channel:module:io:sector:socket:meanV:rmsV:Vfit:meanC:rmsC:Cfit:Charge:FitStatus:AcChargeI:AcChargeO:np");
    FitP->SetMarkerStyle(20);
    FitP->SetLineWidth(2);
    FitP->SetAutoFlush(-10000000);
  }
  FitP_t Point;
  // List of runs
  //select beginTime,runNumber,from_unixtime(startRunRTS),from_unixtime(startRunDaq),from_unixtime(startRunTrg),from_unixtime(endRunRTS) from  runUpdateStatus limit 20;
  TSQLServer *RunLog = OnlDbServer("RunLog",year);
  //  TString sql("SELECT runNumber,from_unixtime(startRunRTS),from_unixtime(endRunRTS) from runUpdateStatus where beginTime > \"2012-04-23 10:09:15\" order by beginTime limit 2;");
  //  TString sql("SELECT runNumber,from_unixtime(startRunRTS),from_unixtime(endRunRTS) from runUpdateStatus where beginTime > \"2016-01-13\" order by beginTime;");
  //  TString sql("SELECT runNumber,from_unixtime(startRunRTS),from_unixtime(endRunRTS) from runUpdateStatus order by beginTime;");
  //  TString sql("SELECT runNumber,from_unixtime(firstEventTime),from_unixtime(lastEventTime) from daqSummary  order by beginTime;");
  TString sql("SELECT runNumber,from_unixtime(firstEventTime),from_unixtime(lastEventTime),firstEventTime,lastEventTime from daqSummary  order by beginTime;");
  TSQLResult *res = RunLog->Query(sql.Data());
  Int_t Nruns = res->GetRowCount(); cout << "Got " << Nruns << " rows in result" << endl;
  Run_t **runs = new Run_t*[Nruns+1];
  Int_t nfields = res->GetFieldCount();
  Int_t NoGoodRuns = 0;
  TString names[nfields];
  TSQLRow *row;
  Bool_t IsBeenZeroSaved = kFALSE;
  
  Double_t AcCharge[2] = {0, 0};
  for (Int_t i = 0; i < nfields; i++) {
    names[i] = res->GetFieldName(i);
    cout << Form("%20s", names[i].Data());
  }
  cout << endl;
  for (Int_t i = 0; i < Nruns; i++) {
    row = res->Next();
    if (_debug > 2) {    
      for (Int_t j = 0; j < nfields; j++) {
	cout << Form("%i %20s = %s", j, names[j].Data(), row->GetField(j)) << endl;
      }
    }
    Int_t u1 = TString(row->GetField(3)).Atoi();
    Int_t u2 = TString(row->GetField(4)).Atoi();
    if (u1 <= u95 || u1 > uNext || u2 <= u95 || u2 > uNext  || u2 <= u1) continue;
    runs[NoGoodRuns] = new Run_t(TString(row->GetField(0)).Atoi(),row->GetField(1), row->GetField(2));
    //    runs[NoGoodRuns]->stop.Set(row->GetField(2));
    runs[NoGoodRuns]->Print();
    NoGoodRuns++;
  }
  runs[NoGoodRuns] = new Run_t(*runs[NoGoodRuns-1]);
  runs[NoGoodRuns]->start = runs[NoGoodRuns]->stop;
  runs[NoGoodRuns]->Print();
  delete RunLog;
  //  return;
  TSQLServer *daq    = OnlDbServer("Conditions_daq",year);
  const Char_t *IO[2] = {"Inner","Outer"};
  TpcAvgPowerSupply_st avgI; memset(&avgI, 0, sizeof(TpcAvgPowerSupply_st)); // Integrated
  TpcAvgPowerSupply_st avgT; memset(&avgT, 0, sizeof(TpcAvgPowerSupply_st)); // Tripped
  for (Int_t i = 0; i < 192; i++) avgT.Voltage[i] = -999.;
  Int_t runL = LastProcessedRun(&avgI, AcCharge);
  for (Int_t r = 0; r < NoGoodRuns; r++) {
    if (! runs[r]) continue;
    if (r > 0) SafeDelete(runs[r-1]);
    if (runs[r]->run <= runL) {
      //      SafeDelete(runs[r]); 
      continue;
    }
    Int_t uBegin = runs[r]->start.Convert();
    Int_t uStop  = runs[r]->stop.Convert();
    Int_t uLast  = uStop;
    Int_t uTrip  = -1;
    Int_t uLastNonTrip = -1;
    if (uStop - uBegin < 10) {
      //      SafeDelete(runs[r]); 
      continue; // ignore short runs
    }
    Int_t uEnd   = uStop;
    if (r < NoGoodRuns-1) uEnd =  runs[r]->stop.Convert();
    TpcAvgPowerSupply_st avgC; memset(&avgC, 0, sizeof(TpcAvgPowerSupply_st)); // Current
    avgC.run = runs[r]->run;
    avgT.run = runs[r]->run;
    vector<M_t> mXCV[2][NUM_CARDS][NUM_CHANNELS];
#if 0
    if (avgC.run < 16166017) {
      //      SafeDelete(runs[r]); 
      continue;
    };
#endif
    avgC.start_time = uBegin;
    avgC.stop_time  = uStop;
    TCL::ucopy(avgI.Charge,avgC.Charge,192); 
    Double_t CurrentsSum = 0;
    Int_t NoRowsRead = 0;
    memset(&Point.run, 0, sizeof(Point));
    Point.run = avgC.run;
    Point.uBegin = uBegin;
    Point.uStop = uStop;
    Point.uEnd = uEnd;
    for (Int_t io = 0; io < 2; io++) {
      if (! runs[r]) continue;
      //      TString Sql(Form("select UNIX_TIMESTAMP(beginTime),Voltages,Currents,Status,Reason from Conditions_daq.tpcPowerSupply%s ",IO[io]));
      TString Sql(Form("select beginTime,Currents,Reason,Voltages from Conditions_daq.tpcPowerSupply%s",IO[io]));
      Sql += " WHERE beginTime > \"";
      Sql += runs[r]->start.AsSQLString();
      if (r < NoGoodRuns - 1) {
	Sql += " \" and beginTime < \"";
	//      Sql += runs[r]->stop.AsSQLString();
	//	Sql += runs[r+1]->start.AsSQLString();
	Sql += runs[r]->stop.AsSQLString();
      }
      Sql += "\"  order by beginTime;";
      if (_debug > 2) cout << Sql.Data() << endl;
      res = daq->Query(Sql.Data());
      if (! res) {
	//	SafeDelete(runs[r]); 
	continue;
      };
      nfields = res->GetFieldCount();
      for (Int_t i = 0; i < nfields; i++) names[i] = res->GetFieldName(i);
      
      Int_t nrows = res->GetRowCount();
      if (_debug > 2)   cout << "Got rows " << nrows << " in result for run " << runs[r]->run << endl;
      if (! nrows) {
	delete res;
	continue;
      }
      NoRowsRead += nrows;
      Int_t Reason; // time, Status[96], 
      Float_t Currents[96], Voltages[96];
      for (Int_t i = 0; i < nrows; i++) {
	row = res->Next(); 
	if (_debug > 2) {
	  for (int j = 0; j < nfields; j++) {
	    cout << Form("%20s = %s\n", names[j].Data(), row->GetField(j));
	  }
	  cout << endl;
	  cout << "[0]" << row->GetField(0) << endl;
	}
	TDatime t(row->GetField(0));//  t.Print(); 
	Int_t u = t.Convert();
	if (u > uLast) {
	  //	  SafeDelete(runs[r]); 
	  continue;
	};
	Int_t N;
	N = readF(row->GetField(1),  Currents); assert(N == 96);
	N = readF(row->GetField(3),  Voltages); assert(N == 96);
	if (_debug > 2) {
	  for (Int_t k = 0; k < 96; k++) cout << Currents[k] << ",";
	  cout << endl;
	  for (Int_t k = 0; k < 96; k++) cout << Voltages[k] << ",";
	  cout << endl;
	}
	Reason = TString(row->GetField(2)).Atoi();
	if (_debug > 2) {
	  cout << "Reason " << Reason << endl;
	}
	if (! Reason) {
	  if (u > uLastNonTrip) uLastNonTrip = u;
	} else if (Reason != 1) {
	  if (
	      (Reason & 1 << 5) || // 5 Trip for violation of supply limits
	      (Reason & 1 << 6) || // 6 Trip for violation user's current limit
	      (Reason & 1 << 7) || // 7 Trip for voltage error
	      (Reason & 1 << 8)    // 8 Trip for violation of voltage limit
	      ) {
	    if (uTrip < 0 || u < uTrip) {uTrip = u; uLast = uTrip;}
	  }
	}
	for (Int_t k = 0; k < 96; k++) {
	  if (Currents[k] < 0.0) Currents[k] = 0;
	  if (Voltages[k] < 0.0) Voltages[k] = 0;
	  Int_t channel = k%8;
	  Int_t module  = k/8;
	  mXCV[io][module][channel].push_back(M_t(u,Currents[k],Voltages[k]));
	}
	delete row;
      }
      delete res;
    }
    if (! NoRowsRead) {
      //      SafeDelete(runs[r]); 
      continue;
    }
    // Fit
    Int_t failed = 0;
    for (Int_t io = 0; io < 2; io++) {
      Point.io = io;
      for (Int_t module = 0; module < NUM_CARDS; module++) {
	Point.module = module;
	for (Int_t channel = 0; channel < NUM_CHANNELS; channel++) {
	  Point.channel = channel;
	  Int_t sec  = sector(module,channel); Point.sector = sec;
	  Int_t socket  = channel%4 + 4*io + 1; Point.socket = socket;
	  if (_debug > 1) cout << "sector = " << sec << " socket = " << socket << endl;
	  Int_t l    = 8*(sec-1)+socket-1;
	  UInt_t n = mXCV[io][module][channel].size();
	  if (n < 3) {
	    if (_debug > 1) cout << "mXCV[" << io << "][" << module << "][" << channel << "] = " << n << " skipped." << endl;
	    continue;
	  } 
	  TArrayD x(n), c(n), V(n);
	  Double_t xav = 0;
	  for (UInt_t j = 0; j < n; j++) {
	    x[j] = mXCV[io][module][channel][j]._x;
	    xav += x[j];
	    c[j] = mXCV[io][module][channel][j]._C;
	    V[j] = mXCV[io][module][channel][j]._V;
	  }
	  xav /= n;
	  for (UInt_t j = 0; j < n; j++) {
	    x[j] -= xav;
	  }
	  Int_t iok0 = 0, iok1 = 0;
	  TGraph *graphC = new TGraph(n,x.GetArray(),c.GetArray());
	  Cut3Rms(graphC);
	  Double_t mean   = graphC->GetMean(2);
	  Double_t rms    = graphC->GetRMS(2);
	  Point.meanC = mean;
	  Point.rmsC  = rms;
	  Point.np    = n;
	  avgC.Current[l] = mean;
	  if (rms/(n-1) > 1e-2) { // 0.1 uA
	    iok0 = FitGraph(graphC,0);
	    if (iok0 > 0 || !lf) {
	      failed++;
	    } else {
	      TVectorD params(2);
	      lf->GetParameters(params);
	      avgC.Current[l] = params(0);
	      Point.Cfit = avgC.Current[l];
	    }
	  }
	  delete graphC;
	  TGraph *graphV = new TGraph(n,x.GetArray(),V.GetArray());
	  Cut3Rms(graphV);
	  mean = graphV->GetMean(2);
	  rms    = graphV->GetRMS(2);
	  avgC.Voltage[l] = mean;
	  Point.meanV = mean;
	  Point.rmsV  = rms;
	  if (rms/(n-1) > 0.5) { // 0.5 V ignore as ramping
	    failed++;
	  } else if (rms/(n-1) > 0.1) {  // 0.1V
	    iok1 = FitGraph(graphV,1);
	    if (iok1 > 0 || ! lf) {
	      failed++;
	    } else {
	      TVectorD params(2);
	      lf->GetParameters(params);
	      avgC.Voltage[l] = params(0);
	      Point.Vfit = avgC.Voltage[l];
	    }
	  }
	  Point.FitStatus = failed;
	  delete graphV;
	  if (failed) continue;
	  Double_t dC = 1e-6*avgC.Current[l]*(avgC.stop_time - avgC.start_time);
	  Point.Charge = dC;
	  if (io) Point.AcChargeI =  AcCharge[io];
	  else    Point.AcChargeO =  AcCharge[io];
	  FitP->Fill(&Point.run);
	  avgI.Charge[l] += dC;
	  AcCharge[io]   += dC;
	  CurrentsSum += avgC.Current[l];
	}
      }
    } // end of io loop
    if (CurrentsSum < 0.001) {
      if (IsBeenZeroSaved) continue;
      IsBeenZeroSaved = kTRUE;
    } else {IsBeenZeroSaved = kFALSE;}
    St_TpcAvgPowerSupply *TpcAvgPowerSupply = new St_TpcAvgPowerSupply("TpcAvgPowerSupply",1);
    TpcAvgPowerSupply->AddAt(&avgC);// TpcAvgPowerSupply->Print(0,1);
    cout << "Run " << avgC.run << " Acummulated charge Inner = " << AcCharge[0] << " (C), Outer = " << AcCharge[1] << "(C)" << endl;
    if (uTrip > 0) {
      avgC.start_time = uBegin;
      avgC.stop_time  = uTrip;
      avgT.start_time = uTrip;
      avgT.stop_time  = uStop;
    }
    if (_debug > 0) {
      for (Int_t sec = 1; sec <= 24; sec++) {
	cout << "Voltage " << sec;
	for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.3f",avgC.Voltage[8*(sec-1)+socket-1]);
	cout << endl;
      }
      for (Int_t sec = 1; sec <= 24; sec++) {
	cout << "Current " << sec;
	for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.5f",avgC.Current[8*(sec-1)+socket-1]);
	cout << endl;
      }
      for (Int_t sec = 1; sec <= 24; sec++) {
	cout << "Charge " << sec;
	for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.3f",avgC.Charge[8*(sec-1)+socket-1]);
	cout << endl;
      }
      if (_debug > 1) {
	TpcAvgPowerSupply->Print(0,24);
      }
    }
    if (! runs[r]) continue;
    TString fOut =  Form("TpcAvgPowerSupply.%8i.%06i.root",runs[r]->start.GetDate(),runs[r]->start.GetTime());
    TFile *outf = new TFile(fOut.Data(),"recreate");
    TpcAvgPowerSupply->Write();
    delete outf;
    delete TpcAvgPowerSupply;
    cout << fOut.Data() << " has been written" << endl;
    if (uTrip > 0 && uLastNonTrip > 0) {
      assert(uTrip > uLastNonTrip);
      TpcAvgPowerSupply = new St_TpcAvgPowerSupply("TpcAvgPowerSupply",1);
      TpcAvgPowerSupply->AddAt(&avgT);// TpcAvgPowerSupply->Print(0,1);
      //      TDatime uT; uT.Set(uTrip);
      TDatime uT; uT.Set(uLastNonTrip);
      fOut =  Form("TpcAvgPowerSupply.%8i.%06i.root",uT.GetDate(),uT.GetTime());
      outf = new TFile(fOut.Data(),"recreate");
      TpcAvgPowerSupply->Write();
      delete outf;
      delete TpcAvgPowerSupply;
      cout << "Tripped " << fOut.Data() << " has been written" << endl;
    }
    FitP->AutoSave("SaveSelf");
    if (! gROOT->IsBatch() && Ask()) return;
  }
  delete daq;
  for (Int_t r = 0; r <= NoGoodRuns; r++) {
    SafeDelete(runs[r]);
  }
  delete [] runs;
  fSumF->Write();
}
