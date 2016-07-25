/* 
   root.exe lmysql.C MakeTpcAvgCurrent.C+
 */
#include <stdio.h>
#include "Riostream.h"
#include "TSystem.h"
#include "tables/St_TpcAvgCurrent_Table.h"
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
#include "TROOT.h"
#include "TF1.h"
static TLinearFitter *lf = 0;
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
//________________________________________________________________________________
Int_t sector(Int_t lecroy = 0, Int_t channel = 0) {
  return 1 + 2*lecroy + channel/4;
}
//________________________________________________________________________________
Int_t ParseString (const Char_t *string, TObjArray &Opt) {
  Opt.Clear();
  TString tChain(string);
  TObjArray *obj = tChain.Tokenize("[^ ;,]+");
  Opt = *obj;
  Int_t nParsed = Opt.GetEntries();
#if 0
  for (Int_t k = 0; k < nParsed; k++) {
    if (Opt.At(k)) {
      cout << k << "\t" << ((TObjString *) Opt.At(k))->GetName() << endl;
    }
  }
#endif
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
Int_t FitGraph(TGraph *graph = 0) {
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
  Double_t xav = 0;
  for (Int_t l = 0; l < n; l++) {
    xav += x[l];
  }
  xav /= n;
  for (Int_t l = 0; l < n; l++) {
    x[l] -= xav;
  }
  
  TArrayD Y(n,graph->GetY()); Double_t *y = Y.GetArray();
  TArrayD E(n); Double_t *e = E.GetArray();
  
  for (Int_t i = 0; i < n; i++) e[i] = 0.01;
  lf->AssignData(n, 1, x, y, e);
  //Perform the fitting and look at the results
  Double_t h = 0.80;
  TBits bits(n);
  if (n < 4) lf->Eval();
  else      {lf->EvalRobust(h);   lf->GetFitSample(bits);}
  TVectorD params(2);
  TVectorD errors(2);
  lf->GetParameters(params);
  //  lf->GetErrors(errors);
  for (Int_t i=0; i<2; i++)
    cout << Form("par[%d]=%f ", i, params(i));
  Double_t chisquare=lf->GetChisquare();
  cout << Form("chisquare=%f\n", chisquare) << endl;
  
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
    xav     += x[l  ];
    yav     += y[l];
#if 0
    cout << Form("%2i x %8.3f y %8.3f +/- %8.2f res = %8.0f dev = %7.2f Bit: %1i",
		 l,x[l],y[l],e[l],1e4*res,dev,bits.TestBitNumber(l)) << endl;
#endif
    if (n >= 4 && ! bits.TestBitNumber(l) && TMath::Abs(dev) > 5)  continue;
    x[j  ] = x[l];
    y[j]   = y[l];
    e[j]   = e[l];
    if (e[j] < 1e-7) e[j] = 0.01;
    j++;
  }
  if (j >= 3) {
    xav /= n;
    yav /= n;
    lf->ClearPoints();
    lf->AssignData(j, 1, x, y, e);
    lf->Eval();
    lf->GetParameters(params);
    lf->GetErrors(errors);
    for (Int_t i=0; i<2; i++)   cout << Form("par[%d]=%f+-%f\t", i, params(i), errors(i));
    chisquare=lf->GetChisquare();
    cout << Form("chisquare=%f\n", chisquare) << endl;
    TString line(graph->GetTitle());
  }
  if (! gROOT->IsBatch() && Ask()) return 1;
  return iok;
}
//________________________________________________________________________________
Int_t readI(const Char_t *string, Int_t *array) {
  TObjArray Opt;
  Int_t N = ParseString(string,Opt);
  for (Int_t k = 0; k < N; k++) {
    array[k] = TString(((TObjString *) Opt.At(k))->GetName()).Atoi();
  }
  return N;
}
#include "OnDb.h"
//________________________________________________________________________________
TSQLServer *OnlDbServer(const Char_t *dataset = "Conditions_rich", Int_t year = 2013) {
  // http://www.star.bnl.gov/protected/common/common2012/trigger2012/sampleEfficiencypp500GeV/getSampleFrac.pl
  TString database = OnDb(year,dataset);
#if 0
  if      (year == 2014)     {
    if (dataset[0] == 'R') database = "onldb2.starp.bnl.gov:3501";
    else                   database = "onldb2.starp.bnl.gov:3502";
  }
  else if (year == 2013)     database = "dbbak.starp.bnl.gov:341";
  else if (year == 2012)     database = "dbbak.starp.bnl.gov:3411";
  else if (year == 2011)     database = "dbbak.starp.bnl.gov:3410";
  else if (year == 2010)     database = "dbbak.starp.bnl.gov:3409";
  else if (year == 2009)     database = "dbbak.starp.bnl.gov:3408";
  else if (year == 2008)     database = "dbbak.starp.bnl.gov:3407";
  else if (year == 2007)     database = "dbbak.starp.bnl.gov:3406";
  else if (year == 2006)     database = "dbbak.starp.bnl.gov:3405";
  else if (year == 2005)     database = "dbbak.starp.bnl.gov:3404";
  else if (year == 2004)     database = "dbbak.starp.bnl.gov:3403";
  else if (year == 2003)     database = "dbbak.starp.bnl.gov:3402";
  else if (year == 2002)     database = "dbbak.starp.bnl.gov:3401";
  else if (year == 2001)     database = "dbbak.starp.bnl.gov:3400";
#endif
  TString dbT("mysql://"); dbT += database; dbT += "/"; dbT += dataset; //Conditions_rich";
  TSQLServer *db = TSQLServer::Connect(dbT.Data(),"", ""); if (! db) {cout << "Can't connect " << dbT.Data() << endl; return db;}
#if 0
   // list tables in database "Conditions_daq" (the permission tables)
  cout << "List all tables in database " << dataset << " on server " << db->GetHost() << endl;
  TSQLRow *row;
  TSQLResult *res;
  res = db->GetTables(dataset);
  while ((row = res->Next())) {
    cout << row->GetField(0) << endl;
    //    delete row;
  }
  delete res;
#endif
  return db;
}
//________________________________________________________________________________
void MakeTpcAvgCurrent(Int_t year = 2015) {
  // List of runs
  //select beginTime,runNumber,from_unixtime(startRunRTS),from_unixtime(startRunDaq),from_unixtime(startRunTrg),from_unixtime(endRunRTS) from  runUpdateStatus limit 20;
  TSQLServer *RunLog = OnlDbServer("RunLog",year);
  //  TString sql("SELECT runNumber,from_unixtime(startRunRTS),from_unixtime(endRunRTS) from runUpdateStatus where beginTime > \"2012-04-23 10:09:15\" order by beginTime limit 2;");
  TString sql("SELECT runNumber,from_unixtime(startRunRTS),from_unixtime(endRunRTS) from runUpdateStatus where beginTime > \"2013-02-05\" order by beginTime;");
  TSQLResult *res = RunLog->Query(sql.Data());
  Int_t Nruns = res->GetRowCount(); cout << "Got " << Nruns << " rows in result" << endl;
  Run_t **runs = new Run_t*[Nruns+1];
  Int_t nfields = res->GetFieldCount();
  TString names[nfields];
  TSQLRow *row;
  Bool_t IsBeenZeroSaved = kFALSE;
  TGraph *graphs[192]; memset(graphs, 0, sizeof(graphs));
  Int_t   nentries[192]; memset(nentries, 0, 192*sizeof(Int_t));
  Double_t AcCharge[2] = {0, 0};
  for (Int_t i = 0; i < nfields; i++) {
    names[i] = res->GetFieldName(i);
    cout << Form("%20s", names[i].Data());
  }
  cout << endl;
  for (Int_t i = 0; i < Nruns; i++) {
    row = res->Next();
#if 0    
    for (Int_t j = 0; j < nfields; j++) {
      cout << Form("%i %20s = %s", j, names[j].Data(), row->GetField(j)) << endl;
    }
#endif
    runs[i] = new Run_t(TString(row->GetField(0)).Atoi(),row->GetField(1), row->GetField(2));
    //    runs[i]->stop.Set(row->GetField(2));
    runs[i]->Print();
  }
  runs[Nruns] = new Run_t(*runs[Nruns-1]);
  runs[Nruns]->start = runs[Nruns]->stop;
  runs[Nruns]->Print();
  delete RunLog;
  //  return;
  TSQLServer *daq    = OnlDbServer("Conditions_daq",year);
  const Char_t *IO[2] = {"Inner","Outer"};
  TpcAvgCurrent_st avgI; memset(&avgI, 0, sizeof(TpcAvgCurrent_st));
  for (Int_t r = 0; r < Nruns; r++) {
    Int_t ub = runs[r]->start.Convert();
    Int_t us = runs[r]->stop.Convert();
    Int_t ue = runs[r+1]->start.Convert();
    TpcAvgCurrent_st avgC; memset(&avgC, 0, sizeof(TpcAvgCurrent_st));
    Float_t VoltagesI[192]; memset(VoltagesI, 0, 192*sizeof(Float_t));
    avgC.run = runs[r]->run;
#if 0
    if (avgC.run < 16166017) continue;
#endif
    avgC.start_time = ub;
    avgC.stop_time  = ue;
    TCL::ucopy(avgI.AcCharge,avgC.AcCharge,192); 
    Int_t NIO[2] = {0,0};
    Double_t CurrentsSum = 0;
    for (Int_t l = 0; l < 192; l++) {SafeDelete(graphs[l]); memset(nentries, 0, 192*sizeof(Int_t));}
    for (Int_t io = 0; io < 2; io++) {
      //      TString Sql(Form("select UNIX_TIMESTAMP(beginTime),Voltages,Currents,Status,Reason from Conditions_daq.tpcPowerSupply%s ",IO[io]));
      TString Sql(Form("select beginTime,Currents,Reason,Voltages from Conditions_daq.tpcPowerSupply%s",IO[io]));
      Sql += " WHERE beginTime >= \"";
      Sql += runs[r]->start.AsSQLString();
      Sql += " \" and beginTime < \"";
      //      Sql += runs[r]->stop.AsSQLString();
      Sql += runs[r+1]->start.AsSQLString();
      Sql += "\"  order by beginTime;";
      cout << Sql.Data() << endl;
      res = daq->Query(Sql.Data());
      nfields = res->GetFieldCount();
      for (Int_t i = 0; i < nfields; i++) names[i] = res->GetFieldName(i);
      
      Int_t nrows = res->GetRowCount();
      cout << "Got rows " << nrows << " in result" << endl;
      if (! nrows) continue;
      Int_t Reason; // time, Status[96], 
      Float_t Currents[96], Voltages[96];
      Int_t u0 = ub;
      for (Int_t i = 0; i < nrows; i++) {
	row = res->Next(); 
#if 0
	for (int j = 0; j < nfields; j++) {
	  cout << Form("%20s = %s\n", names[j].Data(), row->GetField(j));
	}
	cout << endl;
	cout << "[0]" << row->GetField(0) << endl;
#endif
	TDatime t(row->GetField(0));//  t.Print(); 
	Int_t u = t.Convert();
	Int_t N;
	N = readF(row->GetField(1),  Currents); assert(N == 96);
	N = readF(row->GetField(3),  Voltages); assert(N == 96);
#if 0
	for (Int_t k = 0; k < 96; k++) cout << Currents[k] << ",";
	cout << endl;
#endif
	Reason = TString(row->GetField(2)).Atoi();
#if 0
	cout << "Reason " << Reason << endl;
#endif
	//	delete row;
	if (Reason != 1) continue;
	if (u <= us) NIO[io]++;
	for (Int_t k = 0; k < 96; k++) {
	  if (Currents[k] <= 0.0) continue;
	  if (Voltages[k] < 500)  continue;
	  Int_t channel = k%8;
	  Int_t module  = k/8;
	  Int_t sec  = sector(module,channel);
	  Int_t socket  = channel%4 + 4*io + 1;
	  Int_t l    = 8*(sec-1)+socket-1;
	  avgI.AcCharge[l] += 1e-6*Currents[k]*(u-u0);
	  AcCharge[io] += 1e-6*Currents[k]*(u-u0);
	  if (u > us) continue;
#if 0	    
	  VoltagesI[l] += Voltages[k];
	  avgC.AvCurrent[l] += Currents[k];
#endif
	  if (! graphs[l]) graphs[l] = new TGraph(nrows); 
	  graphs[l]->SetPoint(nentries[l],u,Currents[k]);
	  nentries[l]++;
	  graphs[l]->Set(nentries[l]);
	}
	u0 = u;
	//	delete row;
      }
      if (NIO[io]) {
	for (Int_t k = 0; k < 96; k++) {
	  Int_t channel = k%8;
	  Int_t module  = k/8;
	  Int_t sec  = sector(module,channel);
	  Int_t socket  = channel%4 + 4*io + 1;
	  Int_t l    = 8*(sec-1)+socket-1;
	  avgC.AvCurrent[l] /= NIO[io];
	  if (nentries[l] > 2) {
	    cout << "sector = " << sec << " socket = " << socket << endl;
	    Int_t iok = FitGraph(graphs[l]);
	    if (iok > 0) return;
	    if (lf) {
	      TVectorD params(2);
	      lf->GetParameters(params);
	      avgC.AvCurrent[l] = params(0);
	    }
	  }
	  VoltagesI[l] /= NIO[io];
	  CurrentsSum += avgC.AvCurrent[l];
	}
      }
      delete res;
    }
    if (CurrentsSum < 0.001) {
      if (IsBeenZeroSaved) continue;
      IsBeenZeroSaved = kTRUE;
    } else {IsBeenZeroSaved = kFALSE;}
    St_TpcAvgCurrent *TpcAvgCurrent = new St_TpcAvgCurrent("TpcAvgCurrent",1);
    TpcAvgCurrent->AddAt(&avgC);// TpcAvgCurrent->Print(0,1);
    cout << "Acummulated charge Inner = " << AcCharge[0] << " (C), Outer = " << AcCharge[1] << "(C)" << endl;
#if 0
    for (Int_t sec = 1; sec <= 24; sec++) {
      cout << "Voltage " << sec;
      for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.3f",VoltagesI[8*(sec-1)+socket-1]);
      cout << endl;
    }
#endif
    for (Int_t sec = 1; sec <= 24; sec++) {
      cout << "Current " << sec;
      for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.5f",avgC.AvCurrent[8*(sec-1)+socket-1]);
      cout << endl;
    }
    for (Int_t sec = 1; sec <= 24; sec++) {
      cout << "Charge " << sec;
      for (Int_t socket = 1; socket <= 8; socket++) cout << "\t" << Form("%10.3f",avgC.AcCharge[8*(sec-1)+socket-1]);
      cout << endl;
    }
    TString fOut =  Form("TpcAvgCurrent.%8i.%06i.root",runs[r]->start.GetDate(),runs[r]->start.GetTime());
    TFile *outf = new TFile(fOut.Data(),"recreate");
    TpcAvgCurrent->Write();
    delete outf;
    delete TpcAvgCurrent;
    cout << fOut.Data() << " has been written" << endl;
    if (! gROOT->IsBatch() && Ask()) return;
  }
  delete daq;
  for (Int_t r = 0; r <= Nruns; r++) {
    delete runs[r];
  }
  delete [] runs;
}
