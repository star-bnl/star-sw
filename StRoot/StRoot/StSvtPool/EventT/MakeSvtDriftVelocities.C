//#define DEBUG
#include "Riostream.h"
class St_db_Maker;
class TTable;
St_db_Maker *dbMk = 0;
TTable *table = 0;
#if 0
// lines which should be added to Drift_ALL.h
Int_t Drift = 1;
Int_t time =       207; 
Int_t date =  20070321;
const Char_t *Pass = "Pass217 FFK"; 
#endif 
#include "Drift_ALL.h"
static const Int_t N = sizeof(Data)/sizeof(data_t);


//________________________________________________________________________________
void MakeSvtDriftVelocities(){
  cout << "Date " << date << "\t Time " << time << endl;
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,db,nodefault");
  StMaker *dbMk = chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  StEvtHddr *header = chain->GetEvtHddr();
  header->SetRunNumber(1);
  dbMk->SetDateTime(date,time); 
  header->SetDateTime(date,time);
  chain->MakeEvent();
  St_svtHybridDriftVelocity *svtHybridDriftVelocity = (St_svtHybridDriftVelocity *) dbMk->GetDataBase("Calibrations/svt/svtHybridDriftVelocity"); 
  if (! (svtHybridDriftVelocity)) return;
  Int_t NN = svtHybridDriftVelocity->GetNRows();
#ifdef DEBUG
  cout << "NN row from svtHybridDriftVelocity " << NN << " N corrections " << N << endl;
#endif
  svtHybridDriftVelocity_st *row = svtHybridDriftVelocity->GetTable();
  Double_t drift[10];
  Double_t anode[10];
  for (Int_t i = 0; i < NN; i++) {
    for (Int_t j = 0; j < N; j++) {
      if (Data[j].barrel == row[i].barrel && 
	  Data[j].ladder == row[i].ladder &&
	  Data[j].wafer  == row[i].wafer  &&
	  Data[j].hybrid == row[i].hybrid) {
	  Double_t *par = &row[i].v0;
	  Double_t *cor = &Data[j].v0;
	  Int_t nu = row[i].npar%10;
	  Int_t nv = (row[i].npar/10)%10;
	  Int_t I  = (row[i].npar/100)%10;
#ifdef DEBUG
	cout << "Found Match" << endl;
	svtHybridDriftVelocity->Print(i,1);
	cout << Data[j].type    << ",\t" << Data[j].idx   << ",\t" << Data[j].nrows<< ",\t" 
	     << Data[j].barrel  << ",\t" << Data[j].layer << ",\t" << Data[j].ladder<< ",\t" 
	     << Data[j].wafer   << ",\t" << Data[j].hybrid<< ",\t" 
	     << Data[j].Npar    << ",\t";
	cout << "\tnu = " << nu << "\tnv = " << nv << "\tNpar = " << Data[j].Npar;
	for (Int_t l = 0; l <= Data[j].Npar; l++) cout << ",\t" << cor[l];
	cout << "\t" << Data[j].Comment << endl;
#endif 
	if (row[i].type == 0 && row[i].npar == 0) {
	  row[i].type = 2; row[i].v0 = 0;
	}
	if (row[i].type == 2) {
// 	  svtHybridDriftVelocity->Print(i,1);
	  row[i].type = 2;
	  //	  cout << "nu = " << nu << " nv = " << nv << endl;
	  memset(drift, 0, 10*sizeof(Double_t));
	  memset(anode, 0, 10*sizeof(Double_t));
	  if (nu) memcpy (drift, &par[0], nu*sizeof(Double_t));
	  if (nv) memcpy (anode, &par[nu], nv*sizeof(Double_t));
	  if (Drift) {
	    for (Int_t k = 0; k <= Data[j].Npar; k++) {
	      drift[k] += cor[k];
	    }
	    if (nu <=  Data[j].Npar) nu = Data[j].Npar+1;
	  } else {
	    for (Int_t k = 0; k <= Data[j].Npar; k++) {
	      anode[k] += cor[k];
	    }
	    if (nv <=  Data[j].Npar) nv = Data[j].Npar+1;
	    
	  }
#ifdef DEBUG
	  for (Int_t k = 0; k < nu+nv; k++) {
	    if (k < nu) cout << "\td:" << par[k] << "\t" << drift[k];
	    else        cout << "\ta:" << par[k] << "\t" << anode[k-nu];
	  }
	  cout << endl;
#endif
	  row[i].npar = 100*I + 10*nv + nu;
	  memset(par, 0, 10*sizeof(Double_t));
	  for (Int_t k = 0; k < nu + nv; k++) {
	    if (k >= 10) break;
	    if (k < nu) par[k] = drift[k];
	    else        par[k] = anode[k-nu];
	  }
	}
#ifdef DEBUG
 	svtHybridDriftVelocity->Print(i,1);
#endif
      }
    }
  }
  // Merge Drift velocities
  Char_t Out[132];
  sprintf(Out,"%s.%8i.%06i.C",svtHybridDriftVelocity->GetName(),date,time);
  ofstream out;
  out.open(Out);
  cout << "Create " << Out << endl;
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_svtHybridDriftVelocity\")) return 0;" << endl;
  out << "  svtHybridDriftVelocity_st row[" << NN << "] = {//" << Pass << endl; 
  for (Int_t i = 0; i < NN; i++) {
    out << Form("{%2i,%1i,%4i,%4i,%3i,%7i",row[i].type,row[i].status,row[i].idx,row[i].nrows,row[i].npar,row[i].Id);
    out << Form(",%1i,%2i,%1i,%1i",row[i].barrel,row[i].ladder,row[i].wafer,row[i].hybrid);
    out << Form(",%6.3f,%5.3f,%7.3f,%6.3f",row[i].tmin,row[i].dtmin,row[i].tmax,row[i].dtmax);
    Double_t *v = &row[i].v0;
    for (Int_t j = 0; j < 10; j++) {
      if (v[j]) out << Form(",%8.5f",v[j]);
      else      out << ", 0.00000";
    }
    if (i < NN - 1)  out << "},";
    else             out << "}";
    out << Form("// B%iL%02iW%iH%i",row[i].barrel,row[i].ladder,row[i].wafer,row[i].hybrid) << endl;
  } 
  out << "  };" << endl;
  out << "  St_svtHybridDriftVelocity *tableSet = new St_svtHybridDriftVelocity(\"" << svtHybridDriftVelocity->GetName() << "\"," << NN << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NN << "; i++) tableSet->AddAt(&row[i].type, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
