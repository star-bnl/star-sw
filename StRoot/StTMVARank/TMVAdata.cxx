#include "Riostream.h"
#include "PVgadgets.h"
TableImpl(PVgadgets);
#include "TMVAdata.h"
ClassImp(TMVAdata);
using namespace std;
TMVAdata*     TMVAdata::fgInstance = 0;
//________________________________________________________________________________
void TMVAdata::Init() {
  TTableDescriptor *ds = GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    fAcceptMap[aName] = kFALSE;
  }
}
//________________________________________________________________________________
void TMVAdata::SetListOfActiveVariables(const Char_t *list) {
  cout << "SetListOfActiveVariables(" << list << ")" << endl;
  TPMERegexp reg(":");
  TString ListOfActiveVariables(list);
  Int_t N = reg.Split(ListOfActiveVariables);
  for (Int_t i = 0; i < N; i++) {
    SetAcceptVar(reg[i]);
    }
  Print();
}
//________________________________________________________________________________
void TMVAdata:: Print() {
  TTableDescriptor *ds = GetTableDesc();
  tableDescriptor_st *s = ds->GetTable();
  for (Int_t i = 0; i < ds->GetNRows(); i++, s++) {
    TString aName(s->fColumnName);
    std::cout << "TMVAdata::AcceptVar\t" << aName.Data() << " = " << fAcceptMap[aName] << std::endl;
  }    
}
