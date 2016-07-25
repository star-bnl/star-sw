#include "TPRegexp.h"
#include "TDirectory.h"
#include "TKey.h"
#include "TObject.h"
#include "TList.h"
#include "Riostream.h"
using namespace std;
//________________________________________________________________________________
void SumHist(const Char_t *pattern = "zB$") {
  TPRegexp reg(pattern);
  //  gDirectory->ls();
  TList *keys = gDirectory->GetListOfKeys();
  TList *objs = gDirectory->GetList();
  if (! objs && ! keys) return;
  TIter next(keys);
  TObject *o;
  TH2F *sum = 0;
  TKey *key = 0;
  while ((key = (TKey *) next())) {
    TString Name(key->GetName());
    if (! Name.Contains(reg)) continue;
    if (  Name.Contains("alpha") || Name.Contains("He3")) continue;
    cout <<  key->GetName() << endl;
    o = objs->FindObject(key->GetName());
    if (! o) {
      o = key->ReadObj();
    }
    if (! o) continue;
    cout <<  o->GetName() << endl;
    if (! o->IsA()->InheritsFrom( "TH2F" ) ) continue;
    TH2F *h2 = (TH2F *) o;
    if (! sum) {
      sum = new TH2F(*h2);
      TString name("Sum"); name += pattern;
      name.ReplaceAll("$","");
      sum->SetName(name);
    } else {
      sum->Add(h2);
    }
    cout <<  o->GetName() << endl;
  }
}
