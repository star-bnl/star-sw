//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
#include "St_ObjectSet.h"
#include "TBrowser.h"
ClassImp(St_ObjectSet)
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(const Char_t *name, TObject *obj):St_DataSet(name),fObj(obj)
{SetTitle("St_ObjectSet");}
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(TObject *obj) : St_DataSet("unknown","St_ObjectSet")
              ,fObj(obj){;}

//_____________________________________________________________________________
St_ObjectSet::~St_ObjectSet(){delete fObj;}

//______________________________________________________________________________
void St_ObjectSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
   if (b && fObj) b->Add(fObj);
  St_DataSet::Browse(b);
}
