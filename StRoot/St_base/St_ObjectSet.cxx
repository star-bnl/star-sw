//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
#include "St_ObjectSet.h"
ClassImp(St_ObjectSet)
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(const Char_t *name, TObject *obj):St_DataSet(name),m_Obj(obj){;}
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(TObject *obj) : St_DataSet(obj?obj->GetName():"unknown",obj?obj->GetTitle():"St_ObjectSet")
              ,m_Obj(obj){;}

//_____________________________________________________________________________
St_ObjectSet::~St_ObjectSet(){;}

//_____________________________________________________________________________
TObject *St_ObjectSet::GetObject() { return m_Obj;}
