//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
#ifndef STAR_St_ObjectSet
#define STAR_St_ObjectSet

#include "St_DataSet.h"

class St_ObjectSet : public St_DataSet {
protected:
  TObject *fObj;  // TObject to be inserted
public:
  St_ObjectSet(const Char_t *name, TObject *obj=0);
  St_ObjectSet(TObject *obj=0);
  virtual ~St_ObjectSet();
  virtual void     Browse(TBrowser *b);
  virtual TObject *GetObject() const {return fObj;};
  virtual void     SetObject(TObject *obj){ fObj = obj;}
  virtual void     AddObject(TObject *obj){ SetObject(obj);}
  virtual Long_t   HasData() const {return fObj ? 1 : 0;} 
  ClassDef(St_ObjectSet,1)
};

#endif

