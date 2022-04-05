#ifndef St_tpcSCGLC_h
#define St_tpcSCGLC_h
#include "TChair.h"
#include "tables/St_tpcSCGL_Table.h"


class St_tpcSCGLC : public TChair {
 public:
  static St_tpcSCGLC* 	instance();

  tpcSCGL_st *Struct(Int_t i = 0)         {return ((St_tpcSCGL*) Table())->GetTable()+i;}
  float* SC()                             {return Struct()->SC;}
  float* SCoffset()                       {return Struct()->SCoffset;}
  float* SCexponent()                     {return Struct()->SCexponent;}
  float* SCscaler()                       {return Struct()->SCscaler;}
  float* GL()                             {return Struct()->GL;}
  float* GLoffset()                       {return Struct()->GLoffset;}
  float  GLradius()                       {return Struct()->GLradius;}
  float  GLwidth()                        {return Struct()->GLwidth;}
  int    mode()                           {return Struct()->mode;}
  char*  comment()                        {return Struct()->comment;}

 protected:
  St_tpcSCGLC(St_tpcSCGL *table=0) : TChair(table) {}
  virtual ~St_tpcSCGLC() {fgInstance = 0;}
 private:
  static St_tpcSCGLC* fgInstance;
  ClassDef(St_tpcSCGLC,1)
};
#endif
