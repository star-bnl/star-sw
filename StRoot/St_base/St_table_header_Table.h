//*-- Author :    Valery Fine(fine@bnl.gov)   23/03/00
// $Id: St_table_header_Table.h,v 1.3 2000/03/27 02:19:45 fine Exp $
//
#ifndef STAF_St_table_header_Table
#define STAF_St_table_header_Table

#include "TTable.h"

#include "table_header.h"

extern "C" {
   void *ReAllocate(table_head_st *h, Int_t newsize);
}

class St_table_header : public TTable
{
protected:
  static TTableDescriptor *fgColDescriptors;
  virtual TTableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}
  virtual void SetDescriptorPointer(TTableDescriptor *list) const { fgColDescriptors = list;}
  St_table_header() : TTable("table_header",sizeof(table_head_st)) {SetType("table_head");}

public:
  St_table_header(const TTable *table);
  table_head_st *GetTable(Int_t i=0) const { return ((table_head_st *)GetArray())+i;}
  table_head_st &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }
  const table_head_st &operator[](Int_t i) const { assert(i>=0 && i < GetNRows()); 
						   return *((const table_head_st *)(GetTable(i))); }
  static TTable *Object(TTable *&table,const table_head_st *h);

  ClassDef(St_table_header,0) //C++ wrapper for <table_header> 
};

inline TTable *St_table_header::Object(TTable *&table,const table_head_st *header)
{
  // return a pointer to the C++ object by table header provided
  assert(header); 
  table = (TTable *)header->dsl_pointer;
  return table;
}
//______________________________________________________________________________
// $Log: St_table_header_Table.h,v $
// Revision 1.3  2000/03/27 02:19:45  fine
// bug fix
//
// Revision 1.2  2000/03/27 00:34:04  fine
// function ReAllocate and method TTable::Object have been move to St_table_head class
//
// Revision 1.1  2000/03/23 17:58:13  fine
// new class to hold STAF table header aside
// 
//______________________________________________________________________________
#endif
