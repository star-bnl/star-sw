//*-- Author :    Valery Fine(fine@bnl.gov)   23/03/00
// $Id: St_table_header_Table.h,v 1.5 2000/05/15 18:54:48 fine Exp $
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
 public:
    St_table_header(const TTable *table);
    static TTable *Object(TTable *&table,const table_head_st *h);
    ClassDefTable(St_table_header,table_head_st)
    ClassDef(St_table_header,0) //C++ wrapper for <table_header> 
};

//______________________________________________________________________________
inline TTable *St_table_header::Object(TTable *&table,const table_head_st *header)
{
  // return a pointer to the C++ object by table header provided
  assert(header); 
  table = (TTable *)header->dsl_pointer;
  return table;
}
//______________________________________________________________________________
// $Log: St_table_header_Table.h,v $
// Revision 1.5  2000/05/15 18:54:48  fine
// ClassDefTable macro in use here
//
// Revision 1.4  2000/03/30 05:20:52  fine
// bug fixed
//
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
