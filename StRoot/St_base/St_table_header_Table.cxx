//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: St_table_header_Table.cxx,v 1.4 2000/03/30 05:20:52 fine Exp $
//
#include "St_table_header_Table.h"
/////////////////////////////////////////////////////////////////////////
//                                                                     //
//  Class St_table_header fcreate and ill the STAF "table header"      //
//  to call STAF module (pams) properly                                //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

#include "Stypes.h"

TTableDescriptor *St_table_header::fgColDescriptors = 0;
TableClassImp(St_table_header,table_head)

//______________________________________________________________________________
St_table_header::St_table_header(const TTable *table) 
                : TTable("table_header",1,sizeof(table_head_st))
{
   SetType("table_header");
   table_head_st header;
   *header.name = 0;
   *header.type = 0;
   strncat(header.name,table->GetName(),sizeof(header.name));  // table name
   strncat(header.type,table->GetType(),sizeof(header.type));  // table type
   header.maxlen = table->GetTableSize();  // # rows allocated 
   header.nok    = table->GetNRows();	   // # rows filled */
   header.rbytes = table->GetRowSize();    // # bytes per row 
   header.dsl_pointer = long(table);	   // back pointer to table  
   header.data_pointer = 0;	           // Unused 
   AddAt(&header,0);
}

//______________________________________________________________________________
void *ReAllocate(table_head_st *header, Int_t newsize)
{
  //
  // header - "plain" C interface to re-allocate the STAF table
  //          "this"  pointer is  supplied indirectly via
  //          header->dsl_pointer member
  //
  // newsize - is a new size of the STAF table.
  //           If it is smaller is the old one then nothing happens
  //
 if (header && newsize)
   return ((TTable *)header->dsl_pointer)->ReAllocate(newsize);
 else
   return 0;
}
 

//______________________________________________________________________________
// $Log: St_table_header_Table.cxx,v $
// Revision 1.4  2000/03/30 05:20:52  fine
// bug fixed
//
// Revision 1.3  2000/03/29 20:39:18  fine
// bug fix
//
// Revision 1.2  2000/03/27 00:34:04  fine
// function ReAllocate and method TTable::Object have been move to St_table_head class
//
// Revision 1.1  2000/03/23 17:58:13  fine
// new class to hold STAF table header aside
// 
//______________________________________________________________________________

