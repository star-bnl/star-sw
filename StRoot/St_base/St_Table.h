//*-- Author :    Valery Fine   24/03/98
// $Id: St_Table.h,v 1.41 1999/11/24 00:31:26 fine Exp $
// $Log: St_Table.h,v $
// Revision 1.41  1999/11/24 00:31:26  fine
// operator[] const has been introduced
//
// Revision 1.40  1999/10/28 16:24:35  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.39  1999/10/28 00:32:55  fine
// method At() has been removed
//
// Revision 1.38  1999/09/24 21:56:08  fisyak
// Add operator [] for particular table (VF)
//
// Revision 1.37  1999/09/07 19:30:29  fine
// table descriptor access has been changed. All tables are affected and must be re-compiled
//
// Revision 1.36  1999/09/04 00:28:02  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.35  1999/08/30 23:15:09  fine
// St_Table::Fit method has been introduced
//
// Revision 1.34  1999/08/20 13:22:25  fine
// new method St_Table::Draw
//.
// Revision 1.33  1999/08/12 16:41:31  fine
// Clean up
//
// Revision 1.32  1999/08/12 16:39:49  fine
// clash between St_Table::GetSize and TArray::GEtSize has been resolved
//
// Revision 1.31  1999/08/11 14:44:39  fine
// name clash with ROOT over enum resolved
//
// Revision 1.30  1999/08/11 00:42:33  fine
// new I/O via St_baseDescriptor table has been implemented
//
// Revision 1.29  1999/07/01 01:45:32  fisyak
// GetRowDescritors => GetRowDescriptors
//
// Revision 1.28  1999/06/26 01:40:56  fisyak
// Add Valery's abstract buffer
//
// Revision 1.27  1999/06/25 01:35:54  fine
// New streamers for St_Tables
//
// Revision 1.26  1999/02/24 17:10:58  fine
//  St_Table  New and Purge method have been introdiced, some clean up for St_module as well
//
// Revision 1.25  1999/01/28 19:13:08  fine
// St_TableSorter has been made up
//
// Revision 1.24  1999/01/19 03:13:32  fine
// St_DataSet::Fine and St_DataSet::FindObject methods have been introduced
//
// Revision 1.23  1999/01/13 20:29:15  fine
// St_DataSet::Pass() method - the option kUp has been introduced
//
// Revision 1.22  1998/12/30 22:30:18  fine
// St_Table::PrintHrader method has been introduced
//
// Revision 1.21  1998/12/30 01:08:02  fisyak
// Add Public SetNRows for used No. of rows
//
// Revision 1.20  1998/12/30 00:52:42  fisyak
// Remove SetfN from public
//
// Revision 1.18  1998/12/29 19:37:40  fine
// St_NodeView:  new class to create refs topology of the "main" St_Node object has been introduced
//
// Revision 1.17  1998/12/17 16:57:57  fine
// St_Table: some extra protections have been established (safe "strncat" have replaced the  unsafe  "strncpy")
//
// Revision 1.16  1998/12/07 20:23:12  fine
// St_Table:: working versions of the Print() and SavePrimitive methods
//
// Revision 1.15  1998/12/06 00:45:49  fisyak
// Add SavePrimitive
//
// Revision 1.14  1998/12/06 00:38:17  fisyak
// Add SavePrimitive
// 
#ifndef STAF_St_Table
#define STAF_St_Table
  
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  St_Table                                                            //
//                                                                      //
//  It is a base class to create a "wrapper" class                      //
//  holding the plain C-structure array                                 //
//  (1 element of the structure per element)                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifdef __CINT__
#pragma Ccomment on
#endif

#include "St_DataSet.h"
#include "TArray.h"
#include "table_header.h" 
#include "tableDescriptor.h" 
#include "TCut.h"


#ifndef __CINT__
#  include <string.h>
#include <fstream.h>
#include <assert.h>
#endif
enum  EBufSizes { kChar1Byte   =sizeof(Char_t) 
                 ,kShort2Bytes =sizeof(Short_t)
                 ,kLong4Bytes  =sizeof(Long_t)
                 ,kDouble8Bytes=sizeof(Double_t)
                } ; 

enum ETableBits {
    kIsNotOwn         = BIT(23)   // if the St_Table wrapper doesn't own the STAF table                                 // As result of the Update() method for example
};
class G__DataMemberInfo;
class St_XDFFile;
class St_tableDescriptor;
class TH1;


extern "C" {
   void *ReAllocate(table_head_st *h, Int_t newsize);
}


class St_Table : public St_DataSet, public TArray {
   friend class St_XDFFile;
   friend class St_DataSet;
private:
   Long_t         *s_Size;       // Size of the one element (row) of the table
   table_head_st  *s_TableHeader;// Pointer to the STAF table header data structure

protected:
   void       CopyStruct(Char_t *dest, const Char_t *src);
   Char_t    *Create();
   virtual     void       Clear(Option_t *opt="");  
   virtual     void       Delete(Option_t *opt="");
   virtual Bool_t  EntryLoop(const Char_t *exprFileName,Int_t &action, TObject *obj, Int_t nentries=1000000000, Int_t firstentry=0, Option_t *option="");
   void       LinkHeader();
   void       SetHeadFields(Text_t *name);
   Int_t      SetfN(Long_t len);
   void       SetName(const Char_t *name);
   void       SetTablePointer(void *table);
   void       SetUsedRows(Int_t n);
   void       SetTitle(const Char_t *title);
   void       SetType(const Text_t *const type);
   void       StreamerHeader(TBuffer &b);
   Int_t      StreamerHeader(StBufferAbc &b);
   int        PointerToPointer(G__DataMemberInfo &m);
   void       SetTableName(const Char_t *name);
   void       SetTableType(const Char_t *type);
   void       StreamerTable(TBuffer &b);
   Int_t      StreamerTable(StBufferAbc &b);
   virtual St_tableDescriptor *GetDescriptorPointer() const;
   virtual void  SetDescriptorPointer(St_tableDescriptor *list) const ;

   Long_t    *s_MaxIndex;   // The used capacity of this array

   void      ReAlloc(Int_t newsize);
 
public:

   enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                          ,kULong, kUShort, kUChar, kChar };

   Char_t    *s_Table;       // Array of (fN*s_Size) longs
 
   St_Table(Text_t *name=0, Int_t size=0);
   St_Table(Text_t *name, Int_t n,Int_t size);
   St_Table(Text_t *name, Int_t n, Char_t *array,Int_t size);
   St_Table(Text_t *name, Text_t *type, Int_t n, Char_t *array, Int_t size);
   St_Table(const St_Table &table);
   St_Table    &operator=(const St_Table &rhs);
   virtual    ~St_Table();
  
   virtual     void       Adopt(Int_t n, void *array);
   virtual     void       AddAt(const void *c, Int_t i);
              const void *At(Int_t i) const;
   virtual     void       Browse(TBrowser *b);
   virtual     void       CopySet(St_Table &array);
   virtual     void       Draw(Option_t *opt);
   virtual     TH1       *Draw(TCut varexp, TCut selection, Option_t *option=""
                         ,Int_t nentries=1000000000, Int_t firstentry=0);
   virtual     TH1       *Draw(const Text_t *varexp, const Text_t *selection, Option_t *option=""
                              ,Int_t nentries=1000000000, Int_t firstentry=0); // *MENU*
   virtual     void      *GetArray() const ;
   virtual     TClass    *GetRowClass() const ;
   virtual     Long_t     GetNRows() const;
   virtual     Long_t     GetRowSize() const;
   virtual     Long_t     GetTableSize() const;
   virtual     St_tableDescriptor *GetTableDescriptors() const;
   virtual     St_tableDescriptor *GetRowDescriptors() const;
   virtual     const Char_t *GetType() const;
   virtual     void       Fit(const Text_t *formula ,const Text_t *varexp, const Text_t *selection="",Option_t *option="" ,Option_t *goption=""
                              ,Int_t nentries=1000000000, Int_t firstentry=0); // *MENU*

   virtual     Long_t     HasData() const { return 1; }
   virtual     Bool_t     IsFolder();
   virtual     void       ls(Option_t *option="");
   virtual     void       ls(Int_t deep);
               Int_t      NaN();
   static      St_Table  *New(const Char_t *name, const Char_t *type, void *array, UInt_t size);
   virtual     Char_t    *MakeExpression(const Char_t *expressions[],Int_t nExpressions);
   virtual     Char_t    *Print(Char_t *buf,Int_t n) const ;
   virtual     void       Print(Option_t *opt="");
   virtual  const Char_t *Print(Int_t row, Int_t rownumber=10,
                                const Char_t *colfirst="", const Char_t *collast="") const; // *MENU*
   virtual  const Char_t *PrintHeader() const; // *MENU*
   virtual     void       Project(const Text_t *hname, const Text_t *varexp, const Text_t *selection="", Option_t *option=""
                                 ,Int_t nentries=1000000000, Int_t firstentry=0);

   virtual Int_t         Purge(Option_t *opt="");   

               void      *ReAllocate(Int_t newsize);
               void      *ReAllocate();
   virtual     table_head_st *GetHeader() const;
   void        MakeHeader(const Char_t *prefix,const Char_t *tablename,const Char_t *suffix, FILE *fl=0); // Create header file for STAF table class
   static Int_t MakeWrapClass(Text_t *name);
   virtual     Int_t      ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize);
   virtual     void       SavePrimitive(ofstream &out, Option_t *option);
   virtual     void       SetHeader(table_head_st *table);
//   ULong_t   &operator(){ return GetTable();}

   virtual     void       StafStreamer(Char_t *structname=0, FILE *fl=0);
   virtual     void       Set(Int_t n);
   virtual     void       Set(Int_t n, Char_t *array);
   virtual     void       SetNRows(Int_t n);
   virtual     Int_t      Streamer(StBufferAbc &R__b);
   virtual     void       Reset(Int_t c=0);
   virtual     void       Update();
   virtual     void       Update(St_DataSet *set,UInt_t opt=0);
               void      *operator[](Int_t i);
               const void *operator[](Int_t i) const;

 //  ----   Table descriptor service   ------

   virtual  const Char_t *GetColumnName(Int_t columnIndex)      const;
   virtual   UInt_t      *GetIndexArray(Int_t columnIndex)      const;
   virtual   UInt_t       GetNumberOfColumns()                  const;
   virtual   UInt_t       GetOffset(Int_t columnIndex)          const;
   virtual   Int_t        GetOffset(const Char_t *columnName=0) const;
   virtual   UInt_t       GetColumnSize(Int_t columnIndex)      const;
   virtual   Int_t        GetColumnSize(const Char_t *columnName=0) const;
   virtual   UInt_t       GetTypeSize(Int_t columnIndex)        const;
   virtual   Int_t        GetTypeSize(const Char_t *columnName=0) const ;
   virtual   UInt_t       GetDimensions(Int_t columnIndex)      const;
   virtual   Int_t        GetDimensions(const Char_t *columnName=0) const ;
   virtual   EColumnType  GetColumnType(Int_t columnIndex)      const;
   virtual   EColumnType  GetColumnType(const Char_t *columnName=0) const;

   ClassDef(St_Table,0)  // Array of the STAF structure
};
 
inline  table_head_st *St_Table::GetHeader()const { return s_TableHeader; }
inline  Bool_t St_Table::IsFolder(){ return fList && fList->Last() ? kTRUE : kFALSE;}
inline  void   St_Table::Print(Option_t *) { Print((Char_t *)0,Int_t(0)); }

inline  void   St_Table::SetUsedRows(Int_t n) {if (s_TableHeader) *s_MaxIndex = n;}
inline  void   St_Table::SetTitle(const Char_t *title){SetType(title);}
inline  void   St_Table::SetHeader(table_head_st *table){s_TableHeader = table;} 
inline  void   St_Table::SetNRows(Int_t n) {SetUsedRows(n);} 
//   ULong_t   &operator(){ return GetTable();}


inline void *St_Table::operator[](Int_t i)
{
   if (!BoundsOk("St_Table::operator[]", i))
      i = 0;
    return (void *)(s_Table+i*(*s_Size));
}

inline const void *St_Table::operator[](Int_t i) const
{
   if (!BoundsOk("St_Table::operator[]", i))
      i = 0;
    return (const void *)(s_Table+i*(*s_Size));
}

inline void St_Table::Draw(Option_t *opt)
{ Draw(opt, "", "", 1000000000, 0); }

#endif 

