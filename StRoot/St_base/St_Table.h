//*-- Author :    Valery Fine   24/03/98
// $Id: St_Table.h,v 1.15 1998/12/06 00:45:49 fisyak Exp $
// $Log: St_Table.h,v $
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


#ifndef __CINT__
#  include <string.h>
#endif
#include <fstream.h>
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
   void       Clear(Option_t *opt="");  
   void       Delete(Option_t *opt="");
   void       LinkHeader();
   void       SetHeadFields(Text_t *name);
   Int_t      SetfN(Long_t len);
   void       SetName(const Char_t *name);
   void       SetTablePointer(void *table);
   void       SetUsedRows(Int_t n);
   void       SetTitle(const Char_t *title);
   void       SetType(const Text_t *const type);
   void       StreamerHeader(TBuffer &b);
   int        PointerToPointer(G__DataMemberInfo &m);
   Long_t    *s_MaxIndex;   // The used capacity of this array
 
public:

   Char_t    *s_Table;       // Array of (fN*s_Size) longs
 
   St_Table(Text_t *name=0, Int_t size=0);
   St_Table(Text_t *name, Int_t n,Int_t size);
   St_Table(Text_t *name, Int_t n, Char_t *array,Int_t size);
   St_Table(Text_t *name, Text_t *type, Int_t n, Char_t *array, Int_t size);
   St_Table(const St_Table &table);
   St_Table    &operator=(const St_Table &rhs);
   virtual    ~St_Table();
  
   virtual     void       Adopt(Int_t n, void *array);
   virtual     void       AddAt(ULong_t *c, Int_t i);
   virtual     void      *At(Int_t i);
   virtual     void       Browse(TBrowser *b);
   virtual     void       CopySet(St_Table &array);
   virtual     void      *GetArray() const ;
   virtual     TClass    *GetRowClass() const ;
   virtual     Long_t     GetNRows() const;
   virtual     Long_t     GetRowSize() const;
   virtual     Long_t     GetTableSize() const;
   virtual     const Char_t *GetType() const;
   virtual     Long_t     HasData() const { return 1; }
   virtual     Bool_t     IsFolder();
   virtual     void       ls(Option_t *option="");
   virtual     void       ls(Int_t deep);
   virtual     Char_t    *Print(Char_t *buf,Int_t n) const ;
   virtual     void       Print(Option_t *buf="") { Print((Char_t *)0,Int_t(0)); }
   virtual  const Char_t *Print(Int_t row, Int_t rownumber=10, const Char_t *colfirst="", const Char_t *collast="") const; // *MENU*
               void      *ReAllocate(Int_t newsize);
   virtual     table_head_st *GetHeader() const;
   void        MakeHeader(const Char_t *prefix,const Char_t *tablename,const Char_t *suffix, FILE *fl=0); // Create header file for STAF table class
   static Int_t MakeWrapClass(Text_t *name);
   virtual     Int_t      ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize);
   virtual     void       SavePrimitive(ofstream &out, Option_t *option);
   virtual     void       SetHeader(table_head_st *table);
//   ULong_t   &operator(){ return GetTable();}
   virtual     void       SetTableName(Char_t *name);
   virtual     void       SetTableType(Char_t *type);

   virtual     void       StafStreamer(Char_t *structname=0, FILE *fl=0);
   virtual     void       Set(Int_t n);
   virtual     void       Set(Int_t n, Char_t *array);
   virtual     void       Reset(Int_t c=0);
   virtual     void       Update();
   virtual     void       Update(St_DataSet *set,UInt_t opt=0);
   virtual     void      *operator[](Int_t i);
 
   ClassDef(St_Table,0)  // Array of the STAF structure
};
 
inline  table_head_st *St_Table::GetHeader()const { return s_TableHeader; }
inline  Bool_t St_Table::IsFolder(){ return fList && fList->Last() ? kTRUE : kFALSE;}
inline  void   St_Table::SetUsedRows(Int_t n) {if (s_TableHeader) *s_MaxIndex = n;}
inline  void   St_Table::SetTitle(const Char_t *title){SetType(title);}
inline  void   St_Table::SetHeader(table_head_st *table){s_TableHeader = table;} 
//   ULong_t   &operator(){ return GetTable();}
inline  void   St_Table::SetTableName(Char_t *name){ if (s_TableHeader) strncpy(s_TableHeader->name,name,20); }
inline  void   St_Table::SetTableType(Char_t *type){ if (s_TableHeader) strncpy(s_TableHeader->type,type,20); }

inline void *St_Table::At(Int_t i)
{
   if (!BoundsOk("St_Table::At", i))
      i = 0;
   return (void *)(s_Table+i*(*s_Size));
}
#if 1
inline void *St_Table::operator[](Int_t i)
{
   if (!BoundsOk("St_Table::operator[]", i))
      i = 0;
    return (void *)(s_Table+i*(*s_Size));
}
#endif
#endif 

