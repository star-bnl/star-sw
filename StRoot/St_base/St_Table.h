//*-- Author :    Valery Fine   24/03/98
 
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
 
#include "TNamed.h"
#include "TArray.h"
#include "table_header.h" 


#ifndef __CINT__
#  include <string.h>
#endif
 
enum  EBufSizes { kChar1Byte   =sizeof(Char_t) 
                 ,kShort2Bytes =sizeof(Short_t)
                 ,kLong4Bytes  =sizeof(Long_t)
                 ,kDouble8Bytes=sizeof(Double_t)
                } ; 

class G__DataMemberInfo;
class St_XDFFile;

class St_Table : public TNamed, public TArray {
   friend class St_XDFFile;
   friend class St_DataSet;
private:
   Long_t         *s_Size;       // Size of the one element (row) of the table
   table_head_st  *s_TableHeader;// Pointer to the STAF table header data structure

protected:
   void       Copy(Char_t *dest, Char_t *src);
   Char_t    *Create();
   void       Delete();
   void       LinkHeader();
   void       SetHeadFields(Text_t *name);
   Int_t      SetfN(Long_t len);
   void       SetName(const Text_t *const name);
   void       SetTablePointer(void *table);
   void       SetUsedRows(Int_t n){if (s_TableHeader) *s_MaxIndex = n;}
   void       SetTitle(const Text_t *const title){SetType(title);}
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
 
   virtual     void       Adopt(Int_t n, Char_t *array);
   virtual     void       AddAt(ULong_t *c, Int_t i);
   virtual     void      *At(Int_t i);
   virtual     void       Browse(TBrowser *b);
   virtual     void       Copy(St_Table &array);
   virtual     Long_t     GetNRows(){ return *s_MaxIndex;}
   virtual     Long_t     GetRowSize(){ return *s_Size;}
   virtual     Long_t     GetTableSize(){ return fN;}
   virtual     void      *GetArray() const { return s_Table; }
   virtual     const Char_t *GetType() const { return GetTitle();}
   virtual     void       ls(Option_t *option="");
   virtual     Char_t    *Print(Char_t *buf="",Int_t n=0);
   virtual     table_head_st *GetHeader(){ return s_TableHeader; }
   void        MakeHeader(const Char_t *prefix,const Char_t *tablename,const Char_t *suffix, FILE *fl=0); // Create header file for STAF table class
   static Int_t MakeWrapClass(Text_t *name);
   virtual     Int_t      ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize);
   virtual     void       SetHeader(table_head_st *table){s_TableHeader = table;} 
//   ULong_t   &operator(){ return GetTable();}
   virtual     void       SetTableName(Char_t *name){ if (s_TableHeader) strncpy(s_TableHeader->name,name,20); }
   virtual     void       SetTableType(Char_t *type){ if (s_TableHeader) strncpy(s_TableHeader->type,type,20); }

   virtual     void       StafStreamer(Char_t *structname=0, FILE *fl=0);
   virtual     void       Set(Int_t n);
   virtual     void       Set(Int_t n, Char_t *array);
   virtual     void       Reset(Int_t c=0);
//   void       &operator[](Int_t i);
 
   ClassDef(St_Table,0)  // Array of the STAF structure
};
 
inline void *St_Table::At(Int_t i)
{
   if (!BoundsOk("St_Table::At", i))
      i = 0;
   return (void *)(s_Table+i*(*s_Size));
}
#if 0 
inline void &St_Table::operator[](Int_t i)
{
   if (!BoundsOk("St_Table::operator[]", i))
      i = 0;
    return (void &)(s_Table+i*(*s_Size));
}
#endif
#endif 

