//*-- Author :    Valeri Fine  08/12/94 begin_html mailto://fine@bnl.gov  end_html

#ifndef ROOT_Stypes
#define ROOT_Stypes
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Stypes                                                               //
// $Id: Stypes.h,v 1.12 2000/02/29 01:54:49 fine Exp $                                                               
// Basic types used by STAF - ROOT interface.                           //
//                                                                      //
// This header file contains the set of the macro definitions           //
// to generate a ROOT dictionary for "pure" C-strucutre the way ROOT    //
// does it for the "normal" C++ classes                                 //
//                                                                      //
// This header file should be included into the all STAF table wrapper  //
// classes (by stic compiler)                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include "Rtypes.h"

enum EReturnCodes { 
  kStOK=0, 	// OK
  kStOk=0, 	// OK
  kStWarn, 	// Warning, something wrong but work can be continued
  kStEOF, 	// End Of File 
  kStErr, 	// Error, drop this and go to the next event
  kStFatal      // Fatal error, processing impossible
};  
#ifdef ANSICPP
#   define _QUOTE2_(name1,name2) _QUOTE_(name1##name2)
#else
#   define _QUOTE2_(name1,name2) _QUOTE_(_NAME1_(name1)name2)
#endif

#define _TableInit_(name) \
   extern void AddClass(const char *cname, Version_t id, VoidFuncPtr_t dict); \
   extern void RemoveClass(const char *cname); \
   class _NAME3_(__St_,name,Init__) { \
      public: \
         _NAME3_(__St_,name,Init__)() { \
            AddClass(_QUOTE2_(St_,name), _NAME2_(St_,name)::Class_Version(), \
                     &_NAME2_(St_,name)::Dictionary); \
         } \
         ~_NAME3_(__St_,name,Init__)() { \
            RemoveClass(_QUOTE2_(St_,name)); \
            RemoveClass(_QUOTE2_(name,_st)); \
         } \
   }; \
   static _NAME3_(__St_,name,Init__) _NAME3_(__gSt_,name,Init__);
  
#define _TableImp_(name) \
   TClass *_NAME2_(St_,name)::Class() \
          { if (!fgIsA) _NAME2_(St_,name)::Dictionary(); return fgIsA; } \
   const char *_NAME2_(St_,name)::ImplFileName() { return __FILE__; } \
   int _NAME2_(St_,name)::ImplFileLine() { return __LINE__; } \
   TClass *_NAME2_(St_,name)::fgIsA = 0; \
   _TableInit_(name)

#define TableStreamerImp(name)                                           \
void _NAME2_(St_,name)::Streamer(TBuffer &R__b) {                        \
   if (!R__b.IsReading()) R__b.WriteVersion(_NAME2_(St_,name)::IsA());   \
   St_Table::Streamer(R__b); }                                             

#define TableImp(name)                                             \
   void _NAME2_(St_,name)::Dictionary()                            \
   {                                                               \
      TClass *c = CreateClass(_QUOTE2_(St_,name), Class_Version(), \
                              DeclFileName(), ImplFileName(),      \
                              DeclFileLine(), ImplFileLine());     \
                  CreateClass(_QUOTE2_(name,_st), Class_Version(), \
                              _QUOTE2_(name,.h), _QUOTE2_(name,.h),\
                               1,               1 );               \
      fgIsA = c;                                                   \
   }                                                               \
   _TableImp_(name)

#define TableImpl(name)                                            \
  St_tableDescriptor *_NAME2_(St_,name)::fgColDescriptors = 0;     \
  TableImp(name)                                                   \
  TableStreamerImp(name)

// $Log: Stypes.h,v $
// Revision 1.12  2000/02/29 01:54:49  fine
// St_Table -> turn automatic schema evolution for table version 2 and above
//
// Revision 1.11  2000/01/23 20:58:53  fine
// new macro ClassDefTable has been introduced
//
// Revision 1.10  2000/01/12 18:07:24  fine
//  cvs symbols have been added and copyright class introduced
//
#endif 


#define ClassDefTable(stem)                       \
  class _NAME2_(St_,stem) : public St_Table       \
  {                                               \
   protected:                                     \
     static St_tableDescriptor *fgColDescriptors; \
     virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}                \
     virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}       \
   public:                                        \
    _NAME2_(St_,stem)() : St_Table(_QUOTE_(stem),sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));}    \
    _NAME2_(St_,stem)(Text_t *name) : St_Table(name,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));} \
    _NAME2_(St_,stem)(Int_t n) : St_Table(_QUOTE_(stem),n,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));}     \
    _NAME2_(St_,stem)(Text_t *name,Int_t n) : St_Table(name,n,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));} \
    _NAME2_(stem,_st) *GetTable(Int_t i=0) const { return ((_NAME2_(stem,_st) *)s_Table)+i;}                       \
    _NAME2_(stem,_st) &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }                 \
    const _NAME2_(stem,_st) &operator[](Int_t i) const { assert(i>=0 && i < GetNRows()); return *((const _NAME2_(stem,_st) *)(GetTable(i))); } \
    ClassDef(_NAME2_(St_,stem),0); /*C++ wrapper for <stem> StAF table */                                   \
  };                   

