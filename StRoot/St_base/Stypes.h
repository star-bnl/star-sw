//*-- Author :    Valeri Fine  08/12/94 begin_html mailto://fine@bnl.gov  end_html

#ifndef STAR_Stypes
#define STAR_Stypes
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Stypes                                                               //
// $Id: Stypes.h,v 1.14 2000/03/24 20:35:25 fine Exp $                                                               
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
#include "Ttypes.h"

#if 0
 
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
   TTable::Streamer(R__b); }                                             

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
  TTableDescriptor *_NAME2_(St_,name)::fgColDescriptors = 0;       \
  TableImp(name)                                                   \
  TableStreamerImp(name)
//___________________________________________________________________
//___________________________________________________________________
#define _TableInit_(className,structName)                          \
   extern void AddClass(const char *cname, Version_t id, VoidFuncPtr_t dict); \
   extern void RemoveClass(const char *cname);                     \
   class _NAME2_(__name,Init__) {                                  \
      public:                                                      \
         _NAME2_(__name,Init__)() {                                \
            AddClass(_QUOTE_(name), name::Class_Version(),         \
                     &name::Dictionary);                           \
         }                                                         \
         ~_NAME2_(__name,Init__)() {                               \
            RemoveClass(_QUOTE_(className));                       \
            RemoveClass(_QUOTE_(structName));                      \
         }                                                         \
   };                                                              \
   static _NAME2_(__name,Init__) _NAME3_(__g,name,Init__);

#define TableClassStreamerImp(className,structName)              \
void className::Streamer(TBuffer &R__b) {                        \
   if (!R__b.IsReading()) R__b.WriteVersion(className::IsA());   \
   TTable::Streamer(R__b); }                                             

//___________________________________________________________________
#define _TableClassImp_(className,structName)                    \
   TClass *className::Class()                                    \
          { if (!fgIsA) className::Dictionary(); return fgIsA; } \
   const char *className::ImplFileName() { return __FILE__; }    \
   int className::ImplFileLine() { return __LINE__; }            \
   TClass *className::fgIsA = 0;                                 \
   _TableInit_(className,structName)


//___________________________________________________________________
#define TableClassImp(className,structName)                         \
   void name::Dictionary()                                          \
   {                                                                \
      TClass *c = CreateClass(_QUOTE_(className), Class_Version(),  \
                              DeclFileName(), ImplFileName(),       \
                              DeclFileLine(), ImplFileLine());      \
                  CreateClass(_QUOTE_(structName), Class_Version(), \
                              _QUOTE2_(structName,.h), _QUOTE2_(structName,.h),\
                               1,               1 );                \
      fgIsA = c;                                                    \
   }                                                                \
   _TableClassImp_(className,structName)

//___________________________________________________________________
#define TableClassImpl(className,structName)                       \
  TTableDescriptor *name::fgColDescriptors = 0;                    \
  TableClassImp(className,structName)                              \
  TableClassStreamerImp(className,structName)

// $Log: Stypes.h,v $
// Revision 1.14  2000/03/24 20:35:25  fine
// adjusted to ROOT 2.24. Doesn't work yet. Under development
//
// Revision 1.11  2000/01/23 20:58:53  fine
// new macro ClassDefTable has been introduced
//
// Revision 1.10  2000/01/12 18:07:24  fine
//  cvs symbols have been added and copyright class introduced
//

#define ClassDefTable(stem)                       \
  class _NAME2_(St_,stem) : public TTable       \
  {                                               \
   protected:                                     \
     static St_tableDescriptor *fgColDescriptors; \
     virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}                \
     virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}       \
  public:                                         \
    _NAME2_(St_,stem)() : TTable(_QUOTE_(stem),sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));}    \
    _NAME2_(St_,stem)(Text_t *name) : TTable(name,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));} \
    _NAME2_(St_,stem)(Int_t n) : TTable(_QUOTE_(stem),n,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));}     \
    _NAME2_(St_,stem)(Text_t *name,Int_t n) : TTable(name,n,sizeof(_NAME2_(stem,_st))) {SetType(_QUOTE_(stem));} \
    _NAME2_(stem,_st) *GetTable(Int_t i=0) const { return ((_NAME2_(stem,_st) *)s_Table)+i;}                       \
    _NAME2_(stem,_st) &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }                 \
    const _NAME2_(stem,_st) &operator[](Int_t i) const { assert(i>=0 && i < GetNRows()); return *((const _NAME2_(stem,_st) *)(GetTable(i))); } \
    ClassDef(_NAME2_(St_,stem),0); /*C++ wrapper for <stem> StAF table */                                   \
  };                   

#endif
#endif 
