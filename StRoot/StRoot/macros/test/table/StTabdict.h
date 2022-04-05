/********************************************************************
* StTabdict.h
********************************************************************/
#ifdef __CINT__
#error StTabdict.h/C is only for compilation. Abort cint.
#endif
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define G__ANSIHEADER
#define G__DICTIONARY
#include "G__ci.h"
extern "C" {
extern void G__cpp_setup_tagtableStTabdict();
extern void G__cpp_setup_inheritanceStTabdict();
extern void G__cpp_setup_typetableStTabdict();
extern void G__cpp_setup_memvarStTabdict();
extern void G__cpp_setup_globalStTabdict();
extern void G__cpp_setup_memfuncStTabdict();
extern void G__cpp_setup_funcStTabdict();
extern void G__set_cpp_environmentStTabdict();
}


#include "TROOT.h"
#include "TMemberInspector.h"
#include "StTab.h"
#include <algorithm>
namespace std { }
using namespace std;

#ifndef G__MEMFUNCBODY
#endif

extern G__linked_taginfo G__StTabdictLN_TClass;
extern G__linked_taginfo G__StTabdictLN_TBuffer;
extern G__linked_taginfo G__StTabdictLN_TMemberInspector;
extern G__linked_taginfo G__StTabdictLN_TObject;
extern G__linked_taginfo G__StTabdictLN_TNamed;
extern G__linked_taginfo G__StTabdictLN_Tab_st;
extern G__linked_taginfo G__StTabdictLN_TTable;
extern G__linked_taginfo G__StTabdictLN_TDataSet;
extern G__linked_taginfo G__StTabdictLN_TTableDescriptor;
extern G__linked_taginfo G__StTabdictLN_vectorlElongcOallocatorlElonggRsPgR;
extern G__linked_taginfo G__StTabdictLN_reverse_iteratorlEvectorlElongcOallocatorlElonggRsPgRcLcLiteratorgR;
extern G__linked_taginfo G__StTabdictLN_StTab;

/* STUB derived class for protected member access */
