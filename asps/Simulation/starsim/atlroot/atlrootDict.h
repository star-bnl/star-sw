/********************************************************************
* atlrootDict.h
********************************************************************/
#ifdef __CINT__
#error atlrootDict.h/C is only for compilation. Abort cint.
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
extern void G__cpp_setup_tagtableatlrootDict();
extern void G__cpp_setup_inheritanceatlrootDict();
extern void G__cpp_setup_typetableatlrootDict();
extern void G__cpp_setup_memvaratlrootDict();
extern void G__cpp_setup_globalatlrootDict();
extern void G__cpp_setup_memfuncatlrootDict();
extern void G__cpp_setup_funcatlrootDict();
extern void G__set_cpp_environmentatlrootDict();
}


#include "TROOT.h"
#include "TMemberInspector.h"
#include "agconvert.h"
#include "aroot.h"

#ifndef G__MEMFUNCBODY
#endif

extern G__linked_taginfo G__atlrootDictLN_TClass;
extern G__linked_taginfo G__atlrootDictLN_TBuffer;
extern G__linked_taginfo G__atlrootDictLN_TMemberInspector;
extern G__linked_taginfo G__atlrootDictLN_TVolumeView;
extern G__linked_taginfo G__atlrootDictLN_TVolume;
extern G__linked_taginfo G__atlrootDictLN_agconvert;
extern G__linked_taginfo G__atlrootDictLN_aroot;

/* STUB derived class for protected member access */
