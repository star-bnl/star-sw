/********************************************************************
* TGeant3Cint.h
********************************************************************/
#ifdef __CINT__
#error TGeant3Cint.h/C is only for compilation. Abort cint.
#endif
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#define G__ANSIHEADER
#include "G__ci.h"
extern "C" {
extern void G__cpp_setup_tagtableTGeant3Cint();
extern void G__cpp_setup_inheritanceTGeant3Cint();
extern void G__cpp_setup_typetableTGeant3Cint();
extern void G__cpp_setup_memvarTGeant3Cint();
extern void G__cpp_setup_globalTGeant3Cint();
extern void G__cpp_setup_memfuncTGeant3Cint();
extern void G__cpp_setup_funcTGeant3Cint();
extern void G__set_cpp_environmentTGeant3Cint();
}


#include "TROOT.h"
#include "TMemberInspector.h"
#include "TGeant3.h"
#include "TPaveTree.h"
#include "THIGZ.h"
#include "TGeant3GUI.h"
#include "AliGeant3.h"

#ifndef G__MEMFUNCBODY
#endif

extern G__linked_taginfo G__TGeant3CintLN_TClass;
extern G__linked_taginfo G__TGeant3CintLN_TObject;
extern G__linked_taginfo G__TGeant3CintLN_TString;
extern G__linked_taginfo G__TGeant3CintLN_TNamed;
extern G__linked_taginfo G__TGeant3CintLN_TVirtualPad;
extern G__linked_taginfo G__TGeant3CintLN_AliMC;
extern G__linked_taginfo G__TGeant3CintLN_Quest_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcbank_t;
extern G__linked_taginfo G__TGeant3CintLN_Gclink_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcflag_t;
extern G__linked_taginfo G__TGeant3CintLN_Gckine_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcking_t;
extern G__linked_taginfo G__TGeant3CintLN_Gckin2_t;
extern G__linked_taginfo G__TGeant3CintLN_Gckin3_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcmate_t;
extern G__linked_taginfo G__TGeant3CintLN_Gctmed_t;
extern G__linked_taginfo G__TGeant3CintLN_Gctrak_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcvolu_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcsets_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcnum_t;
extern G__linked_taginfo G__TGeant3CintLN_Gccuts_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcmulo_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcphys_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcopti_t;
extern G__linked_taginfo G__TGeant3CintLN_Gctlit_t;
extern G__linked_taginfo G__TGeant3CintLN_Gcvdma_t;
extern G__linked_taginfo G__TGeant3CintLN_Gctpol_t;
extern G__linked_taginfo G__TGeant3CintLN_Ertrio_t;
extern G__linked_taginfo G__TGeant3CintLN_Eropts_t;
extern G__linked_taginfo G__TGeant3CintLN_Eroptc_t;
extern G__linked_taginfo G__TGeant3CintLN_Erwork_t;
extern G__linked_taginfo G__TGeant3CintLN_TGeant3;
extern G__linked_taginfo G__TGeant3CintLN_TGeant3cLcLdA;
extern G__linked_taginfo G__TGeant3CintLN_TAttLine;
extern G__linked_taginfo G__TGeant3CintLN_TAttFill;
extern G__linked_taginfo G__TGeant3CintLN_TBox;
extern G__linked_taginfo G__TGeant3CintLN_TPave;
extern G__linked_taginfo G__TGeant3CintLN_TAttText;
extern G__linked_taginfo G__TGeant3CintLN_TPaveLabel;
extern G__linked_taginfo G__TGeant3CintLN_TPaveTree;
extern G__linked_taginfo G__TGeant3CintLN_TAttPad;
extern G__linked_taginfo G__TGeant3CintLN_TCanvas;
extern G__linked_taginfo G__TGeant3CintLN_TPad;
extern G__linked_taginfo G__TGeant3CintLN_THIGZ;
extern G__linked_taginfo G__TGeant3CintLN_TGObject;
extern G__linked_taginfo G__TGeant3CintLN_TGWindow;
extern G__linked_taginfo G__TGeant3CintLN_TGFrame;
extern G__linked_taginfo G__TGeant3CintLN_TGCompositeFrame;
extern G__linked_taginfo G__TGeant3CintLN_TGLayoutHints;
extern G__linked_taginfo G__TGeant3CintLN_TGHorizontalFrame;
extern G__linked_taginfo G__TGeant3CintLN_TGMainFrame;
extern G__linked_taginfo G__TGeant3CintLN_TGCanvas;
extern G__linked_taginfo G__TGeant3CintLN_TGListBox;
extern G__linked_taginfo G__TGeant3CintLN_TGListTreeItem;
extern G__linked_taginfo G__TGeant3CintLN_TGListTree;
extern G__linked_taginfo G__TGeant3CintLN_TGLabel;
extern G__linked_taginfo G__TGeant3CintLN_TGTextButton;
extern G__linked_taginfo G__TGeant3CintLN_TGTextBuffer;
extern G__linked_taginfo G__TGeant3CintLN_TGTextEntry;
extern G__linked_taginfo G__TGeant3CintLN_TGPopupMenu;
extern G__linked_taginfo G__TGeant3CintLN_TGMenuBar;
extern G__linked_taginfo G__TGeant3CintLN_TGComboBox;
extern G__linked_taginfo G__TGeant3CintLN_TGTab;
extern G__linked_taginfo G__TGeant3CintLN_TClonesArray;
extern G__linked_taginfo G__TGeant3CintLN_AliGuiGeomDialog;
extern G__linked_taginfo G__TGeant3CintLN_AliGuiGeomMain;
extern G__linked_taginfo G__TGeant3CintLN_AliDrawVolume;
extern G__linked_taginfo G__TGeant3CintLN_AliGUIMaterial;
extern G__linked_taginfo G__TGeant3CintLN_AliGUIMedium;
extern G__linked_taginfo G__TGeant3CintLN_AliGeant3GeometryGUI;
extern G__linked_taginfo G__TGeant3CintLN_AliGeant3;
