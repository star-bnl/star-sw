#ifndef ROOT_StTGeant3gu
#define ROOT_StTGeant3gu 
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: StTGeant3gu.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

//////////////////////////////////////////////// 
//  C++ interface to Geant3 basic routines    // 
//////////////////////////////////////////////// 
/* Modified ALICE version for STAR experiment at BNL. By V.Perev */



#define WITHG3
#ifdef WITHROOT
#undef WITHG3
#endif
#ifdef WITHBOTH
#undef WITHG3
#undef WITHROOT
#endif
#include "Rtypes.h"

// Rewritten to C++ functions
class StTGeant3;
class TGeoManager;

class StTGeant3gu
{
public:
static void InitBridge();

static Float_t Gudtim(Float_t&, Float_t&, Int_t&, Int_t&);
static Float_t Guplsh(Int_t&, Int_t&);
static void Guhadr();
static void Guout();
static void Guphad();
static void Gudcay();
static void Guiget(Int_t&, Int_t&, Int_t&);
static void Gudigi();
static void Guinme(Float_t*, Int_t&, Float_t*, Int_t& IYES);
static void Guinti();
static void Gunear(Int_t&, Int_t&, Float_t*, Int_t&);
static void Guskip(Int_t& ISKIP);
static void Guswim(Float_t& CHARGE, Float_t& STEP, Float_t* VECT, Float_t* VOUT);
static void Guview(Int_t&, Int_t&, DEFCHARD, Int_t& DEFCHARL);
static void Gupara();
static void Gutrak();
static void Gutrev();
static void Gufld(Float_t *x, Float_t *b);
static void Gustep();
static void Gukine ();

static StTGeant3     *fgGeant3;
static TGeoManager *fgGeoMgr;
};

#ifndef WIN32
#  define gudigi gudigi_
#  define guhadr guhadr_
#  define guout  guout_
#  define guphad guphad_
#  define gudcay gudcay_
#  define guiget guiget_
#  define guinme guinme_
#  define guinti guinti_
#  define gunear gunear_
#  define guskip guskip_
#  define guview guview_
#  define gupara gupara_
#  define gudtim gudtim_
#  define guplsh guplsh_
#  define gutrev gutrev_
#  define gutrak gutrak_
#  define guswim guswim_
#  define gufld  gufld_
#  define gustep gustep_
#  define gukine gukine_

#  define gheish gheish_
#  define flufin flufin_
#  define gfmfin gfmfin_
#  define gpghei gpghei_
#  define fldist fldist_
#  define gfmdis gfmdis_
#  define g3helx3 g3helx3_
#  define g3helix g3helix_
#  define g3rkuta g3rkuta_
#  define g3track g3track_
#  define gtreveroot gtreveroot_
#  define g3last  g3last_
#  define g3invol g3invol_
#  define g3tmedi g3tmedi_
#  define g3media g3media_
#  define g3tmany g3tmany_
#  define g3tnext g3tnext_
#  define ginvol ginvol_
#  define gtmedi gtmedi_
#  define gtmany gtmany_
#  define gtonly gtonly_
#  define gmedia gmedia_
#  define glvolu glvolu_
#  define gtnext gtnext_

#else
#  define gudigi GUDIGI
#  define guhadr GUHADR
#  define guout  GUOUT
#  define guphad GUPHAD
#  define gudcay GUDCAY
#  define guiget GUIGET
#  define guinme GUINME
#  define guinti GUINTI
#  define gunear GUNEAR
#  define guskip GUSKIP
#  define guview GUVIEW
#  define gupara GUPARA
#  define gudtim GUDTIM
#  define guplsh GUPLSH
#  define gutrev GUTREV
#  define gutrak GUTRAK
#  define guswim GUSWIM
#  define gufld  GUFLD
#  define gustep GUSTEP
#  define gukine GUKINE

#  define gheish GHEISH
#  define flufin FLUFIN
#  define gfmfin GFMFIN
#  define gpghei GPGHEI
#  define fldist FLDIST
#  define gfmdis GFMDIS
#  define g3helx3 G3HELX3
#  define g3helix G3HELIX
#  define g3rkuta G3RKUTA
#  define gtrack GTRACK
#  define gtreveroot GTREVEROOT
#  define glast  GLAST
#  define ginvol GINVOL
#  define gtmedi GTMEDI
#  define gtmany GTMANY
#  define gmedia GMEDIA
#  define glvolu GLVOLU
#  define gtnext GTNEXT 

#endif

extern "C" type_of_call void gheish();
extern "C" type_of_call void flufin();
extern "C" type_of_call void gfmfin();
extern "C" type_of_call void gpghei();
extern "C" type_of_call void fldist();
extern "C" type_of_call void gfmdis();
extern "C" type_of_call void g3helx3(Float_t&, Float_t&, Float_t*, Float_t*);
extern "C" type_of_call void g3helix(Float_t&, Float_t&, Float_t*, Float_t*);
extern "C" type_of_call void g3rkuta(Float_t&, Float_t&, Float_t*, Float_t*);
extern "C" type_of_call void g3track();
extern "C" type_of_call void gtreveroot();
extern "C" type_of_call void g3last();
extern "C" type_of_call void g3invol(Float_t*, Int_t&);
extern "C" type_of_call void g3tmedi(Float_t*, Int_t&);
extern "C" type_of_call void g3tmany(Int_t&);
extern "C" type_of_call void g3media(Float_t*, Int_t&, Int_t&);
extern "C" type_of_call void g3tnext();
extern "C" type_of_call void ginvol(Float_t*, Int_t&);
extern "C" type_of_call void gtmedi(Float_t*, Int_t&);
extern "C" type_of_call void gtmany(Int_t&);
extern "C" type_of_call void gtonly(Int_t&);
extern "C" type_of_call void gmedia(Float_t*, Int_t&, Int_t&);
extern "C" type_of_call void glvolu(Int_t &nlev, Int_t *lnam,Int_t *lnum, Int_t &ier);
extern "C" type_of_call void gtnext();



#endif
