#ifndef STAR_StKinkMaker
#define STAR_StKinkMaker

#define kaonMass 0.493677
#define pionMass 0.139569
#define muonMass 0.105658
#define pi0Mass  0.134976

#define kaonToMuonQ 0.236
#define kaonToPionQ 0.205
#define pionToMuonQ 0.030

#define radToDeg        57.2957795131
#define degToRad        0.0174532925199                 

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_kink_Maker virtual base class for Maker                           //
//                                                                      //
//                                                                      //  
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StKinkLocalTrack.hh"
#include "StPhysicalHelixD.hh"

#include "TH1.h"

#include <tables/St_tkf_tkfpar_Table.h>
#include <tables/St_dst_track_Table.h>
#include <tables/St_dst_vertex_Table.h>
#include <tables/St_dst_tkf_vertex_Table.h>

#include <tables/St_tpt_track_Table.h>
#include <tables/St_tte_eval_Table.h>
#include <tables/St_g2t_track_Table.h>
#include <tables/St_g2t_vertex_Table.h>

class St_tkf_tkfpar;
class St_dst_track;
class St_dst_vertex;
class St_dst_tkf_vertex;

class StKinkLocalTrack;
class StPhysicalHelixD;

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StKinkMaker : public StMaker {
private:
// static Char_t  m_VersionCVS = "$Id: StKinkMaker.h,v 1.1.2.1 1999/07/01 17:27:37 fisyak Exp $";
  St_tkf_tkfpar    *m_tkfpar;          //!
  StKinkLocalTrack *mKinkLocalTrack;   //!
protected:
  Int_t    meetTwoHelices2D(const Float_t cut, const StPhysicalHelixD& helix1, 
		    const StPhysicalHelixD& helix2, Float_t xCoordinates[2], 
		    Float_t yCoordinates[2]);
  Float_t  dcaTwoLines(Float_t xn1[3], Float_t xn2[3], Float_t sxz1, Float_t syz1, 
		     Float_t sxz2, Float_t syz2, Float_t point1AtDca[3], Float_t point2AtDca[3]);

public: 
  StKinkMaker(const char *name="kink");
  virtual  ~StKinkMaker(); 
  virtual  Int_t  Init();
  virtual  Int_t  Make();
  virtual  void   PrintInfo();
ClassDef(StKinkMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

