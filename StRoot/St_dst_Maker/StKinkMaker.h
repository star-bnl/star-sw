// $Id: StKinkMaker.h,v 1.12 1999/09/24 01:23:36 fisyak Exp $
// $Log: StKinkMaker.h,v $
// Revision 1.12  1999/09/24 01:23:36  fisyak
// Reduced Include Path
//
// Revision 1.11  1999/08/26 17:30:38  wdeng
// Fix typo. Reorganize Make() function. Use shorter names for identifiers
//
// Revision 1.10  1999/08/23 22:37:29  wdeng
// New definiton with function dcaTwoLines
//
// Revision 1.9  1999/08/02 18:42:44  wdeng
// Cleanup.
//
// Revision 1.8  1999/07/15 13:57:52  perev
// cleanup
//
// Revision 1.7  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.6  1999/07/12 19:03:13  wdeng
// move #define statements to StKinkMaker.cxx
//
// Revision 1.5  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//
// Revision 1.4  1999/07/07 15:47:36  wdeng
// add Id and Log at the first two lines for the purpose of version maintainance
//
#ifndef STAR_StKinkMaker
#define STAR_StKinkMaker
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

#include "tables/St_tkf_tkfpar_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_dst_tkf_vertex_Table.h"

#include "tables/St_tpt_track_Table.h"
#include "tables/St_tte_eval_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

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
  Bool_t            m_kinkEvalOn;   //switch for the evaluation
  // static Char_t  m_VersionCVS = "$Id: StKinkMaker.h,v 1.12 1999/09/24 01:23:36 fisyak Exp $";
  St_tkf_tkfpar    *m_tkfpar;          //!

  StKinkLocalTrack *myTrack1;          //! 
  StKinkLocalTrack *myTrack2;          //!

  dst_tkf_vertex_st kinkVtxRow;        //!
  dst_vertex_st     dstVtxRow;         //!

  StThreeVectorD   parentMoment, daughterMoment;
  StThreeVectorD   mKinkVertex;
  Float_t          parentImpact, daughterImpact;
  Float_t          dca, decayAngle;

  Int_t  kinkVtxIndex, dstVtxIndex;

 protected:
  Int_t    MeetTwoHelices2D(const Float_t cut, const StPhysicalHelixD& helix1, 
			    const StPhysicalHelixD& helix2, Float_t xCords[2], 
			    Float_t yCords[2]);
  Float_t  DcaTwoLines(const StThreeVectorD& t1Project, const StThreeVectorD& t2Project, 
		       const StThreeVectorD& parentMom, const StThreeVectorD& daughterMom, 
		       Float_t point1AtDca[3], Float_t point2AtDca[3]);
  void     FillTableRow();
  void     FillIflag();
 
 public: 
  StKinkMaker(const char *name="kink");
  virtual  ~StKinkMaker(); 
  virtual  Int_t  Init();
  virtual  Int_t  Make();
  virtual  void   kinkEval(Bool_t flag=kTRUE){m_kinkEvalOn=flag;} // *MENU*
  virtual  void   kinkEvalOn() {kinkEval();} 
  virtual  void   kinkEvalOff(){kinkEval(kFALSE);}      
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StKinkMaker.h,v 1.12 1999/09/24 01:23:36 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StKinkMaker, 1)   //StAF chain virtual base class for Makers
};
    
#endif
    
  
