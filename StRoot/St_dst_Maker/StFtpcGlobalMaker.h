#ifndef STAR_StFtpcGlobalMaker
#define STAR_StFtpcGlobalMaker

// $Id: StFtpcGlobalMaker.h,v 1.4 2004/02/12 18:38:21 oldi Exp $
// $Log: StFtpcGlobalMaker.h,v $
// Revision 1.4  2004/02/12 18:38:21  oldi
// Removal of intermediate tables to store FTPC hits and tracks.
// Now the TObjArray's of hits and tracks are passed directly to
// StFtpcGlobalMaker.cxx and StFtpcPrimaryMaker.cxx where they are (still)
// copied into the dst tables.
//
// Revision 1.3  2003/09/10 19:47:44  perev
// ansi corrs
//
// Revision 1.2  2001/03/30 13:30:12  jcs
// correct Id and Log
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcGlobalMaker virtual base class for Maker                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "tables/St_fde_fdepar_Table.h"  

#include "tables/St_dst_point_Table.h" 
#include "tables/St_dst_track_Table.h" 
#include "tables/St_dst_vertex_Table.h" 
#include "tables/St_dst_dedx_Table.h" 

class St_fde_fdepar;
class St_dst_point;
class St_dst_track;
class St_dst_vertex;
class St_dst_dedx;

class StFtpcGlobalMaker : public StMaker {
  
 private:
  St_fde_fdepar        *m_fdepar;            //!

 protected:
  
  
 public: 
  StFtpcGlobalMaker(const char *name="fglobal");
  virtual       ~StFtpcGlobalMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcGlobalMaker.h,v 1.4 2004/02/12 18:38:21 oldi Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StFtpcGlobalMaker,0)   //StAF chain virtual base class for Makers
    };
    
#endif
    
