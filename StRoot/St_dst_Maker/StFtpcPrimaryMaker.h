#ifndef STAR_StFtpcPrimaryMaker
#define STAR_StFtpcPrimaryMaker

// $Id: StFtpcPrimaryMaker.h,v 1.3 2003/09/10 19:47:45 perev Exp $
// $Log: StFtpcPrimaryMaker.h,v $
// Revision 1.3  2003/09/10 19:47:45  perev
// ansi corrs
//
// Revision 1.2  2001/03/30 13:27:16  jcs
// correct Id and Log
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcPrimaryMaker virtual base class for Maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class dst_vertex_st;
class dst_track_st;
class St_dst_track;
class St_dst_vertex;

class StFtpcPrimaryMaker : public StMaker {
  
 private:
 protected:
  
  
 public: 
  StFtpcPrimaryMaker(const char *name="fprimary");
  virtual       ~StFtpcPrimaryMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcPrimaryMaker.h,v 1.3 2003/09/10 19:47:45 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StFtpcPrimaryMaker,0)   //StAF chain virtual base class for Makers
    };
    
#endif
    
