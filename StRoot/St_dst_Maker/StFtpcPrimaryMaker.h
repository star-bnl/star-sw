#ifndef STAR_StFtpcPrimaryMaker
#define STAR_StFtpcPrimaryMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcPrimaryMaker virtual base class for Maker                          //
//                                                                      //
//  
// 
//
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
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcPrimaryMaker.h,v 1.1 2000/09/09 18:09:21 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StFtpcPrimaryMaker, 1)   //StAF chain virtual base class for Makers
    };
    
#endif
    
