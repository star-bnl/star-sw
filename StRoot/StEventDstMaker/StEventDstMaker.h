// $Id: StEventDstMaker.h,v 1.2 2003/09/10 19:47:14 perev Exp $
// $Log: StEventDstMaker.h,v $
// Revision 1.2  2003/09/10 19:47:14  perev
// ansi corrs
//
// Revision 1.1  2001/05/30 17:48:30  perev
// StEvent branching
//
//
#ifndef STAR_StEventDstMaker
#define STAR_StEventDstMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDstMaker                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
class StEventDstMaker : public StMaker {
 public: 
                  StEventDstMaker(const char *name="eventDst");
   virtual       ~StEventDstMaker();
   virtual Int_t Init();
   virtual Int_t  Make();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventDstMaker.h,v 1.2 2003/09/10 19:47:14 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StEventDstMaker,0)   //
};

#endif
