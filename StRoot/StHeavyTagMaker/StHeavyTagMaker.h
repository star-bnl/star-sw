/***************************************************************************
 *
 * $Id: StHeavyTagMaker.h,v 1.1 2004/07/29 14:04:53 jeromel Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, July 2004
 ***************************************************************************
 *
 * Description:   Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StHeavyTagMaker.h,v $
 * Revision 1.1  2004/07/29 14:04:53  jeromel
 * Written by MC. First version
 *
 **************************************************************************/

#ifndef STAR_StHeavyTagMaker
#define STAR_StHeavyTagMaker

//
// StHeavyTagMaker 
//
#include "StMaker.h"
class HeavyTag_st;
class StEvent;

class StHeavyTagMaker : public StMaker {
 public: 
    StHeavyTagMaker(const char *name="HeavyTag",const char* title=0);
    
   virtual  ~StHeavyTagMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   HeavyTag_st* tag(); 

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHeavyTagMaker.h,v 1.1 2004/07/29 14:04:53 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}
protected:
    void   fillTag();                 // does the actual work;
private:
    HeavyTag_st*    mTagTable;        //! the tag table to fill
    StEvent*        mEvent;           //!
    double         mMassThres[3];      //!

   ClassDef(StHeavyTagMaker,0)   
};

#endif
