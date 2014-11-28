/***************************************************************************
 *
 * $Id: StHeavyTagMaker.h,v 1.3 2014/08/06 11:43:20 jeromel Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, July 2004
 ***************************************************************************
 *
 * Description:   Maker to fill the Heavy Flavor Tags
 *
 ***************************************************************************
 *
 * $Log: StHeavyTagMaker.h,v $
 * Revision 1.3  2014/08/06 11:43:20  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2004/07/29 23:06:12  calderon
 * Changed adc cut to match towers to 360 ADC counts,
 * and documented the origin.
 * Added Description to cxx file.
 * Removed unnecessary static_cast for StDetectorId
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
  {static const char cvs[]="Tag $Name:  $ $Id: StHeavyTagMaker.h,v 1.3 2014/08/06 11:43:20 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
protected:
    void   fillTag();                 // does the actual work;
private:
    HeavyTag_st*    mTagTable;        //! the tag table to fill
    StEvent*        mEvent;           //!
    double         mMassThres[3];      //!

   ClassDef(StHeavyTagMaker,0)   
};

#endif
