/***************************************************************************
 *
 * $Id: StStrangeTagsMaker.h,v 1.5 2000/01/27 19:29:50 fisyak Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 ***************************************************************************
 *
 * Description:   Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StStrangeTagsMaker.h,v $
 * Revision 1.5  2000/01/27 19:29:50  fisyak
 * Put StrangeTag to .data
 *
 * Revision 1.4  1999/09/24 01:23:26  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/07/15 13:57:29  perev
 * cleanup
 *
 * Revision 1.2  1999/02/24 02:03:39  genevb
 * Add Xi vertices
 *
 * Revision 1.1  1999/02/21 23:35:12  genevb
 * Strangeness Tags Maker
 *
 **************************************************************************/
#ifndef StStrangeTagsMaker_HH
#define StStrangeTagsMaker_HH

#include <iostream.h>
#include "TROOT.h"
#include "StMaker.h"
#include "tables/St_StrangeTag_Table.h"
class StEvent;

class StStrangeTagsMaker : public StMaker {
public:
    StStrangeTagsMaker(const char *name="StrangeTags", const char *title=0);
    ~StStrangeTagsMaker();
    
    Int_t  Init();                    // create and fills the tags
    Int_t  Make();                    // create and fills the tags
    
    StrangeTag_st* tag();             // returns pointer to the tag table
    void          printTag(ostream& = cout);
    
protected:
    void   fillTag();                 // does the actual work;
    
private:
    StrangeTag_st*  mTagTable;        //! the tag table to fill
    StEvent*        mEvent;           //! pointer to DST data

    Float_t         mRange;
    Float_t         m2Range;
    Float_t         mMasspi2;
    Float_t         mMasspr2;
    Float_t         mMassla2;
    
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StStrangeTagsMaker.h,v 1.5 2000/01/27 19:29:50 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

    ClassDef(StStrangeTagsMaker, 1)   // macro for rootcint
};

#endif
