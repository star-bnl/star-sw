/***************************************************************************
 *
 * $Id: StStrangeTagsMaker.h,v 1.2 1999/02/24 02:03:39 genevb Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 ***************************************************************************
 *
 * Description:   Maker to fill the Strangeness Tags
 *
 ***************************************************************************
 *
 * $Log: StStrangeTagsMaker.h,v $
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
#include "tables/StrangeTag.h"
class StEvent;

class StStrangeTagsMaker : public StMaker {
public:
    StStrangeTagsMaker(const char *name, const char *title);
    ~StStrangeTagsMaker();
    
    Int_t  Init();                    // create and fills the tags
    Int_t  Make();                    // create and fills the tags
    void   PrintInfo();               // prints version
    
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
    
    ClassDef(StStrangeTagsMaker, 1)   // macro for rootcint
};

#endif
