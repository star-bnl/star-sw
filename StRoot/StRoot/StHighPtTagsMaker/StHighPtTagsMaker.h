/*! 
 * \class StHighPtTagsMaker
 * \author Thomas Henry, July 2004, based on Gene Van Buren, Feb 1995, Lee Barnby and Leon Gaillard, July 2004
 * \brief A maker to fill the HighPt tags
 * 
 * Tags are written in tables and end up in the tags.root file. It is expected that tags may
 * be used, either uniquely or in combination, to select events. This maker fills the tags 
 * related to HighPt events i.e. High Towers and High Pt Tracks.  Other interesting 
 * HighPt related variables such as refMult are expected to be in the general
 * tags maker.
 *
 * $Id: StHighPtTagsMaker.h,v 1.2 2014/08/06 11:43:20 jeromel Exp $
 *
 */


#ifndef StHighPtTagsMaker_HH
#define StHighPtTagsMaker_HH

#include <iostream>
#include "TROOT.h"
#include "StMaker.h"
#include "tables/St_HighPtTag_Table.h"
class StEvent;

class StHighPtTagsMaker : public StMaker {
public:
    StHighPtTagsMaker(const char *name="HighPtTags", const char *title=0);
    ~StHighPtTagsMaker();
    
    Int_t  Init();                    // create and fills the tags
    Int_t  Make();                    // create and fills the tags
    
    HighPtTag_st* tag();             // returns pointer to the tag table
    void          printTag(ostream& = cout);
    
protected:
    void   fillTag();                 // does the actual work;
    
private:
    HighPtTag_st*  mTagTable;        //! the tag table to fill
    StEvent*        mEvent;           //! pointer to DST data

    // Comment for Thomas:
    // These are redeclared as local variables in the fillTag() method
    // then there is no need to declare them in the header file
    // as data members.  Data members are used to store values that
    // are then passed between functions or returned by the class
    // but there are no access methods for that, so it is
    // better to just use them as local variables in fillTag() for the
    // moment. MCBS
//     Float_t         mHighPtTrack;
//     Float_t         mMaxRawEtBEMCTower;
//     Float_t         mMaxRawEtEEMCTower;
    
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHighPtTagsMaker.h,v 1.2 2014/08/06 11:43:20 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StHighPtTagsMaker,1)   // macro for rootcint
};

#endif

/***************************************************************************
 *
 * $Log: StHighPtTagsMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:20  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2004/07/30 23:02:06  calderon
 * Revised entry after testing.  Commented out data members that are used
 * as local variables in fillTag().  Speeded up primary track loop.  Added
 * protection against null pointers.
 *
 * Revision 1.0  2004/07/28 07:47:45  thenry
 * Created
 *
 **************************************************************************/
