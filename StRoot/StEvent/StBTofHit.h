/*!
 * \class StBTofHit 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofHit.h,v 2.1 2008/12/22 20:30:58 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBTofHit.h,v $
 * Revision 2.1  2008/12/22 20:30:58  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StBTofHit_hh
#define StBTofHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StContainers.h"

class StTrack;

class StBTofHit : public StObject {
public:
    StBTofHit();
    ~StBTofHit();

    unsigned char   tray() const;
    unsigned char   module() const;
    unsigned char   cell() const;
    double          leadingEdgeTime() const;
    double          trailingEdgeTime() const;
    double          tot() const;

    StTrack*        associatedTrack();
    const StTrack*  associatedTrack() const;
    
    unsigned short  idTruth() const;
    unsigned short  qaTruth() const;

    void setTray(unsigned char);
    void setModule(unsigned char);
    void setCell(unsigned char);
    void setLeadingEdgeTime(double);
    void setTrailingEdgeTime(double);
    void setAssociatedTrack(StTrack*);
    void setIdTruth(Int_t idtru, Int_t qatru=0);

 protected:
    UChar_t   mTray;
    UChar_t   mModule;
    UChar_t   mCell;
    Double_t  mLeadingEdgeTime;
    Double_t  mTrailingEdgeTime;
    //    StTrack *mAssociatedTrack;   //$LINK
#ifdef __CINT__
    StObjLink        mAssociatedTrack;		
#else
    StLink<StTrack>  mAssociatedTrack;		
#endif //__CINT__
    UShort_t  mIdTruth;  // simulation associated track id
    UShort_t  mQuality;  // quality of this information (percentage of charge produced by mIdTruth)

    ClassDef(StBTofHit,1)
};

ostream& operator<<(ostream&, const StTofHit&); // Printing operator

#endif
