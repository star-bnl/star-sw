/*!
 * \class StMtdHit 
 */
/***************************************************************************
 *
 * $Id: StMtdHit.h,v 2.3 2018/03/15 22:00:34 smirnovd Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdHit.h,v $
 * Revision 2.3  2018/03/15 22:00:34  smirnovd
 * Fix linker error by removing declared but undefined functions
 *
 * Revision 2.2  2015/10/09 17:46:14  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMtdHit_hh
#define StMtdHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StContainers.h"

class StTrack;

class StMtdHit : public StObject {
public:
    StMtdHit();
    ~StMtdHit();

    int             backleg() const;
    int             module() const;
    int             cell() const;
    pair<double,double>   leadingEdgeTime() const;
    pair<double,double>   trailingEdgeTime() const;
    pair<double,double>   tot() const;
    double          tof() const;

    StTrack*        associatedTrack();
    const StTrack*  associatedTrack() const;
    
    int             idTruth() const;
    int             qaTruth() const;

    void setBackleg(unsigned char);
    void setModule(unsigned char);
    void setCell(unsigned char);
    void setLeadingEdgeTime(pair<double,double>);
    void setTrailingEdgeTime(pair<double,double>);
    void setAssociatedTrack(StTrack*);
    void setIdTruth(int idtru, int qatru=0);

 protected:
    UChar_t   mBackLeg;
    UChar_t   mModule;
    UChar_t   mCell;
    pair<Double_t,Double_t>  mLeadingEdgeTime;
    pair<Double_t,Double_t>  mTrailingEdgeTime;
    //    StTrack *mAssociatedTrack;   //$LINK
#ifdef __CINT__
    StObjLink        mAssociatedTrack;		
#else
    StLink<StTrack>  mAssociatedTrack;		
#endif //__CINT__
    Int_t  mIdTruth;  // simulation associated track id
    UShort_t  mQuality;  // quality of this information (percentage of charge produced by mIdTruth)

    ClassDef(StMtdHit,2)
};

ostream& operator<<(ostream&, const StMtdHit&); // Printing operator

#endif
