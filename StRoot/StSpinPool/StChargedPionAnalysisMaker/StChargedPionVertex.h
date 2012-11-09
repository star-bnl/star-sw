#ifndef ST_CHARGED_PION_VERTEX_HH
#define ST_CHARGED_PION_VERTEX_HH

// $Id: StChargedPionVertex.h,v 1.4 2012/11/09 03:31:34 perev Exp $

#include "TVector3.h"

class StChargedPionVertex : public TVector3 {
public:
    StChargedPionVertex();
    virtual ~StChargedPionVertex();
    
    float ranking() { return mRanking; }
    
    void setRanking(float a) { mRanking = a; }
    
private:
    Float_t mRanking;
    
    ClassDef(StChargedPionVertex, 1)
};

#endif

/*****************************************************************************
 * $Log: StChargedPionVertex.h,v $
 * Revision 1.4  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.3  2008/12/29 15:58:31  kocolosk
 * removed commented code and added $Id: StChargedPionVertex.h,v 1.4 2012/11/09 03:31:34 perev Exp $/$Log: StChargedPionVertex.h,v $
 * removed commented code and added $Id$/Revision 1.4  2012/11/09 03:31:34  perev
 * removed commented code and added $Id$/Cleanup
 * removed commented code and added $Id$/ as needed
 *
 *****************************************************************************/
