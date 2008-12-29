#ifndef ST_CHARGED_PION_VERTEX_HH
#define ST_CHARGED_PION_VERTEX_HH

// $Id: StChargedPionVertex.h,v 1.3 2008/12/29 15:58:31 kocolosk Exp $

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
 * Revision 1.3  2008/12/29 15:58:31  kocolosk
 * removed commented code and added Id and Log as needed
 *
 *****************************************************************************/
