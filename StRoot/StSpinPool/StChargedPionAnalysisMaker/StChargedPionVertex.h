#ifndef ST_CHARGED_PION_VERTEX_HH
#define ST_CHARGED_PION_VERTEX_HH

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