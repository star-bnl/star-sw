/***************************************************************************
 *
 * $Id: StVertex.hh,v 1.3 1999/01/27 12:53:39 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.hh,v $
 * Revision 1.3  1999/01/27 12:53:39  ullrich
 * Made setType() virtual. See StV0Vertex for reason.
 *
 * Revision 1.4  1999/01/30 23:03:18  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/27 12:53:39  ullrich
 * Made setType() virtual. See StV0Vertex for reason.
 *
 * Revision 1.2  1999/01/15 22:54:22  wenaus
 * version with constructors for table-based loading
 *
#include "StTables/dst_vertex.h"
#ifndef StVertex_hh
#define StVertex_hh
#include "StEvent/StEnumerations.hh"
#include "StThreeVector.hh"
#include "StEvent/StVecPtrGlobalTrack.hh"
#include "tables/dst_vertex.h"

class StVertex {
public:
    StVertex();
    StVertex(dst_vertex_st*);
    // StVertex(const StVertex&);       use default
    // const StVertex & operator=(const StVertex&);
    virtual ~StVertex();
    
    int operator==(const StVertex&) const;
    int operator!=(const StVertex&) const;
    StVecPtrGlobalTrack&        daughters();
    unsigned int                numberOfDaughters();
    const StGlobalTrack*        parent();
    const StThreeVector<float>& position();
    const StThreeVector<float>& positionError();
    unsigned long               qualityBitmask();
    float                       chiSquared();         
    long                        index() {return mIndex;};

    void setParent(StGlobalTrack* );         
    void setPosition(const StThreeVector<float>&);       
    void setQualityBitmask(unsigned long); 
    void setChiSquared(float);     
    void setIndex(long ii) {mIndex = ii;};

protected:
    unsigned long          mIndex;
    StVertexType           mType;
    StVecPtrGlobalTrack    mDaughters;
    StGlobalTrack*         mParent;
    StThreeVector<float>   mPosition;
    StThreeVector<float>   mPositionError;
    unsigned long          mQualityBitmask;
    float                  mChiSquared;                
};

{
    return (i < mDaughters.size() ? mDaughters[i] : 0);
}

inline const StGlobalTrack* StVertex::parent(){ return mParent; }          

inline const StThreeVector<float>& StVertex::position(){ return mPosition; }        

inline const StThreeVector<float>& StVertex::positionError(){ return mPositionError; }   

inline unsigned long StVertex::qualityBitmask(){ return mQualityBitmask; }  

inline float StVertex::chiSquared(){ return mChiSquared; }      

#endif
