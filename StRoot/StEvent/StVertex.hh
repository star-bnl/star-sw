/***************************************************************************
 *
 * $Id: StVertex.hh,v 1.6 1999/03/23 21:47:45 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 01/30/1999 T. Wenaus  Add index method to allow indexed access when in
 *                       a list
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.hh,v $
 * Revision 1.6  1999/03/23 21:47:45  ullrich
 * Member function made virtual
 *
 * Revision 1.5  1999/02/17 11:04:52  ullrich
 * Added numberOfDaughters() and daughter(i) methods.
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
 **************************************************************************/
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

    virtual StVertexType                type();
    virtual StVecPtrGlobalTrack&        daughters();
    virtual unsigned int                numberOfDaughters();
    virtual StGlobalTrack*              daughter(unsigned int);
    virtual const StGlobalTrack*        parent();
    virtual const StThreeVector<float>& position();
    virtual const StThreeVector<float>& positionError();
    virtual unsigned long               qualityBitmask();
    virtual float                       chiSquared();         
    virtual long                        index() {return mIndex;};

    virtual void setType(StVertexType);           
    virtual void setParent(StGlobalTrack* );         
    virtual void setPosition(const StThreeVector<float>&);       
    virtual void setPositionError(const StThreeVector<float>&);  
    virtual void setQualityBitmask(unsigned long); 
    virtual void setChiSquared(float);     
    virtual void setIndex(long ii) {mIndex = ii;};

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

inline StVertexType StVertex::type() { return mType; }

inline StVecPtrGlobalTrack& StVertex::daughters(){ return mDaughters; }       

inline unsigned int StVertex::numberOfDaughters() { return mDaughters.size(); }

inline StGlobalTrack* StVertex::daughter(unsigned int i)
{
    return (i < mDaughters.size() ? mDaughters[i] : 0);
}

inline const StGlobalTrack* StVertex::parent(){ return mParent; }          

inline const StThreeVector<float>& StVertex::position(){ return mPosition; }        

inline const StThreeVector<float>& StVertex::positionError(){ return mPositionError; }   

inline unsigned long StVertex::qualityBitmask(){ return mQualityBitmask; }  

inline float StVertex::chiSquared(){ return mChiSquared; }      

#endif
