/***************************************************************************
 *
 * StMcVertex.hh
 *
 **************************************************************************/
#ifndef StMcVertex_hh
#define StMcVertex_hh
#include <string>
#include "StThreeVector.hh"
#include "StMcEvent/StMcTrackCollection.hh"
#include "tables/g2t_vertex.h"  // Need to include a header file for the table!!

class StMcTrack;

class StMcVertex {
public:
    StMcVertex();
    StMcVertex(float x, float y, float z);
    StMcVertex(g2t_vertex_st*);
    virtual ~StMcVertex();
    
    int operator==(const StMcVertex&) const;
    int operator!=(const StMcVertex&) const;

  // "Get" Methods

  
    StMcTrackCollection*        daughters();
    unsigned int                numberOfDaughters();
    StMcTrack*                  daughter(unsigned int);
    const StMcTrack*            parent();
    const StThreeVector<float>& position();
    string                      geantVolume();
    float                       tof();
    int                         geantProcess();

  // "Set" Methods
  

    void setParent(StMcTrack* );         
    void setPosition(const StThreeVector<float>&);       
    void addDaughter(StMcTrack*);
    void setGeantVolume(string);
    void setTof(float);
    void setGeantProcess(int); 

protected:
    

    StMcTrackCollection*   mDaughters;
    StMcTrack*             mParent;
    StThreeVector<float>   mPosition;
    string                 mGeantVolume;
    float                mTof;
    int                  mGeantProcess;
};



inline StMcTrackCollection* StMcVertex::daughters(){ return mDaughters; }       

inline unsigned int StMcVertex::numberOfDaughters() { return mDaughters->size(); }

inline StMcTrack* StMcVertex::daughter(unsigned int i)
{
    return (i < mDaughters->size() ? (*mDaughters)[i] : 0);
}

inline const StMcTrack* StMcVertex::parent(){ return mParent; }          

inline const StThreeVector<float>& StMcVertex::position(){ return mPosition; }        

inline string StMcVertex::geantVolume(){ return mGeantVolume; }   

inline float StMcVertex::tof(){ return mTof; }  

inline int StMcVertex::geantProcess(){ return mGeantProcess; }      

#endif
