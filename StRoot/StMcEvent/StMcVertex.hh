/***************************************************************************
 *
 * $Id: StMcVertex.hh,v 1.4 1999/09/24 01:23:17 fisyak Exp $
 * $Log: StMcVertex.hh,v $
 * Revision 1.4  1999/09/24 01:23:17  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:55  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcVertex_hh
#define StMcVertex_hh
#include <string>
#include "tables/St_g2t_vertex_Table.h"  // Need to include a header file for the table!!

class StThreeVectorF;
class StMcTrack;
#include "StMcTrackCollection.hh"

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
    const StThreeVectorF&       position();
    string                      geantVolume();
    float                       tof();
    int                         geantProcess();

  // "Set" Methods
  

    void setParent(StMcTrack* );         
    void setPosition(const StThreeVectorF&);       
    void addDaughter(StMcTrack*);
    void setGeantVolume(string);
    void setTof(float);
    void setGeantProcess(int); 

protected:
    

    StMcTrackCollection* mDaughters;
    StMcTrack*           mParent;
    StThreeVectorF       mPosition;
    string               mGeantVolume;
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

inline const StThreeVectorF& StMcVertex::position(){ return mPosition; }        

inline string StMcVertex::geantVolume(){ return mGeantVolume; }   

inline float StMcVertex::tof(){ return mTof; }  

inline int StMcVertex::geantProcess(){ return mGeantProcess; }      

#endif
