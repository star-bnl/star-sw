/***************************************************************************
 *
 * $Id: StMcVertex.hh,v 2.1 1999/11/19 19:06:34 calderon Exp $
 * $Log: StMcVertex.hh,v $
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:17  calderon
 * Completely revised for new StEvent
 *
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
#include "StMcContainers.hh"
#include <string>
#include "StDbUtilities/StGlobalCoordinate.hh"

class g2t_vertex_st;

class StMcVertex : public StGlobalCoordinate {
public:
    StMcVertex();
    StMcVertex(float x, float y, float z);
    StMcVertex(g2t_vertex_st*);
    ~StMcVertex();
    
    int operator==(const StMcVertex&) const;
    int operator!=(const StMcVertex&) const;

  // "Get" Methods

  
    StPtrVecMcTrack&            daughters();
    unsigned int                numberOfDaughters();
    StMcTrack*                  daughter(unsigned int);
    const StMcTrack*            parent();
    string                      geantVolume();
    float                       tof();
    long                        geantProcess();

  // "Set" Methods
  

    void setParent(StMcTrack* );         
    void addDaughter(StMcTrack*);
    void setGeantVolume(string);
    void setTof(float);
    void setGeantProcess(int); 
    void removeDaughter(StMcTrack*);
    
protected:
    

    StPtrVecMcTrack      mDaughters;
    StMcTrack*           mParent;
    string               mGeantVolume;
    float                mTof;
    long                 mGeantProcess;
};



inline StPtrVecMcTrack& StMcVertex::daughters(){ return mDaughters; }       

inline unsigned int StMcVertex::numberOfDaughters() { return mDaughters.size(); }

inline StMcTrack* StMcVertex::daughter(unsigned int i)
{
    return (i < mDaughters.size() ? mDaughters[i] : 0);
}

inline const StMcTrack* StMcVertex::parent(){ return mParent; }          

inline string StMcVertex::geantVolume(){ return mGeantVolume; }   

inline float StMcVertex::tof(){ return mTof; }  

inline long StMcVertex::geantProcess(){ return mGeantProcess; }      

#endif
