/***************************************************************************
 *
 * $Id: StMcVertex.hh,v 2.3 1999/12/14 07:04:50 calderon Exp $
 * $Log: StMcVertex.hh,v $
 * Revision 2.3  1999/12/14 07:04:50  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:53  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
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
#include "StThreeVectorF.hh"

class g2t_vertex_st;

class StMcVertex {
public:
    StMcVertex();
    StMcVertex(g2t_vertex_st*);
    ~StMcVertex();
    
    int operator==(const StMcVertex&) const;
    int operator!=(const StMcVertex&) const;
    
    // "Get" Methods
    
    const StThreeVectorF&       position() const;
    StPtrVecMcTrack&            daughters();
    unsigned int                numberOfDaughters();
    StMcTrack*                  daughter(unsigned int);
    const StMcTrack*            parent();
    string                      geantVolume() const;
    float                       tof() const;
    long                        geantProcess() const;

  // "Set" Methods
  
    void setPosition(const StThreeVectorF&);
    void setParent(StMcTrack* );         
    void addDaughter(StMcTrack*);
    void setGeantVolume(string);
    void setTof(float);
    void setGeantProcess(int); 
    void removeDaughter(StMcTrack*);
    
protected:
    
    StThreeVectorF       mPosition;
    StPtrVecMcTrack      mDaughters;
    StMcTrack*           mParent;
    string               mGeantVolume;
    float                mTof;
    long                 mGeantProcess;
};

ostream&  operator<<(ostream& os, const StMcVertex&);

inline const StThreeVectorF& StMcVertex::position() const { return mPosition;}

inline StPtrVecMcTrack& StMcVertex::daughters(){ return mDaughters; }       

inline unsigned int StMcVertex::numberOfDaughters() { return mDaughters.size(); }

inline StMcTrack* StMcVertex::daughter(unsigned int i)
{
    return (i < mDaughters.size() ? mDaughters[i] : 0);
}

inline const StMcTrack* StMcVertex::parent(){ return mParent; }          

inline string StMcVertex::geantVolume() const { return mGeantVolume; }   

inline float StMcVertex::tof() const { return mTof; }  

inline long StMcVertex::geantProcess() const { return mGeantProcess; }      

#endif
