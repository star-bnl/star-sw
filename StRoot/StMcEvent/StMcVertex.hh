/***************************************************************************
 *
 * $Id: StMcVertex.hh,v 2.10 2004/01/13 21:03:34 fisyak Exp $
 * $Log: StMcVertex.hh,v $
 * Revision 2.10  2004/01/13 21:03:34  fisyak
 * Replace iostream by Stiostream.h (for icc)
 *
 * Revision 2.9  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.8  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.7  2003/09/02 17:58:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.6  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.5  2000/03/29 16:15:55  calderon
 * Added more information from g2t_vertex table
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
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
#include "Stiostream.h"
#include <string>
#ifndef ST_NO_NAMESPACES
using std::string;
#endif

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
    const StMcTrack*            parent() const;
    string const               &geantVolume() const;
    float                       tof() const;
    long                        geantProcess() const;
    long                        generatorProcess() const;
    long                        key() const;
    long                        geantMedium() const; 
    
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
    long                 mGeneratorProcess;
    long                 mKey;
    long                 mGeantMedium;
};

ostream&  operator<<(ostream& os, const StMcVertex&);

inline const StThreeVectorF& StMcVertex::position() const { return mPosition;}

inline StPtrVecMcTrack& StMcVertex::daughters(){ return mDaughters; }       

inline unsigned int StMcVertex::numberOfDaughters() { return mDaughters.size(); }

inline StMcTrack* StMcVertex::daughter(unsigned int i)
{
    return (i < mDaughters.size() ? mDaughters[i] : 0);
}

inline const StMcTrack* StMcVertex::parent() const { return mParent; }          

inline string const &StMcVertex::geantVolume() const { return mGeantVolume; }   

inline float StMcVertex::tof() const { return mTof; }  

inline long StMcVertex::geantProcess() const { return mGeantProcess; }      

inline long StMcVertex::geantMedium() const { return mGeantMedium; }      

inline long StMcVertex::generatorProcess() const { return mGeneratorProcess; }      

inline long StMcVertex::key() const { return mKey; }      

#endif
