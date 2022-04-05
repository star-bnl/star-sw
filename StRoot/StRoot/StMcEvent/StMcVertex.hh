/***************************************************************************
 *
 * $Id: StMcVertex.hh,v 2.13 2005/11/22 21:44:52 fisyak Exp $
 * $Log: StMcVertex.hh,v $
 * Revision 2.13  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.12  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.11  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
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
#include "StObject.h"
#include "TString.h"
#include "StThreeVectorF.hh"

class g2t_vertex_st;

class StMcVertex : public StObject {
public:
    StMcVertex();
    StMcVertex(g2t_vertex_st*);
    ~StMcVertex();
    
    int operator==(const StMcVertex&) const;
    int operator!=(const StMcVertex&) const;
    
    // "Get" Methods
    
  const StThreeVectorF&       position() const          { return *&mPosition;}
  StPtrVecMcTrack&            daughters()               { return *&mDaughters; } 
  const StPtrVecMcTrack&      daughters() const         { return *&mDaughters; } 
  unsigned int                numberOfDaughters()       { return mDaughters.size(); }
  unsigned int                numberOfDaughters() const { return mDaughters.size(); }
  StMcTrack*                  daughter(unsigned int i)  { return (i < mDaughters.size() ? mDaughters[i] : 0); }
  const StMcTrack*            daughter(unsigned int i) const { return (i < mDaughters.size() ? mDaughters[i] : 0); }
  const StMcTrack*            parent() const            { return mParent; }   
  TString const               &geantVolume() const      { return *&mGeantVolume; }
  float tof() const { return mTof; }  
  long geantProcess() const { return mGeantProcess; }      
  long geantMedium() const { return mGeantMedium; }      
  long generatorProcess() const { return mGeneratorProcess; }      
  long key() const { return mKey; }      
    
  // "Set" Methods
  
    void setPosition(const StThreeVectorF&);
    void setParent(StMcTrack* );         
    void addDaughter(StMcTrack*);
    void setGeantVolume(const Char_t *name);
    void setTof(float);
    void setGeantProcess(int); 
    void removeDaughter(StMcTrack*);
    virtual void Print(Option_t *option="") const; // *MENU* 
protected:
    
    StThreeVectorF       mPosition;
    StPtrVecMcTrack      mDaughters;
    StMcTrack*           mParent;
    TString              mGeantVolume;
    float                mTof;
    long                 mGeantProcess;
    long                 mGeneratorProcess;
    long                 mKey;
    long                 mGeantMedium;
    ClassDef(StMcVertex,1)
};
ostream&  operator<<(ostream& os, const StMcVertex&);
#endif
