/***************************************************************************
 *
 * $Id: StL0Trigger.h,v 1.5 1999/04/30 13:16:28 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.h,v $
 * Revision 1.5  1999/04/30 13:16:28  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:28  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:33  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.5  1999/03/04 18:17:06  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.4  1999/03/04 15:56:59  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/02/09 22:38:22  wenaus
 * add missing 'inline'
 *
 * Revision 1.2  1999/01/15 22:53:47  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StL0Trigger_hh
#define StL0Trigger_hh 
#include "StObject.h"
#include "StTrigger.h"
#ifndef __ROOT__
#include <vector>
#endif
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
#ifndef __ROOT__ 
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Long_t, allocator<Long_t> > VecLong;
#else
typedef vector<Long_t> VecLong;
#endif
#endif
class StL0Trigger : public StTrigger {
public:
    StL0Trigger();
    ~StL0Trigger();
    // StL0Trigger(const StL0Trigger&);    use default
    // const StL0Trigger & operator=(const StL0Trigger&);
#ifndef __ROOT__    
    VecLong&      coarsePixelArray();
#endif
    Long_t          mwcCtbMultiplicity() const;
    Long_t          mwcCtbDipole() const;
    Long_t          mwcCtbTopology() const;
    Long_t          mwcCtbMoment() const;

    void setMwcCtbMultiplicity(Long_t);    
    void setMwcCtbDipole(Long_t);              
    void setMwcCtbTopology(Long_t);            
    void setMwcCtbMoment(Long_t);
    
protected:
#ifndef __ROOT__
    VecLong      mCoarsePixelArray;
#endif
    Long_t         mMwcCtbMultiplicity;
    Long_t         mMwcCtbDipole;
    Long_t         mMwcCtbTopology;
    Long_t         mMwcCtbMoment;           
  ClassDef(StL0Trigger,1)  //StL0Trigger structure
};
#ifndef __ROOT__
inline VecLong& StL0Trigger::coarsePixelArray() { return mCoarsePixelArray;}
#endif

inline Long_t StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

inline Long_t StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

inline Long_t StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

inline Long_t StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}
#endif
