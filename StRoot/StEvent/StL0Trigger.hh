/***************************************************************************
 *
 * $Id: StL0Trigger.hh,v 1.4 1999/03/04 15:56:59 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.hh,v $
 * Revision 1.4  1999/03/04 15:56:59  wenaus
 * add std namespace for Sun CC5 compatibility
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
#include "StEvent/StTrigger.hh"
using namespace std;
#include <vector>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<long, allocator<long> > VecLong;
#else
typedef vector<long> VecLong;
#endif

class StL0Trigger : public StTrigger {
public:
    StL0Trigger();
    ~StL0Trigger();
    // StL0Trigger(const StL0Trigger&);    use default
    // const StL0Trigger & operator=(const StL0Trigger&);
    
    VecLong&      coarsePixelArray();
    long          mwcCtbMultiplicity() const;
    long          mwcCtbDipole() const;
    long          mwcCtbTopology() const;
    long          mwcCtbMoment() const;

    void setMwcCtbMultiplicity(long);    
    void setMwcCtbDipole(long);              
    void setMwcCtbTopology(long);            
    void setMwcCtbMoment(long);
    
protected:
    VecLong      mCoarsePixelArray;
    long         mMwcCtbMultiplicity;
    long         mMwcCtbDipole;
    long         mMwcCtbTopology;
    long         mMwcCtbMoment;           
};

inline VecLong& StL0Trigger::coarsePixelArray() { return mCoarsePixelArray;}

inline long StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

inline long StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

inline long StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

inline long StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}

#endif
