/***************************************************************************
 *
 * $Id: StRun.hh,v 1.7 1999/03/23 21:47:41 ullrich Exp $
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
 * $Log: StRun.hh,v $
 * Revision 1.7  1999/03/23 21:47:41  ullrich
 * Member function made virtual
 *
 * Revision 1.6  1999/03/04 18:17:10  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.5  1999/03/04 15:56:59  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.4  1999/02/10 21:50:32  wenaus
 * Plug memory leaks
 *
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:50  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StRun_hh
#define StRun_hh

#include "StEvent/StRunSummary.hh"
#include "StEvent/StEnumerations.hh"
#include "tables/dst_run_header.h"
#include "tables/dst_run_summary.h"
#include <string>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StRun {
public:
    StRun();
    virtual ~StRun();
    StRun(dst_run_header_st&, dst_run_summary_st&);
    StRun(dst_run_header_st&);
    
    int operator==(const StRun&) const;
    int operator!=(const StRun&) const;
    
    virtual long          id() const;
    virtual const string& type() const;
    virtual long          triggerMask() const;
    virtual double        centerOfMassEnergy() const;
    virtual short         beamMassNumber(StBeamDirection) const;
    virtual short         beamCharge(StBeamDirection) const;
    virtual StRunSummary* summary();
    
    virtual void setId(long);
    virtual void setType(const char*);
    virtual void setTriggerMask(long);
    virtual void setCenterOfMassEnergy(double);
    virtual void setBeamMassNumber(StBeamDirection, short);
    virtual void setBeamCharge(StBeamDirection, short);
    virtual void setSummary(StRunSummary*);
    
protected:
    long          mId;
    string        mType;
    long          mTriggerMask;
    double        mCenterOfMassEnergy;
    short         mEastA;
    short         mEastZ;
    short         mWestA;
    short         mWestZ;
    StRunSummary  *mSummary;
    
private:
    const StRun& operator=(const StRun&);
    StRun(const StRun&);
};

inline long StRun::id() const { return mId; }

inline const string& StRun::type() const { return mType; }

inline long StRun::triggerMask() const { return mTriggerMask; }

inline double StRun::centerOfMassEnergy() const { return mCenterOfMassEnergy; }

inline short StRun::beamMassNumber(StBeamDirection dir) const
{
    if (dir == east)
	return mEastA;
    else
	return mWestA;
}

inline short StRun::beamCharge(StBeamDirection dir) const
{
    if (dir == east)
	return mEastZ;
    else
	return mWestZ;
}

inline StRunSummary* StRun::summary() { return mSummary; }

#endif
