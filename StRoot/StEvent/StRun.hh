/***************************************************************************
 *
 * $Id: StRun.hh,v 1.3 1999/01/30 23:03:14 wenaus Exp $
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
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.4  1999/02/10 21:50:32  wenaus
 * Plug memory leaks
 *
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *

 * version with constructors for table-based loading
 *
using namespace std;
#ifndef StRun_hh
#define StRun_hh

#include "StEvent/StRunSummary.hh"
#include "StEvent/StEnumerations.hh"
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
    
    long          id() const;
    const string& type() const;
    long          triggerMask() const;
    double        centerOfMassEnergy() const;
    short         beamMassNumber(StBeamDirection) const;
    short         beamCharge(StBeamDirection) const;
    StRunSummary* summary();
    
    void setId(long);
    void setType(const char*);
    void setTriggerMask(long);
    void setCenterOfMassEnergy(double);
    void setBeamMassNumber(StBeamDirection, short);
    void setBeamCharge(StBeamDirection, short);
    void setSummary(StRunSummary*);
    
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
