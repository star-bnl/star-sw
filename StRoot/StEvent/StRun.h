/***************************************************************************
 *
 * $Id: StRun.h,v 1.1 1999/01/30 03:58:07 fisyak Exp $
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
 * $Log: StRun.h,v $
 * Revision 1.1  1999/01/30 03:58:07  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/01/30 23:03:14  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:50  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TObject.h"
#endif
#define StRun_hh

#include "dst_run_header.h"
#include "dst_run_summary.h"
#include <TString.h>
#if !defined(ST_NO_NAMESPACES)
class StRun : public TObject {
#endif

class StRun : public St_DataSet {
    StRun(dst_run_header_st*, dst_run_summary_st*);
    virtual ~StRun();
    StRun(dst_run_header_st&, dst_run_summary_st&);
    StRun(dst_run_header_st&);
    
    Long_t          id() const;
    const TString& type() const;
    Long_t          triggerMask() const;
    Double_t        centerOfMassEnergy() const;
    Short_t         beamMassNumber(StBeamDirection) const;
    Short_t         beamCharge(StBeamDirection) const;
    StRunSummary* summary();
    virtual Short_t         beamMassNumber(StBeamDirection) const;
    void setId(Long_t);
    void setType(const Char_t*);
    void setTriggerMask(Long_t);
    void setCenterOfMassEnergy(Double_t);
    void setBeamMassNumber(StBeamDirection, Short_t);
    void setBeamCharge(StBeamDirection, Short_t);
    void setSummary(StRunSummary*);
    virtual void setBeamMassNumber(StBeamDirection, Short_t);
    virtual void setBeamCharge(StBeamDirection, Short_t);
    
    TString        mType;
    TString         mCVSTag;  
    Long_t          mId;
    TString         mType;
    Long_t          mTriggerMask;
    Double_t        mCenterOfMassEnergy;
    Short_t         mEastA;
    Short_t         mEastZ;
    Short_t         mWestA;
    Short_t         mWestZ;
    StRunSummary  *mSummary;
    
#ifdef __ROOT__
	ClassDef(StRun,1)  //StRun structure
#endif
    const StRun& operator=(const StRun&);
    StRun(const StRun&);
  ClassDef(StRun,1)  //StRun structure
};

inline Long_t StRun::id() const { return mId; }

inline const TString& StRun::type() const { return mType; }

inline Long_t StRun::triggerMask() const { return mTriggerMask; }

inline Double_t StRun::centerOfMassEnergy() const { return mCenterOfMassEnergy; }

inline Short_t StRun::beamMassNumber(StBeamDirection dir) const
{
    if (dir == east)
	return mEastA;
    else
	return mWestA;
}

inline Short_t StRun::beamCharge(StBeamDirection dir) const
{
    if (dir == east)
	return mEastZ;
    else
	return mWestZ;
}

inline StRunSummary* StRun::summary() { return mSummary; }

#endif
