/***************************************************************************
 *
 * $Id: StRun.h,v 2.0 1999/10/12 18:42:32 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRun.h,v $
 * Revision 2.0  1999/10/12 18:42:32  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StRun_hh
#define StRun_hh

#include "St_DataSet.h"
#include "StEnumerations.h"
#include "TString.h"

class StRunSummary;
class run_header_st;
class dst_run_summary_st;

class StRun : public St_DataSet {
public:
    StRun();
    StRun(const run_header_st&, const dst_run_summary_st&);
    StRun(const run_header_st&);
    virtual ~StRun();
    
    Int_t operator==(const StRun&) const;
    Int_t operator!=(const StRun&) const;
    
    virtual Long_t               id() const;
    virtual Long_t               bfcId() const;
    virtual const TString&       type() const;
    virtual Long_t               triggerMask() const;
    virtual Double_t             centerOfMassEnergy() const;
    virtual Short_t              beamMassNumber(StBeamDirection) const;
    virtual Short_t              beamCharge(StBeamDirection) const;
    virtual Double_t             magneticField() const;
    virtual StRunSummary*        summary();
    virtual const StRunSummary*  summary() const;
    static const TString&        cvsTag();
    
    virtual void setId(Long_t);
    virtual void setBfcId(Long_t);
    virtual void setType(const Char_t*);
    virtual void setTriggerMask(Long_t);
    virtual void setCenterOfMassEnergy(Double_t);
    virtual void setBeamMassNumber(StBeamDirection, Short_t);
    virtual void setBeamCharge(StBeamDirection, Short_t);
    virtual void setSummary(StRunSummary*);
    virtual void setMagneticField(Double_t);
    
protected:
    TString         mType;
    Long_t          mId;           // experiment run ID
    Long_t          mBfcId;        // BFC production run ID
    Long_t          mTriggerMask;
    Short_t         mEastA;
    Short_t         mEastZ;
    Short_t         mWestA;
    Short_t         mWestZ;
    Double_t        mCenterOfMassEnergy;
    Double_t        mMagneticFieldZ;
    
    StRunSummary    *mSummary;
    static TString  mCvsTag;
    
private:
    StRun& operator=(const StRun&);
    StRun(const StRun&);
    void initFromTable(const run_header_st&);
    
    ClassDef(StRun,1)
};
#endif
