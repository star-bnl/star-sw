/***************************************************************************
 *
 * $Id: StRun.h,v 2.1 2001/04/05 04:00:41 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRun.h,v $
 * Revision 2.1  2001/04/05 04:00:41  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
    int operator==(const StRun&) const;
    int operator!=(const StRun&) const;
    
    virtual int                  id() const;
    virtual int                  bfcId() const;
    virtual const TString&       type() const;
    virtual int                  triggerMask() const;
    virtual double               centerOfMassEnergy() const;
    virtual short                beamMassNumber(StBeamDirection) const;
    virtual short                beamCharge(StBeamDirection) const;
    virtual double               magneticField() const;
    virtual StRunSummary*        summary();
    virtual const StRunSummary*  summary() const;
    static const TString&        cvsTag();
    
    virtual void setId(int);
    virtual void setBfcId(int);
    virtual void setType(const char*);
    virtual void setTriggerMask(int);
    virtual void setCenterOfMassEnergy(double);
    virtual void setBeamMassNumber(StBeamDirection, short);
    virtual void setBeamCharge(StBeamDirection, short);
    virtual void setSummary(StRunSummary*);
    virtual void setMagneticField(double);
    
protected:
    TString         mType;
    Int_t           mId;           // experiment run ID
    Int_t           mBfcId;        // BFC production run ID
    Int_t           mTriggerMask;
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
