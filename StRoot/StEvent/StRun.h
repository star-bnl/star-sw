/***************************************************************************
 *
 * $Id: StRun.h,v 1.7 1999/09/24 01:23:01 fisyak Exp $
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
 * Revision 1.7  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/24 01:23:01  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/05/04 20:59:26  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.5  1999/04/30 13:16:28  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
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

#include "St_DataSet.h"
#include "StRunSummary.h"
#include "StEnumerations.h"
#include "dst_run_header.h"
#include "dst_run_summary.h"
#include "TString.h"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StRun : public St_DataSet {
public:
    StRun();
    virtual ~StRun();
    StRun(dst_run_header_st&, dst_run_summary_st&);
    StRun(dst_run_header_st&);
    
    Int_t operator==(const StRun&) const;
    Int_t operator!=(const StRun&) const;
    
    const char   *CVSTag(){return (const char *) mCVSTag;};
    virtual Long_t          id() const;
    virtual const TString& type() const;
    virtual Long_t          triggerMask() const;
    virtual Double_t        centerOfMassEnergy() const;
    virtual Short_t         beamMassNumber(StBeamDirection) const;
    virtual Short_t         beamCharge(StBeamDirection) const;
    virtual StRunSummary* summary();
    
    virtual void setId(Long_t);
    virtual void setType(const Char_t*);
    virtual void setTriggerMask(Long_t);
    virtual void setCenterOfMassEnergy(Double_t);
    virtual void setBeamMassNumber(StBeamDirection, Short_t);
    virtual void setBeamCharge(StBeamDirection, Short_t);
    virtual void setSummary(StRunSummary*);
    
protected:
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
    
private:
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
