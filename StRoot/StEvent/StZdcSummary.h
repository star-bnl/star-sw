/***************************************************************************
 *
 * $Id: StZdcSummary.h,v 1.2 1999/02/09 19:52:35 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSummary.h,v $
 * Revision 1.2  1999/02/09 19:52:35  fisyak
 * Import new Torre staff
 *
 * Revision 1.4  1999/04/28 22:27:41  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:30  wenaus
 * version with constructors for table-based loading
 *
#ifdef __ROOT__
#include "TObject.h"
#endif
#ifndef StZdcSummary_hh
#define StZdcSummary_hh
class StZdcSummary : public TObject {
#include "StEnumerations.h"

class StZdcSummary : public StObject {
public:
    StZdcSummary();
    StZdcSummary(Float_t ae, Float_t aw, Float_t s);
    ~StZdcSummary();
    // StZdcSummary(const StZdcSummary &right);
    // const StZdcSummary & operator=(const StZdcSummary &right);

    Float_t adcSum() const;
    Float_t adcSum(StBeamDirection) const;

    void setAdcSum(StBeamDirection, Float_t); 
    void setAdcSum(Float_t);     
    
protected:
#ifdef __ROOT__
	ClassDef(StZdcSummary,1)  //StZdcSummary structure
#endif
    Float_t mAdcSumWest;
    Float_t mAdcSum;        
  ClassDef(StZdcSummary,1)  //StZdcSummary structure
};

inline Float_t StZdcSummary::adcSum() const { return mAdcSum; }

inline Float_t StZdcSummary::adcSum(StBeamDirection dir) const
{
    if (dir == east)
	return mAdcSumEast;
    else
	return mAdcSumWest;
}

#endif
