/***************************************************************************
 *
 * $Id: StZdcSummary.hh,v 1.2 1999/01/15 22:54:30 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSummary.hh,v $
 * Revision 1.2  1999/01/15 22:54:30  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StZdcSummary_hh
#define StZdcSummary_hh
#include "StEvent/StEnumerations.hh"

class StZdcSummary {
public:
    StZdcSummary();
    StZdcSummary(float ae, float aw, float s);
    ~StZdcSummary();
    // StZdcSummary(const StZdcSummary &right);
    // const StZdcSummary & operator=(const StZdcSummary &right);

    float adcSum() const;
    float adcSum(StBeamDirection) const;

    void setAdcSum(StBeamDirection, float); 
    void setAdcSum(float);     
    
protected:
    float mAdcSumEast;
    float mAdcSumWest;
    float mAdcSum;        
};

inline float StZdcSummary::adcSum() const { return mAdcSum; }

inline float StZdcSummary::adcSum(StBeamDirection dir) const
{
    if (dir == east)
	return mAdcSumEast;
    else
	return mAdcSumWest;
}

#endif
