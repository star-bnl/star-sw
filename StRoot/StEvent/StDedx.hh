/***************************************************************************
 *
 * $Id: StDedx.hh,v 1.3 1999/01/30 23:03:10 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 14/01/1999 T. Wenaus  Add table-based constructor
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedx.hh,v $
 * Revision 1.3  1999/01/30 23:03:10  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:53:32  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StDedx_hh
#define StDedx_hh

#include "tables/dst_dedx.h"

class StDedx {
public:
    StDedx();
    ~StDedx();
    StDedx(dst_dedx_st*);
    // StDedx(const StDedx &right);      use default
    // const StDedx & operator=(const StDedx &right);
    
    unsigned short numberOfPointsUsed() const;
    float          mean() const;
    float          variance() const;
    unsigned long  status() const;

    void setNumberOfPointsUsed(unsigned short);
    void setMean(float);
    void setVariance(float);
    void setStatus(unsigned long);
    
protected:
    unsigned short mNumberOfPointsUsed;
    float          mMean;
    float          mVariance;
    unsigned long  mStatus;              
};

inline unsigned short StDedx::numberOfPointsUsed() const { return mNumberOfPointsUsed; }

inline float StDedx::mean() const { return mMean; }

inline float StDedx::variance() const { return mVariance; }

inline unsigned long StDedx::status() const { return mStatus; }


#endif
