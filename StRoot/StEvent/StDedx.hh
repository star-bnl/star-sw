/***************************************************************************
 *
 * $Id: StDedx.hh,v 1.1 1999/01/15 20:39:40 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedx.hh,v $
 * Revision 1.1  1999/01/15 20:39:40  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StDedx_hh
#define StDedx_hh

class StDedx {
public:
    StDedx();
    ~StDedx();
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
