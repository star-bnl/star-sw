/*!
 * \class StTofData 
 * \author W.J. Llope, Sep 2001
 */
/***************************************************************************
 *
 * $Id: StTofData.h,v 2.3 2003/05/21 18:22:46 ullrich Exp $
 *
 * Author: W.J. Llope, Sep 2001
 ***************************************************************************
 *
 * Description: TOFp Systems raw data (TOFp+pVPD)
 *
 ***************************************************************************
 *
 * $Log: StTofData.h,v $
 * Revision 2.3  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.2  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2001/10/01 19:39:52  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofData_hh
#define StTofData_hh

#include "StObject.h"

class StTofData : public StObject {
public:
    StTofData();
    StTofData(unsigned short, unsigned short, unsigned short, short, unsigned short);
    ~StTofData();    

    int operator==(const StTofData&) const;
    int operator!=(const StTofData&) const;

    unsigned short  dataIndex() const;
    unsigned short  adc() const;
    unsigned short  tdc() const;
             short  tc() const;
    unsigned short  sc() const;
 
    void      setDataIndex(unsigned short);
    void      setAdc(unsigned short);
    void      setTdc(unsigned short);
    void      setTc(short);
    void      setSc(unsigned short);
    
protected:
    UShort_t  mDataIndex;
    UShort_t  mAdc;
    UShort_t  mTdc;
    Short_t   mTc;
    UShort_t  mSc;    

    ClassDef(StTofData,2)
};

inline void
StTofData::setDataIndex(unsigned short dataId)
{
    mDataIndex = dataId;
}

inline void
StTofData::setAdc(unsigned short rawAdc)
{
    mAdc = rawAdc;
}

inline void
StTofData::setTdc(unsigned short rawTdc)
{
    mTdc = rawTdc;
}

inline void
StTofData::setTc(short rawTc)
{
    mTc = rawTc;
}

inline void
StTofData::setSc(unsigned short rawSc)
{
    mSc = rawSc;
}

inline unsigned short
StTofData::dataIndex() const
{
    return mDataIndex;
}

inline unsigned short
StTofData::adc()  const
{
    return mAdc;
}

inline unsigned short
StTofData::tdc()  const
{
    return mTdc;
}

inline short
StTofData::tc()  const
{
    return mTc;
}

inline unsigned short
StTofData::sc()  const
{
    return mSc;
}

ostream& operator<<(ostream& os, const StTofData&);

#endif
