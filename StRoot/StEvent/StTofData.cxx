/***************************************************************************
 *
 * $Id: StTofData.cxx,v 2.2 2003/05/21 18:22:46 ullrich Exp $
 *
 * Author: W.J. Llope, Sep 2001
 ***************************************************************************
 *
 * Description: TOFp Systems raw data (TOFp+pVPD) 
 *
 ***************************************************************************
 *
 * $Log: StTofData.cxx,v $
 * Revision 2.2  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.1  2001/10/01 19:39:52  ullrich
 * Initial Revision.
 *
 **************************************************************************/

#include "StTofData.h"

static const char rcsid[] = "$Id: StTofData.cxx,v 2.2 2003/05/21 18:22:46 ullrich Exp $";

ClassImp(StTofData)

StTofData::StTofData()
    : mDataIndex(0), mAdc(0), mTdc(0), mTc(0), mSc(0)
{ /* noop */ }

StTofData::StTofData(unsigned short dataId, unsigned short rawAdc,
		     unsigned short rawTdc, short rawTc, unsigned short rawSc)
   : mDataIndex(dataId), mAdc(rawAdc), mTdc(rawTdc), mTc(rawTc), mSc(rawSc)
{ /* noop */ }

StTofData::~StTofData() { /* noop */ }
    
int StTofData::operator==(const StTofData& p) const
{
    return (p.mDataIndex == mDataIndex &&
            p.mAdc == mAdc && p.mTdc == mTdc &&
            p.mTc == mTc && p.mSc == mSc );
}

int StTofData::operator!=(const StTofData& p) const
{
    return !(*this == p);  // use operator==()
}

ostream& operator<<(ostream& os, const StTofData& d)
{
    return (os << "StTofData::> "<< " ID=" << d.dataIndex()
                                 << ", adc=" << d.adc()
                                 << ", tdc=" << d.tdc()
                                 << ", tc=" << d.tc()
                                 << ", sc=" << d.sc());
}

