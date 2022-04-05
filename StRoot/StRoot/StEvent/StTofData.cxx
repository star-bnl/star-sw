/***************************************************************************
 *
 * $Id: StTofData.cxx,v 2.3 2005/04/11 22:35:25 calderon Exp $
 *
 * Author: W.J. Llope, Sep 2001
 ***************************************************************************
 *
 * Description: TOFp Systems raw data (TOFp+pVPD) 
 *
 ***************************************************************************
 *
 * $Log: StTofData.cxx,v $
 * Revision 2.3  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 * Revision 2.2  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.1  2001/10/01 19:39:52  ullrich
 * Initial Revision.
 *
 **************************************************************************/

#include "StTofData.h"

static const char rcsid[] = "$Id: StTofData.cxx,v 2.3 2005/04/11 22:35:25 calderon Exp $";

ClassImp(StTofData)

StTofData::StTofData()
  : mDataIndex(0), mAdc(0), mTdc(0), mTc(0), mSc(0), mLeadingTdc(0), mTrailingTdc(0)
{ /* noop */ }

StTofData::StTofData(unsigned short dataId, unsigned short rawAdc,
		     unsigned short rawTdc, short rawTc, unsigned short rawSc, 
		     unsigned int rawLTdc, unsigned int rawTTdc)
  : mDataIndex(dataId), mAdc(rawAdc), mTdc(rawTdc), mTc(rawTc), mSc(rawSc), 
    mLeadingTdc(rawLTdc), mTrailingTdc(rawTTdc)
{ /* noop */ }

StTofData::~StTofData() { /* noop */ }
    
int StTofData::operator==(const StTofData& p) const
{
    return (p.mDataIndex == mDataIndex &&
            p.mAdc == mAdc && p.mTdc == mTdc &&
            p.mTc == mTc && p.mSc == mSc &&
	    p.mLeadingTdc == mLeadingTdc && p.mTrailingTdc == mTrailingTdc);
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
    //	    << ", leading tdc =" << d.leadingTdc()
    //	    << ", trailing tdc =" << d.trailingTdc());
}

