/*!
 * \class StTofRawData 
 * \author Xin Dong, Feb 2005
 */
/***************************************************************************
 *
 * $Id: StTofRawData.h,v 2.3 2008/02/26 20:01:58 ullrich Exp $
 *
 * Author: Xin Dong, Feb 2005
 ***************************************************************************
 *
 * Description: TOF raw hits (from Run 5)
 *
 ***************************************************************************
 *
 * $Log: StTofRawData.h,v $
 * Revision 2.3  2008/02/26 20:01:58  ullrich
 * Added new member mTriggerrime and related methods.
 *
 * Revision 2.2  2007/11/21 00:28:32  ullrich
 * Added new data member mTray plus access functions. New overloaded constructor.
 *
 * Revision 2.1  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 *
 **************************************************************************/
#ifndef StTofRawData_hh
#define StTofRawData_hh

#include "StObject.h"

class StTofRawData : public StObject {
public:
    StTofRawData();

    StTofRawData(unsigned short, unsigned short, unsigned int, unsigned short);
    StTofRawData(unsigned short, unsigned short, unsigned short, unsigned int, unsigned short);
    StTofRawData(unsigned short, unsigned short, unsigned short, unsigned int, unsigned int, unsigned short);
    ~StTofRawData();    

    int operator==(const StTofRawData&) const;
    int operator!=(const StTofRawData&) const;

    unsigned short  leteFlag() const; // 1 - leading; 2 - trailing
    unsigned short  tray() const;
    unsigned short  channel() const;
    unsigned int    tdc() const;
    unsigned int    triggertime() const;
    unsigned short  quality() const;

    void      setLeTeFlag(unsigned short);
    void      setTray(unsigned short);
    void      setChannel(unsigned short);
    void      setTdc(unsigned int);
    void      setTriggertime(unsigned int);
    void      setQuality(unsigned short);
    
protected:
    UShort_t  mLeTeFlag;
    UShort_t  mTray;
    UShort_t  mChannel;
    UInt_t    mTdc;
    UInt_t    mTriggertime;
    UShort_t  mQuality;

    ClassDef(StTofRawData,3)
};

inline void
StTofRawData::setLeTeFlag(unsigned short iflag)
{
    mLeTeFlag = iflag;
}

inline void
StTofRawData::setTray(unsigned short itray)
{
    mTray = itray;
}

inline void
StTofRawData::setChannel(unsigned short ichannel)
{
    mChannel = ichannel;
}

inline void
StTofRawData::setTdc(unsigned int rawTdc)
{
    mTdc = rawTdc;
}

inline void
StTofRawData::setTriggertime(unsigned int rawtriggertime)
{
    mTriggertime = rawtriggertime;
}

inline void
StTofRawData::setQuality(unsigned short quality)
{
    mQuality = quality;
}

inline unsigned short
StTofRawData::leteFlag() const
{
    return mLeTeFlag;
}

inline unsigned short
StTofRawData::tray()  const
{
    return mTray;
}

inline unsigned short
StTofRawData::channel()  const
{
    return mChannel;
}

inline unsigned int
StTofRawData::tdc()  const
{
    return mTdc;
}

inline unsigned int
StTofRawData::triggertime()  const
{
    return mTriggertime;
}

inline unsigned short
StTofRawData::quality()  const
{
    return mQuality;
}

#endif
