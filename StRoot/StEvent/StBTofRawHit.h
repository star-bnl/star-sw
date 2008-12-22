/*!
 * \class StBTofRawHit 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofRawHit.h,v 2.1 2008/12/22 20:31:01 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description: TOF raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StBTofRawHit.h,v $
 * Revision 2.1  2008/12/22 20:31:01  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StBTofRawHit_hh
#define StBTofRawHit_hh

#include <Stiostream.h>
#include "StObject.h"

class StBTofRawHit : public StObject {
public:
    StBTofRawHit();

    StBTofRawHit(unsigned char, unsigned char, unsigned char, unsigned int);
    ~StBTofRawHit();    

    int operator==(const StBTofRawHit&) const;
    int operator!=(const StBTofRawHit&) const;

    unsigned char  leteFlag() const; //! 1 - leading; 2 - trailing
    unsigned char  tray() const;
    unsigned char  channel() const;
    unsigned int   tdc() const;

    void      setLeTeFlag(unsigned char);
    void      setTray(unsigned char);
    void      setChannel(unsigned char);
    void      setTdc(unsigned int);
    
protected:
    UChar_t  mLeTeFlag;
    UChar_t  mTray;
    UChar_t  mChannel;
    UInt_t   mTdc;

    ClassDef(StBTofRawHit,1)
};

ostream& operator<<(ostream&, const StBTofRawHit&); // Printing operator

inline void
StBTofRawHit::setLeTeFlag(unsigned char iflag)
{
    mLeTeFlag = iflag;
}

inline void
StBTofRawHit::setTray(unsigned char itray)
{
    mTray = itray;
}

inline void
StBTofRawHit::setChannel(unsigned char ichannel)
{
    mChannel = ichannel;
}

inline void
StBTofRawHit::setTdc(unsigned int rawTdc)
{
    mTdc = rawTdc;
}

inline unsigned char
StBTofRawHit::leteFlag() const
{
    return mLeTeFlag;
}

inline unsigned char
StBTofRawHit::tray()  const
{
    return mTray;
}

inline unsigned char
StBTofRawHit::channel()  const
{
    return mChannel;
}

inline unsigned int
StBTofRawHit::tdc()  const
{
    return mTdc;
}

#endif
