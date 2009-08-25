/*!
 * \class StBTofRawHit 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofRawHit.h,v 2.3 2009/08/25 15:41:28 fine Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description: TOF raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StBTofRawHit.h,v $
 * Revision 2.3  2009/08/25 15:41:28  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 2.2  2009/01/15 00:48:10  ullrich
 * mLeTeFlag changed to mFlag, tray(), module(), cell() now return int.
 *
 * Revision 2.1  2008/12/22 20:31:01  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StBTofRawHit_hh
#define StBTofRawHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include "TMath.h"

class StBTofRawHit : public StObject {
public:
    StBTofRawHit();

    StBTofRawHit(char, unsigned char, unsigned char, unsigned int);
    ~StBTofRawHit();    

    int operator==(const StBTofRawHit&) const;
    int operator!=(const StBTofRawHit&) const;

    bool      leadingEdge() const;
    bool      trailingEdge() const;
    int       fiberId() const;
    int       flag() const;
    int       tray() const;
    int       channel() const;
    unsigned int   tdc() const;

    void      setFlag(char);
    void      setTray(unsigned char);
    void      setChannel(unsigned char);
    void      setTdc(unsigned int);
    
protected:
    Char_t   mFlag;
    UChar_t  mTray;
    UChar_t  mChannel;
    UInt_t   mTdc;

    ClassDef(StBTofRawHit,1)
};

ostream& operator<<(ostream&, const StBTofRawHit&); // Printing operator

inline void
StBTofRawHit::setFlag(char iflag)
{
    mFlag = iflag;
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

inline bool
StBTofRawHit::leadingEdge() const
{
    return (mFlag>0);
}

inline bool
StBTofRawHit::trailingEdge() const
{
    return (mFlag<0);
}

inline int
StBTofRawHit::fiberId() const
{
    return TMath::Abs(mFlag) - 1;   //! fiber Id = 0, 1, 2, 3
}

inline int
StBTofRawHit::flag() const
{
    return mFlag;
}

inline int
StBTofRawHit::tray()  const
{
    return mTray;
}

inline int
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
