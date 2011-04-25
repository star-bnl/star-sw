/*!
 * \class StMtdRawHit 
 */
/***************************************************************************
 *
 * $Id: StMtdRawHit.h,v 2.1 2011/04/25 21:24:02 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
***************************************************************************
 *
 * Description: MTD raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StMtdRawHit.h,v $
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMtdRawHit_hh
#define StMtdRawHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include <cstdlib>

class StMtdRawHit : public StObject {
public:
    StMtdRawHit();
    
    StMtdRawHit(char, unsigned char, unsigned char, unsigned int);
    ~StMtdRawHit();    
    
    int operator==(const StMtdRawHit&) const;
    int operator!=(const StMtdRawHit&) const;
    
    bool      leadingEdge() const;
    bool      trailingEdge() const;
    int       fiberId() const;
    int       flag() const;
    int       backleg() const;
    int       channel() const;
    unsigned int   tdc() const;
    
    void      setFlag(char);
    void      setBackleg(unsigned char);
    void      setChannel(unsigned char);
    void      setTdc(unsigned int);
    
protected:
    Char_t   mFlag;
    UChar_t  mBackLeg;
    UChar_t  mChannel;
    UInt_t   mTdc;
    
    ClassDef(StMtdRawHit,1)
};

ostream& operator<<(ostream&, const StMtdRawHit&); // Printing operator
inline void StMtdRawHit::setFlag(char iflag) { mFlag = iflag;}
inline void StMtdRawHit::setBackleg(unsigned char ibackleg) { mBackLeg = ibackleg;}
inline void StMtdRawHit::setChannel(unsigned char ichannel){ mChannel = ichannel;}
inline void StMtdRawHit::setTdc(unsigned int rawTdc){ mTdc = rawTdc;}
inline bool StMtdRawHit::leadingEdge() const { return (mFlag>0);}
inline bool StMtdRawHit::trailingEdge() const{ return (mFlag<0);}
inline  int StMtdRawHit::fiberId() const { return abs(static_cast<int>(mFlag)) - 1;}
inline  int StMtdRawHit::flag() const {return mFlag;}
inline  int StMtdRawHit::backleg()  const {return mBackLeg;}
inline  int StMtdRawHit::channel()  const{return mChannel;}
inline unsigned int StMtdRawHit::tdc()  const { return mTdc;}

#endif
