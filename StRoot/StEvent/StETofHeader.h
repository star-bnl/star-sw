/***************************************************************************
 *
 * $Id: StETofHeader.h,v 2.2 2019/08/01 22:52:19 smirnovd Exp $
 *
 * Author: Pengfei Lyu, April 2018
 ***************************************************************************
 *
 * Description: This class stores ETofHeader information from the DAQ stream,
 * e.g. trigger time and STAR time (reset time stamp of the bTOF clock)
 * and other event-wise information
 *
 ***************************************************************************
 *
 * $Log: StETofHeader.h,v $
 * Revision 2.2  2019/08/01 22:52:19  smirnovd
 * Add non-c++ include defining uint64_t for rootcint
 *
 * rootcint is not c++11 aware and therefore cannot deal with the standard c++
 * header <cstdint>
 *
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#ifndef STETOFHEADER_H
#define STETOFHEADER_H

#include <stdint.h>
#include <map>


#include <Stiostream.h>
#include "StObject.h"

class StETofHeader : public StObject {
public:

    StETofHeader();
    StETofHeader( const double&, const double&, const map< unsigned int, uint64_t >&, const map< unsigned int, uint64_t >& ,
                  const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t& );

    ~StETofHeader();

    double    trgGdpbFullTime()   const;
    double    trgStarFullTime()   const;

    map< unsigned int, uint64_t > rocGdpbTs()  const;
    map< unsigned int, uint64_t > rocStarTs()  const;

    unsigned int      starToken()         const;
    unsigned int      starDaqCmdIn()      const;
    unsigned int      starTrgCmdIn()      const;
    uint64_t          eventStatusFlag()   const;


    void    setTrgGdpbFullTime( const double& gdpbFullTime );
    void    setTrgStarFullTime( const double& starFullTime );

    void    setRocGdpbTs( const map< unsigned int, uint64_t >& gdpbTs );
    void    setRocStarTs( const map< unsigned int, uint64_t >& starTs );

    void    setStarToken(    const unsigned int& token    );
    void    setStarDaqCmdIn( const unsigned int& daqCmdIn );
    void    setStarTrgCmdIn( const unsigned int& trgCmdIn );

    void    setEventStatusFlag( const uint64_t& statusFlag );

private:
    Double_t    mTrgGdpbFullTime;
    Double_t    mTrgStarFullTime;
    
    map< UInt_t, ULong64_t > mRocGdpbTs;
    map< UInt_t, ULong64_t > mRocStarTs;
    
    UInt_t      mStarToken;
    UInt_t      mStarDaqCmdIn;
    UInt_t      mStarTrgCmdIn;
    
    ULong64_t   mEventStatusFlag;


    ClassDef( StETofHeader, 1 )
};

#endif // STETOFHEADER_H
