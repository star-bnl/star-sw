/***************************************************************************
 *
 * $Id: StETofHeader.h,v 2.3 2021/03/19 19:56:50 ullrich Exp $
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
 * Revision 2.3  2021/03/19 19:56:50  ullrich
 * include the front-end missmatch pattern (Philipp)
 *
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
#include <vector>


#include <Stiostream.h>
#include "StObject.h"

class StETofHeader : public StObject {
public:
 
    StETofHeader();
    StETofHeader( const Double_t& trgGdpbTime, const Double_t& trgStarTime,
		  const map< UInt_t, ULong64_t >& gdpbTs, const map< UInt_t, ULong64_t >& starTs,
		  const UInt_t& starToken, const UInt_t& starDaqCmdIn, const UInt_t& starTrgCmdIn,
		  const ULong64_t& eventStatusFlag );
    StETofHeader( const Double_t& trgGdpbTime, const Double_t& trgStarTime,
		  const map< UInt_t, ULong64_t >& gdpbTs, const map< UInt_t, ULong64_t >& starTs,
		  const UInt_t& starToken, const UInt_t& starDaqCmdIn, const UInt_t& starTrgCmdIn,
		  const ULong64_t& eventStatusFlag, const vector<Bool_t>& MissMatchFlagVec );

    ~StETofHeader();

    Double_t    trgGdpbFullTime()   const;
    Double_t    trgStarFullTime()   const;

    map< UInt_t, ULong64_t > rocGdpbTs()  const;
    map< UInt_t, ULong64_t > rocStarTs()  const;

    UInt_t      starToken()         const;
    UInt_t      starDaqCmdIn()      const;
    UInt_t      starTrgCmdIn()      const;
    ULong64_t   eventStatusFlag()   const;
    
    vector<Bool_t>       missMatchFlagVec()  const;


    void    setTrgGdpbFullTime( const Double_t& gdpbFullTime );
    void    setTrgStarFullTime( const Double_t& starFullTime );

    void    setRocGdpbTs( const map< UInt_t, ULong64_t >& gdpbTs );
    void    setRocStarTs( const map< UInt_t, ULong64_t >& starTs );

    void    setStarToken(    const UInt_t& token    );
    void    setStarDaqCmdIn( const UInt_t& daqCmdIn );
    void    setStarTrgCmdIn( const unsigned int& trgCmdIn );

    void    setEventStatusFlag( const ULong64_t& statusFlag );

private:
    Double_t    mTrgGdpbFullTime;
    Double_t    mTrgStarFullTime;
    
    map< UInt_t, ULong64_t > mRocGdpbTs;
    map< UInt_t, ULong64_t > mRocStarTs;
    
    UInt_t      mStarToken;
    UInt_t      mStarDaqCmdIn;
    UInt_t      mStarTrgCmdIn;
    
    ULong64_t   mEventStatusFlag;

    vector< Bool_t > mMissMatchFlagVec; 

    ClassDef( StETofHeader, 2 )
};

#endif // STETOFHEADER_H
