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
    /** 
     ** @brief default constructor for pre-2020 data. No missmatch information available. Used in StEtofDigiMaker to initialise the header.
     **/
    StETofHeader( const double&, const double&, const map< unsigned int, uint64_t >&, const map< unsigned int, uint64_t >& ,
                  const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t& );
    /** 
     ** @brief default constructor for post-2020 data. Include missmatch information from FEE. Used in StEtofDigiMaker to initialise the header.
     **/              
    StETofHeader( const double&, const double&, const map< unsigned int, uint64_t >&, const map< unsigned int, uint64_t >& ,
                  const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t&, const std::vector<bool>&  );
    /** 
     ** @brief Full constructor including goodEventFlag, which is normally set in calibrations only.
     **/                
    StETofHeader( const double&, const double&, const map< unsigned int, uint64_t >&, const map< unsigned int, uint64_t >& ,
                  const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t&, const std::vector<bool>&, const std::vector<bool>&  );              

    ~StETofHeader();

    double    trgGdpbFullTime()   const;
    double    trgStarFullTime()   const;

    map< unsigned int, uint64_t > rocGdpbTs()  const;
    map< unsigned int, uint64_t > rocStarTs()  const;

    unsigned int      starToken()         const;
    unsigned int      starDaqCmdIn()      const;
    unsigned int      starTrgCmdIn()      const;
    uint64_t          eventStatusFlag()   const;    
    /** 
     ** @brief Flag for each Get4 TDC to mark if it is available in this event.
     **/     
    std::vector<bool>       missMatchFlagVec()  const;
    /** 
     ** @brief Flag to mark if the event is good for physics analysis for each counter. A counter is considered good in each event when there are zero missmatch flags set and pulser digis on both sides are found. In this case, the counter should perform at its best. Counter efficiency should be constant between good events. 
     **/      
    std::vector<bool>       goodEventFlagVec()  const;


    void    setTrgGdpbFullTime( const double& gdpbFullTime );
    void    setTrgStarFullTime( const double& starFullTime );

    void    setRocGdpbTs( const map< unsigned int, uint64_t >& gdpbTs );
    void    setRocStarTs( const map< unsigned int, uint64_t >& starTs );

    void    setStarToken(    const unsigned int& token    );
    void    setStarDaqCmdIn( const unsigned int& daqCmdIn );
    void    setStarTrgCmdIn( const unsigned int& trgCmdIn );

    void    setEventStatusFlag( const uint64_t& statusFlag );
    void    setGoodEventFlagVec( const std::vector<bool>& FlagVec );
    void    setGoodEventFlagVec( int blubb ) {return;}
    //    void    setGoodEventFlagVec( const std::vector<bool>& FlagVec );

private:
    Double_t    mTrgGdpbFullTime;
    Double_t    mTrgStarFullTime;
    
    map< UInt_t, ULong64_t > mRocGdpbTs;
    map< UInt_t, ULong64_t > mRocStarTs;
    
    UInt_t      mStarToken;
    UInt_t      mStarDaqCmdIn;
    UInt_t      mStarTrgCmdIn;
    
    ULong64_t   mEventStatusFlag;

    std::vector< Bool_t > mMissMatchFlagVec;
    std::vector< Bool_t > mGoodEventFlagVec; 

    ClassDef( StETofHeader, 3 )
};

#endif // STETOFHEADER_H
