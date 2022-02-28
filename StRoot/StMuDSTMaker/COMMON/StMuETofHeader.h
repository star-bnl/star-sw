/***************************************************************************
 *
 * $Id: StMuETofHeader.h,v 1.2 2021/05/11 19:40:43 jdb Exp $
 *
 * Author: Florian Seck, November 2018
 ***************************************************************************
 *
 * Description: This class stores ETofHeader information from the DAQ stream,
 * e.g. trigger time and STAR time (reset time stamp of the bTOF clock)
 * and other event-wise information in MuDsts
 *
 ***************************************************************************
 *
 * $Log: StMuETofHeader.h,v $
 * Revision 1.2  2021/05/11 19:40:43  jdb
 * StETofHeader update from philipp W. modified classes to include the front-end missmatch pattern
 *
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/
#ifndef STMUETOFHEADER_H
#define STMUETOFHEADER_H

#include <map>
#include <vector>

#include "TObject.h"

class StETofHeader;


class StMuETofHeader : public TObject {
public:

    StMuETofHeader();
     /** 
     ** @brief Constructor for conversion from StEvent header.
     **/    
    StMuETofHeader( const StETofHeader* header );
     /** 
     ** @brief default constructor for pre-2020 data. No missmatch information available. Used in StEtofDigiMaker to initialise the header.
     **/
    StMuETofHeader( const double&, const double&, const std::map< unsigned int, uint64_t >&, const std::map< unsigned int, uint64_t >& ,
                    const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t& );
     /** 
     ** @brief default constructor for post-2020 data. Include missmatch information from FEE. Used in StEtofDigiMaker to initialise the header.
     **/                
    StMuETofHeader( const double&, const double&, const std::map< unsigned int, uint64_t >&, const std::map< unsigned int, uint64_t >& ,
                    const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t&, const std::vector< Bool_t >& );
     /** 
     ** @brief Full constructor including goodEventFlag, which is normally set in calibrations only.
     **/                
    StMuETofHeader( const double&, const double&, const std::map< unsigned int, uint64_t >&, const std::map< unsigned int, uint64_t >& ,
                  const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t&, const std::vector<bool>&, 
                  const std::vector<bool>&  );                     

    ~StMuETofHeader();

    virtual void Clear( Option_t *opt = "" ){
        // ensure allocations are destroyed
        mRocGdpbTs.clear();
        mRocStarTs.clear();
        mMissMatchFlagVec.clear();
        mGoodEventFlagVec.clear();
        mMissMatchFlagVec.shrink_to_fit();
        mGoodEventFlagVec.shrink_to_fit();
    }

    double    trgGdpbFullTime()   const;
    double    trgStarFullTime()   const;

    std::map< unsigned int, uint64_t > rocGdpbTs()  const;
    std::map< unsigned int, uint64_t > rocStarTs()  const;

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

    void    setRocGdpbTs( const std::map< unsigned int, uint64_t >& gdpbTs );
    void    setRocStarTs( const std::map< unsigned int, uint64_t >& starTs );

    void    setStarToken(    const unsigned int& token    );
    void    setStarDaqCmdIn( const unsigned int& daqCmdIn );
    void    setStarTrgCmdIn( const unsigned int& trgCmdIn );

    void    setEventStatusFlag( const uint64_t& statusFlag );
    void    setGoodEventFlagVec( const std::vector<bool>& FlagVec );

private:
    Double_t    mTrgGdpbFullTime;
    Double_t    mTrgStarFullTime;
    
    std::map< UInt_t, ULong64_t > mRocGdpbTs;
    std::map< UInt_t, ULong64_t > mRocStarTs;
    
    UInt_t      mStarToken;
    UInt_t      mStarDaqCmdIn;
    UInt_t      mStarTrgCmdIn;
    
    ULong64_t   mEventStatusFlag;

	 std::vector< Bool_t > mMissMatchFlagVec;
	 std::vector< Bool_t > mGoodEventFlagVec;  

    ClassDef( StMuETofHeader, 3 )
};

#endif // STMUETOFHEADER_H
