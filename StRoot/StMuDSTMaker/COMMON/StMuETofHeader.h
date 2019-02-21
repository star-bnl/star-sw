/***************************************************************************
 *
 * $Id: StMuETofHeader.h,v 1.1 2019/02/21 13:32:54 jdb Exp $
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
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/
#ifndef STMUETOFHEADER_H
#define STMUETOFHEADER_H

#include <map>

#include "TObject.h"

class StETofHeader;


class StMuETofHeader : public TObject {
public:

    StMuETofHeader();
    StMuETofHeader( const StETofHeader* header );
    StMuETofHeader( const double&, const double&, const std::map< unsigned int, uint64_t >&, const std::map< unsigned int, uint64_t >& ,
                    const unsigned int&, const unsigned int&, const unsigned int&, const uint64_t& );

    ~StMuETofHeader();

    double    trgGdpbFullTime()   const;
    double    trgStarFullTime()   const;

    std::map< unsigned int, uint64_t > rocGdpbTs()  const;
    std::map< unsigned int, uint64_t > rocStarTs()  const;

    unsigned int      starToken()         const;
    unsigned int      starDaqCmdIn()      const;
    unsigned int      starTrgCmdIn()      const;
    uint64_t          eventStatusFlag()   const;


    void    setTrgGdpbFullTime( const double& gdpbFullTime );
    void    setTrgStarFullTime( const double& starFullTime );

    void    setRocGdpbTs( const std::map< unsigned int, uint64_t >& gdpbTs );
    void    setRocStarTs( const std::map< unsigned int, uint64_t >& starTs );

    void    setStarToken(    const unsigned int& token    );
    void    setStarDaqCmdIn( const unsigned int& daqCmdIn );
    void    setStarTrgCmdIn( const unsigned int& trgCmdIn );

    void    setEventStatusFlag( const uint64_t& statusFlag );

private:
    Double_t    mTrgGdpbFullTime;
    Double_t    mTrgStarFullTime;
    
    std::map< UInt_t, ULong64_t > mRocGdpbTs;
    std::map< UInt_t, ULong64_t > mRocStarTs;
    
    UInt_t      mStarToken;
    UInt_t      mStarDaqCmdIn;
    UInt_t      mStarTrgCmdIn;
    
    ULong64_t   mEventStatusFlag;


    ClassDef( StMuETofHeader, 1 )
};

#endif // STMUETOFHEADER_H
