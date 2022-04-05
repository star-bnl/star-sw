/***************************************************************************
 *
 * $Id: StETofDigiMaker.h,v 1.4 2021/05/10 10:45:13 weidenkaff Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofDigiMaker - class to fill the StEvent from DAQ reader:
 * unpack raw data & save StETofHeader & StETofDigis in StETofCollection 
 *
 ***************************************************************************
 *
 * $Log: StETofDigiMaker.h,v $
 * Revision 1.4  2021/05/10 10:45:13  weidenkaff
 * Added unpacking of status pattern to be written to Event header. PW.StETofDigiMaker.cxx
 *
 * Revision 1.3  2019/02/19 20:32:05  fseck
 * update for unpacking year 2019+ daq files
 *
 * Revision 1.2  2018/07/27 13:58:08  fseck
 * small change to compile also in 64bit mode
 *
 * Revision 1.1  2018/07/25 14:39:40  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFDIGIMAKER_H
#define STETOFDIGIMAKER_H

#include <stdint.h>
#include <string>
#include <vector>
#include "StRTSBaseMaker.h"

class StEvent;
class StETofCollection;
class StETofDigi;
class StETofHardwareMap;

namespace gdpb { class FullMessage; };
namespace gdpbv100 { class FullMessage; };

class StETofDigiMaker: public StRTSBaseMaker {
public:
    /// Default constructor
    StETofDigiMaker( const char* name = "etofDigi" );

    ~StETofDigiMaker();


    Int_t  Init();
    Int_t  InitRun( Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    StETofCollection* getETofCollection();

    void   setDebug( const bool debug );

    void   checkEvent();


    // methods for processing the data in 2019+
    void   processEvent( uint64_t* messageBuffer, size_t nFullMessagesToRead );
    
    void   convertTriggerMessages( vector< gdpbv100::FullMessage >& triggerMessages,
                                   map< unsigned int, uint64_t >& gdpbTs,
                                   map< unsigned int, uint64_t >& starTs );

    void   fillETofHeader( uint64_t* messageBuff, vector< gdpbv100::FullMessage >& triggerMessages );
    void   fillETofDigi( gdpbv100::FullMessage& mess );

    void setFileNameElectronicsMap(     const char* fileName );

    // methods for processing the data in 2018
    void   processEvent2018( uint64_t* messageBuffer, size_t nFullMessagesToRead );

    void   convertTriggerMessages( vector< gdpb::FullMessage >& triggerMessages,
                                   map< unsigned int, uint64_t >& gdpbTs,
                                   map< unsigned int, uint64_t >& starTs );

    void   fillETofHeader( uint64_t* messageBuff, vector< gdpb::FullMessage >& triggerMessages );
    void   fillETofDigi( gdpb::FullMessage& mess );


private:
    StEvent*             mEvent;
    StETofCollection*    mETofCollection;
    Int_t                mRunYear;

	 vector < Bool_t > 			 mMissMatchFlagVec;
    StETofHardwareMap*   		 mHwMap;         // electronic channel to hardware/geometry map
    std::string   				 mFileNameElectronicsMap;      // name of parameter file for electronics-to-hardware map
    std::map< Int_t, Bool_t >  mGet4ActiveMap;               // map of get4Id that are actually mapped to existing channels.

    Bool_t               mDebug;

    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }

    ClassDef( StETofDigiMaker, 1 )
};

inline void StETofDigiMaker::setDebug( const bool debug )  { mDebug = debug; }

#endif // STETOFDIGIMAKER_H
