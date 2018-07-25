/***************************************************************************
 *
 * $Id: StETofDigiMaker.h,v 1.1 2018/07/25 14:39:40 jeromel Exp $
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
 * Revision 1.1  2018/07/25 14:39:40  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFDIGIMAKER_H
#define STETOFDIGIMAKER_H


#include "StRTSBaseMaker.h"

// format of the roc_get4v2 messages
#include "StETofUtil/StETofMessageFormat.h"

class StEvent;
class StETofCollection;
class StETofDigi;

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

    StETofCollection* GetETofCollection();

    void   convertTriggerMessages( vector< gdpb::FullMessage >& triggerMessages,
                                   map< UInt_t, ULong64_t >& gdpbTs,
                                   map< UInt_t, ULong64_t >& starTs );

    void   fillETofHeader( ULong64_t* longBuff, vector< gdpb::FullMessage >& triggerMessages );
    void   fillETofDigi( gdpb::FullMessage& mess );
    void   fillEvent();

    //virtual const char *GetCVS() const
    //{static const char cvs[]="Tag $Name:  $ $Id: StETofDigiMaker.h,v 1.1 2018/07/25 14:39:40 jeromel Exp $ built "__DATE__" " __TIME__ ; return cvs;}

private:
    StEvent*             mEvent;
    StETofCollection*    mETofCollection;
    
    ClassDef( StETofDigiMaker, 1 )
};

#endif // STETOFDIGIMAKER_H
