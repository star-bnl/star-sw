/***************************************************************************
 *
 * $Id: StETofQAMaker.h,v 1.1 2018/07/25 14:38:06 jeromel Exp $
 *
 * Author: Philipp Weidenkaff & Pengfei Lyu, May 2018
 ***************************************************************************
 *
 * Description: StETofQAMaker - class to read the eTofCollection from
 * StEvent build QA histograms. 
 *
 ***************************************************************************
 *
 * $Log: StETofQAMaker.h,v $
 * Revision 1.1  2018/07/25 14:38:06  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFQAMAKER_H
#define STETOFQAMAKER_H

#include <string>

#include "StMaker.h"

class TH2;

class StEvent;
class StETofCollection;
class StETofDigi;
class StETofHit;
class StBTofCollection;


class StETofQAMaker: public StMaker {
public:
    /// Default constructor
    StETofQAMaker( const char* name = "etofQa" );

    ~StETofQAMaker();

    Int_t  Init();
    Int_t  InitRun( Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    StETofCollection* GetETofCollection();
    StBTofCollection* GetBTofCollection();

    void   calcTStart();

    // virtual const char *GetCVS() const
    // {static const char cvs[]="Tag $Name:  $ $Id: StETofQAMaker.h,v 1.1 2018/07/25 14:38:06 jeromel Exp $ built "__DATE__" " __TIME__ ; return cvs;}

private:
    // internal subfunctions----------------------------------------------------------------------------------------------------

    void createHistos();
    void fillHistos(); 
    void writeHistos(); 

    StEvent*            mEvent;
    StETofCollection*   mETofCollection;
    StBTofCollection* 	mBTofCollection;

    double              mTStart;

    std::string         mOutHistFileName;   // name of output histogram root file

    double              mTimeOffset;        // temporary offset for time of flight


    std::vector< std::vector< std::vector< std::vector< std::vector< StETofDigi* > > > > > mStorStDigi; //[mSector][mZPlane][mDet][mStrip][nDigis]

    // store hit ordered by detector.
    std::vector< std::vector< std::vector< std::vector< StETofHit* > > > > mStorStHit; //[mSector][mZPlane][mDet][nDigis]

    // Histograms----------------------------------------------------------------------------------------------------

    std::vector< std::vector< std::vector< TH2* > > > mHistRpcCluSize;      //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH2* > > > mHistRpcCluPosition;  //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH2* > > > mHistRpcCluTOff;      //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH2* > > > mHistRpcCluTot;       //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH2* > > > mHistRpcDigiTot;      //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH2* > > > mHistRpcCluAvWalk;    //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH1* > > > mHistRpcCluMul;       //[mSector][mZPlane][mDet]
    std::vector< std::vector< std::vector< TH1* > > > mHistHitTrigTimeDet;  //[mSector][mZPlane][mDet]

    TH1* mHistBTofAvTimeDiff;

    TH1* mHistHitTrigTimeDiff;
    TH1* mHistDigiTrigTimeDiff;
    TH1* mHistDigiRawTrigTimeDiff;
    TH2* mHistBTofETofMul;
    TH2* mHistBTofAvTimeDiffvETofMul;
    TH2* mHistBTofAvTimeDiffvBTofMul;

    TH1* mHistETofTimeOfFlight;
    TH1* mHistBTofTimeOfFlight;

    ClassDef( StETofQAMaker, 1 )
};

#endif
