/***************************************************************************
 *
 * $Id: StETofQAMaker.cxx,v 1.1 2018/07/25 14:38:06 jeromel Exp $
 *
 * Author: Philipp Weidenkaff & Pengfei Lyu, May 2018
 ***************************************************************************
 *
 * Description: StETofQAMaker - class to read the eTofCollection from
 * StEvent build QA histograms. 
 *
 ***************************************************************************
 *
 * $Log: StETofQAMaker.cxx,v $
 * Revision 1.1  2018/07/25 14:38:06  jeromel
 * Peer reviewed Raghav+Jerome - code from Florian Seck
 *
 *
 ***************************************************************************/
#include <vector>
#include <algorithm>
#include <cmath>

#include "TFile.h"
#include "TH2.h"

#include "StChain/StChainOpt.h" // for renaming the histogram file

#include "StEvent/StEvent.h"

#include "StEvent/StETofCollection.h"
#include "StEvent/StETofDigi.h"
#include "StEvent/StETofHit.h"

#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHit.h"
#include "StEvent/StBTofHeader.h"

#include "StETofQAMaker.h"


const Int_t uNbSectors      = 12;
const Int_t uNbZPlanes      =  3;
const Int_t uNbDetPerModule =  3;
const Int_t uNbStripsPerDet = 32;

const Double_t dStripPitch  = 1.0; //[cm]

const Double_t dBTofTimeRange = 51200; //[ns]

const Double_t dCoarseClockCycleNs = 6.25; //[ns]

//const Double_t dETofTrigOffset = 1958; //[ns]	// run 42
const Double_t dETofTrigOffset = -9047; //[ns]	// run 44


//_____________________________________________________________
StETofQAMaker::StETofQAMaker( const char* name )
: StMaker( "etofQa", name ),
  mEvent( 0 ),              /// pointer to StEvent
  mETofCollection( 0 ),     /// pointer to StETofCollection
  mBTofCollection( 0 ),     /// pointer to StBTofCollection
  mTStart( 0 ),
  mOutHistFileName( "" ),
  mTimeOffset( 0 )
{                                            
    LOG_DEBUG << "StETofQAMaker::ctor"  << endm;
}

//_____________________________________________________________
StETofQAMaker::~StETofQAMaker()
{ /* no op */ 

}

//_____________________________________________________________
/*!
 * This method is to obtain the ETofCollection from StEvent.
 * If StEvent is in the chain, retrieve it;
 * if no StEvent in the chain, a new StEvent is created.
 */
StETofCollection*
StETofQAMaker::GetETofCollection()
{
    /// get StEvent if any at once
    StETofCollection* etofCollection = 0;
    mEvent = dynamic_cast< StEvent* > ( GetInputDS( "StEvent" ) );

    if ( mEvent ) {
        etofCollection = mEvent->etofCollection();

        /// need to create the eTof collection if it's not there
        /// and give it to StEvent
        if ( !etofCollection )  {
            ///  Save the eTof collection to StEvent
            LOG_INFO << "StETofQAMaker::GetETofCollection - making new StETofCollection and giving it to StEvent" << endm;
            etofCollection = new StETofCollection();
            mEvent->setETofCollection( etofCollection );
        }
        else {
            LOG_INFO << "StETofQAMaker::GetETofCollection - StEvent already has a StETofCollection - not making a new one" << endm;
        }
    }
    else {
        LOG_WARN << "No StEvent found by StETofQAMaker::GetETofCollection" << endm;
    }

    return etofCollection;
}

//_____________________________________________________________
/*!
 * This method is to obtain the ETofCollection from StEvent.
 * If StEvent is in the chain, retrieve it;
 * if no StEvent in the chain, a new StEvent is created.
 */
StBTofCollection*
StETofQAMaker::GetBTofCollection()
{
    /// get StEvent if any at once
    StBTofCollection* btofCollection = 0;
    mEvent = dynamic_cast< StEvent* > ( GetInputDS( "StEvent" ) );

    if ( mEvent ) {
        btofCollection = mEvent->btofCollection();

        /// need to create the eTof collection if it's not there
        /// and give it to StEvent
        if ( !btofCollection )  {
            ///  Save the eTof collection to StEvent
            LOG_INFO << "StETofQAMaker::GetBTofCollection - no BTofCollection found!" << endm;				
            btofCollection = new StBTofCollection();
            mEvent->setBTofCollection( btofCollection );
        }
        else {
            LOG_INFO << "StETofQAMaker::GetBTofCollection - StEvent already has a StBTofCollection - not making a new one" << endm;
        }
    }
    else {
        LOG_WARN << "No StEvent found by StETofQAMaker::GetBTofCollection" << endm;
    }

    return btofCollection;
}

//_____________________________________________________________
Int_t
StETofQAMaker::Init()
{
    //Setting up storage
    mStorStDigi.clear();
    mStorStDigi.resize( uNbSectors ); //number of sectors
    mStorStHit.clear();
    mStorStHit.resize( uNbSectors );

    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {
        mStorStDigi[ iSector ].resize( uNbZPlanes ); //number of ZPlanes
        mStorStHit[  iSector ].resize( uNbZPlanes );
        
        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {
            mStorStDigi[ iSector ][ iPlane ].resize( uNbDetPerModule ); //number of Detectors
            mStorStHit[  iSector ][ iPlane ].resize( uNbDetPerModule ); 
            //detectors resized when filled

        for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {
                mStorStDigi[ iSector ][ iPlane ][ iDet ].resize( uNbStripsPerDet ); //number of Strips
                LOG_DEBUG << "StETofQAMaker::Init::Detectors" << endm;
            }
        }
    }

    //TODO: Add Null-pointer handling for collection
    LOG_INFO << "StETofQAMaker::Init" << endm;

    createHistos();

    return kStOk;
}


//_____________________________________________________________
Int_t
StETofQAMaker::InitRun( Int_t runnumber )
{ 
    return kStOk;
}

//_____________________________________________________________
Int_t
StETofQAMaker::FinishRun( Int_t runnumber )
{
    return kStOk;
}

//_____________________________________________________________
Int_t
StETofQAMaker::Finish()
{ 
    writeHistos(); 

    return kStOk;
}

//_____________________________________________________________
Int_t
StETofQAMaker::Make()
{ 
    LOG_INFO << "StETofQAMaker::Make(): starting..." << endm;

    //---------------------------------
    mETofCollection = GetETofCollection();
    LOG_INFO << "StETofQAMaker::Make() - getting the eTOF collection " << mETofCollection      << endm;
    LOG_INFO << "StETofQAMaker::Make() - nDigis = " << ( mETofCollection->etofDigis() ).size() << endm;
    LOG_INFO << "StETofQAMaker::Make() - nHits  = " << ( mETofCollection->etofHits()  ).size() << endm;
    //---------------------------------

    //---------------------------------
    mBTofCollection = GetBTofCollection();
    LOG_INFO << "StETofQAMaker::Make() - getting the bTOF collection " << mBTofCollection   << endm;
    LOG_INFO << "StETofQAMaker::Make() - nHits = " << ( mBTofCollection->tofHits() ).size() << endm;
    //---------------------------------

    calcTStart();

    fillHistos();

    return kStOk;
}

//_____________________________________________________________
void
StETofQAMaker::calcTStart()
{
    LOG_INFO << "StETofQAMaker::calcTStart():  -- loading Vpd data from bTof header" << endm;

    if( !mBTofCollection ) return;

    StBTofHeader* bTofHeader = mBTofCollection->tofHeader();

    if( !bTofHeader ) {
        LOG_INFO << "StETofQAMaker::calcTStart():  -- no bTof header. Skip start time calculation." << endm;
        return;
    }


    const int nVpd = 19; // number of VPD tubes on each side of STAR

    double tSumWest = 0.;
    double tSumEast = 0.;

    int nWest = bTofHeader->numberOfVpdHits( west );
    int nEast = bTofHeader->numberOfVpdHits( east );

    double vpdLeTime[ 2 * nVpd ];

    // check if bTof header is filled with useful information
    if( fabs( bTofHeader->vpdVz() - ( -9999. ) ) < 0.1 ) {
        LOG_INFO << " no valid Vpd data in the bTOF header " << endm;
        return;
    }
    else {
        LOG_INFO << "Vpd Vertex is at: " << bTofHeader->vpdVz() << endm;
    }

    // west side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i ] = bTofHeader->vpdTime( west, i+1 );
        if( vpdLeTime[ i ] > 0. ) {
            tSumWest += vpdLeTime[ i ];
            LOG_INFO << " loading VPD west tubeId = " << i+1 << " time " << vpdLeTime[ i ] << endm;
        }
    }

    // east side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i + nVpd ] = bTofHeader->vpdTime( east, i+1 );
        if( vpdLeTime[ i + nVpd ] > 0. ) {
            tSumEast += vpdLeTime[ i + nVpd ];
            LOG_INFO << " loading VPD east tubeId = " << i+1 << " time " << vpdLeTime[ i + nVpd ] << endm;
        }
    }


    LOG_INFO << "StETofQAMaker::calcTStart():  --  calculating Vpd start time" << endm;

    double tSum = tSumWest + tSumEast;
    
    if( nWest + nEast ) {
        mTStart = tSum / ( nWest + nEast );
    }
    LOG_INFO << "StETofQAMaker::calcTStart():  --  Vpd start time: " << mTStart << endm;

    return;
}

//_____________________________________________________________
void
StETofQAMaker::createHistos()
{
    mHistRpcCluSize.clear(); //Clustersize distribution for each strip of the detector
    mHistRpcCluSize.resize( uNbSectors );

    mHistRpcCluPosition.clear(); //Position distribution of the clusters in the X-Y plane of the detector
    mHistRpcCluPosition.resize( uNbSectors );

    mHistRpcCluTOff.clear(); //Time offset distribution for each strip of the detector
    mHistRpcCluTOff.resize( uNbSectors );

    mHistRpcCluTot.clear(); //TOT distribution for each strip of the detector
    mHistRpcCluTot.resize( uNbSectors );

    mHistRpcDigiTot.clear(); //TOT distribution on digi level
    mHistRpcDigiTot.resize( uNbSectors );

    mHistRpcCluAvWalk.clear(); //TOT versus detector arrival time minus event timezero
    mHistRpcCluAvWalk.resize( uNbSectors );

    mHistRpcCluMul.clear(); //Multiplicity distribution in each event on the detector
    mHistRpcCluMul.resize( uNbSectors );

    mHistHitTrigTimeDet.clear();
    mHistHitTrigTimeDet.resize( uNbSectors );

    mHistBTofAvTimeDiff = new TH1F( 
                Form( "Av_TDiff_ETof_BTof" ),
                Form( "Difference between average hits times in Events between BTof and ETof; dT [ns]; Events []" ),
                102400, -51200, 51200 ); 

    mHistHitTrigTimeDiff= new TH1F( 
                Form( "Hit_TDiff_ETof_Trg" ),
                Form( "Difference between hits on ETof and the trigger token; dT [ns]; Events []" ),
                10240, -51200, 51200 ); 

    mHistDigiTrigTimeDiff= new TH1F( 
                Form( "Digi_TDiff_ETof_Trg" ),
                Form( "Difference between Digis on ETof and the trigger token; dT [ns]; Events []" ),
                10240, -51200, 51200 ); 

    mHistDigiRawTrigTimeDiff= new TH1F( 
                Form( "DigiRaw_TDiff_ETof_Trg" ),
                Form( "Raw time difference between Digis on ETof and the trigger token; dT [ns]; Events []" ),
                10240, -51200, 51200 ); 

    mHistBTofETofMul = new TH2F( 
                Form( "Mul_ETof_BTof" ),
                Form( "Multiplicity correlation between ETof and BTof; ETof Multiplicity []; BTof Multiplicity []" ),
                51, -0.5, 50.5,
                101, -0.5, 500.5 );

    mHistBTofAvTimeDiffvETofMul= new TH2F( 
                Form( "Av_TDiff_v_Mul_ETof" ),
                Form( "Difference between average hits times in Events between BTof and ETof vs ETof multiplicity; dT [ns]; ETof Multiplicity []" ),
                10240, -51200, 51200,
                51, -0.5, 50.5 );

    mHistBTofAvTimeDiffvBTofMul= new TH2F( 
                Form( "Av_TDiff_v_Mul_BTof" ),
                Form( "Difference between average hits times in Events between BTof and ETof vs ETof multiplicity; dT [ns]; BTof Multiplicity []" ),
                10240, -51200, 51200,
                101, -0.5, 500.5 );


    mHistETofTimeOfFlight = new TH1F( "ETof_Time_of_Flight", "time of flight for eTof hits; Tof [ns]; # ETof hits", 1000,  300, 600 );
    mHistBTofTimeOfFlight = new TH1F( "BTof_Time_of_Flight", "time of flight for bTof hits; Tof [ns]; # BTof hits", 1000, -150, 150 );



    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {
        mHistRpcCluSize[     iSector ].resize( uNbZPlanes ); //number of ZPlanes
        mHistRpcCluPosition[ iSector ].resize( uNbZPlanes );
        mHistRpcCluTOff[     iSector ].resize( uNbZPlanes );
        mHistRpcCluTot[      iSector ].resize( uNbZPlanes );
        mHistRpcDigiTot[     iSector ].resize( uNbZPlanes );
        mHistRpcCluAvWalk[   iSector ].resize( uNbZPlanes );
        mHistRpcCluMul[      iSector ].resize( uNbZPlanes );
        mHistHitTrigTimeDet[ iSector ].resize( uNbSectors );

        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {
            mHistRpcCluSize[     iSector ][ iPlane ].resize( uNbDetPerModule ); //number of Detectors
            mHistRpcCluPosition[ iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistRpcCluTOff[     iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistRpcCluTot[      iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistRpcDigiTot[     iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistRpcCluAvWalk[   iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistRpcCluMul[      iSector ][ iPlane ].resize( uNbDetPerModule );
            mHistHitTrigTimeDet[ iSector ][ iPlane ].resize( uNbSectors );

            for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {
                Double_t tSumMax=2;

                mHistRpcCluSize[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_Size", iSector+13, iPlane, iDet ),
                        Form( "Cluster size of Det #%02d in ZPlane %d under Sector %02d; Strip []; size [strips]", iDet, iPlane, iSector+13 ),
                        32, 0, 32, 16, 0.5, 16.5 );
                 
                mHistRpcCluPosition[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_Pos", iSector+13, iPlane, iDet ),
                        Form( "Cluster position of of Det #%02d in ZPlane %d under Sector %02d; Strip []; ypos [cm]", iDet, iPlane, iSector+13 ),
                        32, 0, 32, 100, -50, 50 );

                mHistRpcCluTOff[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_TOff", iSector+13, iPlane, iDet ),
                        Form( "Cluster timezero of Det #%02d in ZPlane %d under Sector %02d; Strip []; TOff [ns]", iDet, iPlane, iSector+13 ),
                        32, 0, 32, 100, -tSumMax, tSumMax );

                mHistRpcCluTot[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_Tot", iSector+13, iPlane, iDet ),
                        Form( "Total Cluster Tot of Det #%02d in ZPlane %d under Sector %02d; Strip []; TOT [ns]", iDet, iPlane, iSector+13 ),
                        32, 0, 32, 100, 0, 100 );

                mHistRpcDigiTot[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_TotDigi", iSector+13, iPlane, iDet ),
                        Form( "Total Digi Tot of Det #%02d in ZPlane %d under Sector %02d; Chan []; TOT [ns]", iDet, iPlane, iSector+13 ),
                        64, 0, 64, 50, 0, 20 );

                mHistRpcCluAvWalk[ iSector ][ iPlane ][ iDet ] = new TH2F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_AvWalk", iSector+13, iPlane, iDet ),
                        Form( "Walk in Det #%02d in ZPlane %d under Sector %02d; cluster TOT; T-TSel", iDet, iPlane, iSector+13 ),
                        25, 0, 10, 400, -tSumMax, tSumMax );

                mHistRpcCluMul[ iSector ][ iPlane ][ iDet ] = new TH1F( 
                        Form( "cl_Sector%02d_ZPlane%d_Det%d_Mul", iSector+13, iPlane, iDet ),
                        Form( "Cluster multiplicity of Det #%02d in ZPlane %d under Sector %02d; Mul []; cnts", iDet, iPlane, iSector+13 ),
                        32, 0, 32 );

                mHistHitTrigTimeDet[ iSector ][ iPlane ][ iDet ] = new TH1F( 
                        Form( "Hit_TDiff_Trg_Sector%02d_ZPlane%d_Det%d_Mul", iSector+13, iPlane, iDet ),
                        Form( "Difference between hits on Det #%02d in ZPlane %d under Sector %02d and the trigger token; dT [ns]; Events []", iDet, iPlane, iSector+13 ),
                        10240, -51200, 51200 );

                LOG_DEBUG << "StETofQAMaker::Init() - createHistos: successfully created histograms for detector " << iSector+13 << iPlane << iDet << endm; //PW
            }
        }
    }

    return;
}//::createHistos

//_____________________________________________________________
void
StETofQAMaker::fillHistos()
{
    LOG_INFO << "StETofQAMaker::fillHistos():  -- filling histograms for calibration and QA" << endm;

    mTimeOffset = 0; //reset time offset for each event

    //calculate trigger on calib time scale
    StETofHeader* etofHeader = mETofCollection->etofHeader();
    
    if( !etofHeader ) {
        LOG_WARN << "StETofQAMaker::fillHistos():  -- no eTof Header available -> skip event." << endm;
        return;
    }
    
    // skip event if it has no reset time of AFCK 0x18e6
    if( etofHeader->rocStarTs().count( 0x18e6 ) == 0 ) {
        LOG_WARN << "StETofQAMaker::fillHistos():  -- no reset time for AFCK 0x18e6 available -> skip event." << endm;
        return;
    }

    double triggerTime = etofHeader->trgGdpbFullTime() - ( etofHeader->rocStarTs().at( 0x18e6 ) * dCoarseClockCycleNs );//using the only working reset time in 2018 for now

    uint64_t iBTofOverflows = ( uint64_t ) ( triggerTime / dBTofTimeRange );

    triggerTime-= iBTofOverflows*dBTofTimeRange;
    LOG_INFO << "corrected trigger time: "<< triggerTime <<" ns" << endm;

    double avEtofTime = 0; 
    double avBtofTime = 0; 
    StSPtrVecBTofHit bTofHits = 0;
    Int_t  nHitsInTrigWindow = 0; 

    mTimeOffset = 0; //reset time offset for each event

    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {

        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {

            for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {
                mStorStHit[ iSector ][ iPlane ][ iDet ].clear();
                mStorStHit[ iSector ][ iPlane ][ iDet ].resize( 0 ); //just to be sure

                for( Int_t iStrip = 0; iStrip < uNbStripsPerDet; iStrip++ ) {
                    mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].clear();
                    mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].resize( 0 );
                }
            }
        }
    }


    //fillStorage
    StSPtrVecETofDigi digis = mETofCollection->etofDigis();
    Int_t nbDigis = digis.size();
    Int_t nbDigisInStor = 0;

    StETofDigi* pDigi;
    for( Int_t iDigiPos = 0; iDigiPos < nbDigis; iDigiPos++ ) {
        //sort digis by logical address
        pDigi = digis[ iDigiPos ];

        if ( pDigi->calibTime() == 0 && pDigi->calibTot() == -1 ) {
            LOG_DEBUG << "StETofQAMaker::fillHistos():  --  digi not calibrated, most likely since it is outside the trigger window. Ignore." << endm;
            continue;
        }

        if( pDigi->sector() == 0 || pDigi->zPlane() == 0 || pDigi->counter() == 0 || pDigi->strip() == 0 ) {
            LOG_WARN << "StETofQAMaker::fillHistos():  --  sector / zPlane / counter / strip  was not assigned to the digi" << endm;
            continue;
        }

        mStorStDigi[ pDigi->sector()  - 13 ]
                   [ pDigi->zPlane()  -  1 ]
                   [ pDigi->counter() -  1 ]
                   [ pDigi->strip()   -  1 ].push_back( pDigi ); 

        nbDigisInStor++;
    }

    LOG_INFO << "storage vector is filled with " << nbDigisInStor << " digis" << endm;


    StSPtrVecETofHit hits = mETofCollection->etofHits();
    Int_t nbHits = hits.size();
    Int_t nbHitsInStor = 0;

    StETofHit* pHit;
    for( Int_t iHitPos = 0; iHitPos < nbHits; iHitPos++ ) {
        pHit = hits[ iHitPos ];

        if( pHit->sector() == 0 || pHit->zPlane() == 0 || pHit->counter() == 0 ) {
            LOG_ERROR << "StETofQAMaker::fillHistos():  -- sector / zPlane / counter was not assigned to the hit -- something is wrong!" << endm;
            continue;
        }

        mStorStHit[ pHit->sector()  - 13 ]
                  [ pHit->zPlane()  -  1 ]
                  [ pHit->counter() -  1 ].push_back( pHit );

        nbHitsInStor++;
    }

    LOG_INFO << "storage vector is filled with "<< nbHitsInStor <<" hits" << endm;
    
    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {
        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {
            for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {

                if( mStorStHit[ iSector ][ iPlane ][ iDet ].size() > 0 ) {
                    LOG_DEBUG << iSector + 13  << "  " << iPlane + 1 << "  " << iDet + 1 << "  size mStorStHit: " << mStorStHit[ iSector ][ iPlane ][ iDet ].size() << endm;
                    mHistRpcCluMul[ iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].size() );		    

                    for( unsigned int i = 0; i < mStorStHit[ iSector ][ iPlane ][ iDet ].size(); i++ ) {

                        //added +(uNbStripsPerDet/2.0) to localX in order to recover strip number.
                        //localX has its origin at the center of the detector!

                        if( mTimeOffset == 0 ) {
                            mTimeOffset = mStorStHit[iSector][iPlane][iDet].at(i)->time();
                        }
                        LOG_DEBUG << "The time offset is set to " << mTimeOffset << endm;

                        mHistRpcCluSize[     iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->localX() + ( uNbStripsPerDet / 2.0 ),
                                                                                mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->clusterSize() );
                        mHistRpcCluPosition[ iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->localX() + ( uNbStripsPerDet / 2.0 ),
                                                                                mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->localY() );
                        mHistRpcCluTOff[     iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->localX() + ( uNbStripsPerDet / 2.0 ),
                                                                                mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->time() - mTimeOffset );
                        mHistRpcCluTot[      iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->localX() + ( uNbStripsPerDet / 2.0 ),
                                                                                mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->totalTot() );
                        mHistRpcCluAvWalk[   iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->totalTot(),
                                                                                mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->time() - mTimeOffset ); 

                        mHistHitTrigTimeDiff->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->time()-triggerTime );

                        mHistHitTrigTimeDet[ iSector ][ iPlane ][ iDet ]->Fill( mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->time()-triggerTime ); //detector specific trigger plot


                        avEtofTime += mStorStHit[ iSector ][ iPlane ][ iDet ].at( i )->time();
                        nHitsInTrigWindow++;

                    } //hit loop
                }

            } //det loop
        } //plane loop
    } //sector loop


    avEtofTime /= nHitsInTrigWindow;

    if( avEtofTime > 51200 ) { LOG_DEBUG << "Average ETof time out of range: "<< avEtofTime << endm; }


    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {
        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {
            for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {
                for( Int_t iStrip = 0; iStrip < uNbStripsPerDet; iStrip++ ) {

                    if( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].size() > 0 ) {
                        LOG_DEBUG << iSector+13  << "  " << iPlane+1 << "  " << iDet+1 << "  " << iStrip+1  << "   size mStorStDigi: " << mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].size() << endm;

                        for( unsigned int i=0; i<mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].size(); i++ ) {
                            mHistRpcCluAvWalk[ iSector ][ iPlane ][ iDet ]->Fill( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->calibTot(),
                                                                                  mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->calibTime() - mTimeOffset );
                            mHistRpcDigiTot[   iSector ][ iPlane ][ iDet ]->Fill( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->strip() + 32 * ( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->side() - 1 ) - 1,
                                                                                  mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->calibTot() );

                            mHistDigiRawTrigTimeDiff->Fill( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->rawTime() - ( etofHeader->trgGdpbFullTime() ) );

                            mHistDigiTrigTimeDiff->Fill( mStorStDigi[ iSector ][ iPlane ][ iDet ][ iStrip ].at( i )->calibTime() - triggerTime );
                            //TODO: Timezero should be minus once available.

                        } //digi loop
                    } 
                } //strip loop
            } //det loop
        } //plane loop
    } //sector loop


    if( mBTofCollection ) {
        bTofHits = mBTofCollection->tofHits();
        
        if( bTofHits.size() && nbHits ) { //catch divisions by zero
            for( UInt_t iHit = 0; iHit < bTofHits.size(); iHit++ ) {
                avBtofTime += bTofHits[iHit]->leadingEdgeTime();
            }
            avBtofTime /= bTofHits.size();

            mHistBTofAvTimeDiff        ->Fill( avEtofTime - avBtofTime );
            mHistBTofAvTimeDiffvETofMul->Fill( avEtofTime - avBtofTime, nHitsInTrigWindow );
            mHistBTofAvTimeDiffvBTofMul->Fill( avEtofTime - avBtofTime, bTofHits.size() );
            mHistBTofETofMul           ->Fill( nHitsInTrigWindow, bTofHits.size() );
        }
    }


    // eTof hits
    if( mETofCollection ) {
        StSPtrVecETofHit eHits = mETofCollection->etofHits();
        int nEHits = eHits.size();
        for( int i = 0; i < nEHits; i++ ) {
        if( mTStart ) {
                float timeOfFlight = eHits[ i ]->time() - mTStart;
                mHistETofTimeOfFlight->Fill( timeOfFlight );
                //LOG_INFO << "eTof time of flight: " << timeOfFlight << endm;
            }
        }
    }


    // bTof hits
    if( mBTofCollection ) {
        StSPtrVecBTofHit bHits = mBTofCollection->tofHits();
        int nBHits = bHits.size();
        for( int i = 0; i < nBHits; i++ ) {
        if( mTStart ) {
                float timeOfFlight = bHits[ i ]->leadingEdgeTime() - mTStart;
                mHistBTofTimeOfFlight->Fill( timeOfFlight );
                //LOG_INFO << "bTof time of flight: " << timeOfFlight << endm;
            }
        }
    }


    return;
}//::fillHistos

//_____________________________________________________________
void
StETofQAMaker::writeHistos()
{
    std::string extension = ".etofQA.root";

    if( GetChainOpt()->GetFileOut() != nullptr ) {
        TString outFile = GetChainOpt()->GetFileOut();
    
        mOutHistFileName = ( std::string ) outFile;

        // get rid of .root
        size_t lastindex = mOutHistFileName.find_last_of( "." );
        mOutHistFileName = mOutHistFileName.substr( 0, lastindex );

        // get rid of .MuDst or .event if necessary
        lastindex = mOutHistFileName.find_last_of( "." );
        mOutHistFileName = mOutHistFileName.substr( 0, lastindex );

        // get rid of directories
        lastindex = mOutHistFileName.find_last_of( "/" );
        mOutHistFileName = mOutHistFileName.substr( lastindex + 1, mOutHistFileName.length() );

        mOutHistFileName = mOutHistFileName + extension;
    } else {
        LOG_ERROR << "Cannot set the output filename for histograms" << endm;
    }

    LOG_INFO << "StETofQAMaker::writeHistos -- creating file to save histograms" << endm;
    TFile histFile( mOutHistFileName.c_str(), "RECREATE", "etofQA" );
    histFile.cd();

    mHistBTofAvTimeDiff        ->Write();
    mHistHitTrigTimeDiff       ->Write();
    mHistDigiTrigTimeDiff      ->Write();
    mHistDigiRawTrigTimeDiff   ->Write();
    mHistBTofETofMul           ->Write();
    mHistBTofAvTimeDiffvETofMul->Write();
    mHistBTofAvTimeDiffvBTofMul->Write();

    mHistETofTimeOfFlight      ->Write();
    mHistBTofTimeOfFlight      ->Write();

    for( Int_t iSector = 0; iSector < uNbSectors; iSector++ ) {
        for( Int_t iPlane = 0; iPlane < uNbZPlanes; iPlane++ ) {
            for( Int_t iDet = 0; iDet < uNbDetPerModule; iDet++ ) {
                mHistRpcCluSize[     iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcCluPosition[ iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcCluTOff[     iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcCluTot[      iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcDigiTot[     iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcCluAvWalk[   iSector ][ iPlane ][ iDet ]->Write();
                mHistRpcCluMul[      iSector ][ iPlane ][ iDet ]->Write();
                mHistHitTrigTimeDet[ iSector ][ iPlane ][ iDet ]->Write();
            }
        }
    }

    histFile.Close();

    return;
}//::writeHistos

//_____________________________________________________________
