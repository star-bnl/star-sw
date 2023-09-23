/***************************************************************************
 *
 * $Id: StETofHitMaker.h,v 1.6 2021/01/29 15:08:31 weidenkaff Exp $
 *
 * Author: Philipp Weidenkaff & Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofHitMaker - class to read the eTofCollection from
 * StEvent and combine digis on both sides of each read-out strip into a hit.
 * The hits on each strip are further merger into clusters.
 * The eTOF collection is filled with hits and written to StEvent.
 *
 ***************************************************************************
 *
 * $Log: StETofHitMaker.h,v $
 * Revision 1.6  2021/01/29 15:08:31  weidenkaff
 * fixed memory leak in StEtofHitMaker.cxx by adding a delete to merged hits
 *
 * Revision 1.5  2020/01/16 03:39:39  fseck
 * add possibility to calculate VPD start time
 *
 * Revision 1.4  2019/12/10 15:58:29  fseck
 * ignore digis in dead time software-wise + possibility to correct clock jumps based on hit position via setting a flag
 *
 * Revision 1.3  2019/03/25 01:08:21  fseck
 * added cyclic mean calculation function for average hit time
 *
 * Revision 1.2  2019/03/08 19:03:35  fseck
 * moved QA histograms for clustered hits into separate function
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFHITMAKER_H
#define STETOFHITMAKER_H

#include <string>
#include <vector>
#include <map>

#include "StMaker.h"
#include "StThreeVectorD.hh"

class TH1;

class StEvent;
class StMuDst;

class StETofDigi;
class StMuETofDigi;

class StETofHit;
class StMuETofHit;

class StETofGeometry;


class StETofHitMaker: public StMaker {
public:
    StETofHitMaker( const char* name = "etofHit" );     // default constructor

    ~StETofHitMaker();


    Int_t  Init();
    Int_t  InitRun(   Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();


    void processStEvent();
    void processMuDst();

    /// read hit building parameters from file
    void setFileNameHitParam( const char* fileName );
    void setFileNameSignalVelocity( const char* fileName );
    void setFileNameModMatrix( const char* fileName );
    void setFileNameAlignParam( const char* fileName );

    void setGet4MinTime( const double minTime );
    void setSoftwareDeadTime( const double& deadTime );
    void setDoClockJumpShift( const bool    doShift  );
    void setDoDoubleClockJumpShift( const bool    doDoubleShift  );
 
    void setIsSim( const bool isSim ); // for simulated digis
    void setDoQA( const bool doQA );
    void setDebug( const bool debug );
    void setDoAfterPulseCorr( const bool apcorr );

    void updateClockJumpMap( const std::map< int, int >& clockJumpDir );

    void modifyHit(int modMode, double& localX, double& localY, double& time);

private:
    // internal subfunctions ----------------------------------------------------------------------
    void bookHistograms();
    void setHistFileName();
    void writeHistograms();

    void clearHits( const bool isMuDst ); // clear hits from StEvent / MuDst in afterburner mode
    void clearStorage();

    bool fillStorage( StETofDigi*   aDigi, unsigned int index ); // sorts digis into vectors by detector
    bool fillStorage( StMuETofDigi* aDigi, unsigned int index ); // sorts digis into vectors by detector

    void matchSides();                               // matches digis on opposing sides to form single strip hits

    void mergeClusters( const bool isMuDst );        // merges hits to clusters across multiple strips

    void assignAssociatedHits( const bool isMuDst ); // write associated hits into digis

    double startTime();
    void   startTimeVpd( double& startTime, double& vertexVz );

    void fillUnclusteredHitQA( const double& tstart, const bool isMuDst );

    void fillHitQA( const bool isMuDst, const double& tstart );
    void updateCyclicRunningMean( const double& value, double& mean, int& count, const double& range );

    unsigned int detectorToKey( const unsigned int detectorId );

    // internal containers ------------------------------------------------------------------------
    StEvent*                mEvent;
    StMuDst*                mMuDst;
    StETofGeometry*         mETofGeom;   // pointer to the ETof geometry utility class


    std::string   mFileNameHitParam;        // name of parameter file for hit parameters
    std::string   mFileNameSignalVelocity;  // name of parameter file for signal velocity
    std::string   mFileNameModMatrix;       // name of parameter file for hit modification on counter level
    std::string	  mFileNameAlignParam;      // name of parameter file for counter alignment in geometry
  
    // store digis ordered by detectorstrip for side-matching 
    std::map< UInt_t, std::vector< StETofDigi* > > mStoreDigi;  // key: strip index, value: vector of digis

    // link between mStorStDigi and StETofCollection->digis()
    std::map< StETofDigi*, UInt_t > mMapDigiIndex; 

    // store hit ordered by detector for merging.
    std::map< UInt_t, std::vector< StETofHit* > > mStoreHit; // key: detector index, value: vector of hits

    // link between hits and constituent digis
    std::map< StETofHit*, std::vector< UInt_t > > mMapHitDigiIndices;
    
    // map between hit index in MuDsts and constituent digis
    std::map< UInt_t, std::vector< UInt_t > > mMapHitIndexDigiIndices;

    // parameters ---------------------------------------------------------------------------------
    Double_t mMaxYPos;               // maximum absolute Y Pos for side matching
    Double_t mMergingRadius;         // maximum XYT radius[ns] for cluster merging

    std::map< UInt_t, Double_t > mSigVel; // signal velocities in each detector

    Double_t mSoftwareDeadTime;            // dead time introduced in software to reject after pulses on the same channel
    Bool_t   mDoClockJumpShift;            // correct for clock jumps on one side
    Bool_t   mDoDoubleClockJumpShift;      // correct for clock jumps on both sides

    std::map< Int_t, Int_t >     mClockJumpDirection;  // stores direction of clock jump for time correction

    std::map< Int_t, int >     mModMatrix;  // stores mode of modification for hits on striplevel (flip)

    float mGet4doublejumpTmin;                                            // cutoff for double jump correction
    std::map< Int_t, bool  >                    mGet4doublejumpFlag;      // get4  jumpflag
    std::map< Int_t, std::vector < float > >    mGet4doublejumpTimes;     // get4  time of last n hits
     
    
    // histograms for QA --------------------------------------------------------
    Bool_t                    mIsSim;
    Bool_t                    mDoQA;
    Bool_t                    mDebug;
    Bool_t                    mApCorr;

    std::string                    mHistFileName;
    std::map< std::string, TH1* >  mHistograms;

    std::vector< Bool_t >          mCounterActive;

    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }

    ClassDef( StETofHitMaker, 0 )
};


inline void StETofHitMaker::setFileNameHitParam(       const char* fileName )     { mFileNameHitParam       = fileName; }
inline void StETofHitMaker::setFileNameSignalVelocity( const char* fileName )     { mFileNameSignalVelocity = fileName; }
inline void StETofHitMaker::setFileNameModMatrix(      const char* fileName )     { mFileNameModMatrix = fileName; }
inline void StETofHitMaker::setFileNameAlignParam(     const char* fileName )     { mFileNameAlignParam  = fileName;   }


inline void StETofHitMaker::setGet4MinTime(      const double  minTime )          { mGet4doublejumpTmin     = minTime; }
inline void StETofHitMaker::setSoftwareDeadTime( const double& deadTime )         { mSoftwareDeadTime       = deadTime; }
inline void StETofHitMaker::setDoClockJumpShift( const bool    doShift  )         { mDoClockJumpShift       = doShift;  }
inline void StETofHitMaker::setDoDoubleClockJumpShift( const bool doDoubleShift ) { mDoDoubleClockJumpShift       = doDoubleShift;  }

inline void StETofHitMaker::setIsSim( const bool isSim )                          { mIsSim = isSim; }
inline void StETofHitMaker::setDoQA(  const bool doQA )                           { mDoQA  = doQA;  }
inline void StETofHitMaker::setDebug( const bool debug )                          { mDebug = debug; }
inline void StETofHitMaker::setDoAfterPulseCorr( const bool apcorr )              { mApCorr = apcorr; }

#endif
