 /***************************************************************************
 *
 * $Id: StETofCalibMaker.h,v 1.6 2019/12/19 02:19:13 fseck Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofCalibMaker - class to read the eTofCollection from
 * StEvent, do calibration, fill the collection with the updated information
 * and write back to StEvent
 *
 ***************************************************************************
 *
 * $Log: StETofCalibMaker.h,v $
 * Revision 1.6  2019/12/19 02:19:13  fseck
 * use known pulser time differences inside one Gbtx to recover missing pulser signals
 *
 * Revision 1.5  2019/12/10 15:54:56  fseck
 * added new database tables for pulsers, updated pulser handling and trigger time calculation
 *
 * Revision 1.4  2019/05/08 23:57:09  fseck
 * added function to set the reference pulser
 *
 * Revision 1.3  2019/03/25 01:09:17  fseck
 * added first version of pulser correction procedure
 *
 * Revision 1.2  2019/03/08 19:01:01  fseck
 * pick up the right trigger and reset time on event-by-event basis  +  fix to clearing of calibrated tot in afterburner mode  +  flag pulser digis
 *
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFCALIBMAKER_H
#define STETOFCALIBMAKER_H


#include <string>
#include <map>
#include <utility>

#include "StMaker.h"

class TH1F;
class TProfile;

class StEvent;
class StMuDst;

class StETofHeader;
class StETofDigi;
class StMuETofDigi;
class StMuETofHeader;

class StETofHardwareMap;


class StETofCalibMaker: public StMaker {
public:
    /// default constructor
    StETofCalibMaker( const char* name = "etofCalib" );

    ~StETofCalibMaker();


    Int_t  Init();
    Int_t  InitRun(   Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    void processStEvent();
    void processMuDst();

    /// read calibration parameters from file
    void setFileNameCalibParam(         const char* fileName );
    void setFileNameElectronicsMap(     const char* fileName );
    void setFileNameStatusMap(          const char* fileName );
    void setFileNameTimingWindow(       const char* fileName );
    void setFileNameSignalVelocity(     const char* fileName );
    void setFileNameCalibHistograms(    const char* fileName );
    void setFileNameResetTimeCorr(      const char* fileName );
    void setFileNamePulserTotPeak(      const char* fileName );
    void setFileNamePulserTimeDiffGbtx( const char* fileName );

    void setDoQA(  const bool doQA  );
    void setDebug( const bool debug );
    void setReferencePulserIndex( const int index );


private:
    bool isFileExisting( const std::string fileName );

    void resetToRaw      ( StETofDigi* aDigi );
    void applyMapping    ( StETofDigi* aDigi );
    void flagPulserDigis ( StETofDigi* aDigi, unsigned int index, std::map< unsigned int, std::vector< unsigned int > >& pulserCandMap );
    void applyCalibration( StETofDigi* aDigi, StETofHeader* etofHeader );


    void resetToRaw      ( StMuETofDigi* aDigi );
    void applyMapping    ( StMuETofDigi* aDigi );
    void flagPulserDigis ( StMuETofDigi* aDigi, unsigned int index, std::map< unsigned int, std::vector< unsigned int > >& pulserCandMap );
    void applyCalibration( StMuETofDigi* aDigi, StMuETofHeader* etofHeader );

    void calculatePulserOffsets( std::map< unsigned int, std::vector< unsigned int > >& pulserCandMap );


    double calibTotFactor (    StETofDigi* aDigi );
    double calibTimeOffset(    StETofDigi* aDigi );
    double slewingTimeOffset(  StETofDigi* aDigi );
    double applyPulserOffset(  StETofDigi* aDigi );

    double resetTimeCorr() const;

    double triggerTime( StETofHeader* etofHeader );
    double resetTime  ( StETofHeader* etofHeader );

    unsigned int channelToKey(  const unsigned int channelId  );
    unsigned int detectorToKey( const unsigned int detectorId );
    unsigned int sideToKey(     const unsigned int sideId     );


    void bookHistograms();
    void setHistFileName();
    void writeHistograms();


    StEvent*             mEvent;
    StMuDst*             mMuDst;
    StETofHardwareMap*   mHwMap;         // electronic channel to hardware/geometry map

    std::string   mFileNameCalibParam;          // name of parameter file for calibration parameters
    std::string   mFileNameElectronicsMap;      // name of parameter file for electronics-to-hardware map
    std::string   mFileNameStatusMap;           // name of parameter file for status map
    std::string   mFileNameTimingWindow;        // name of parameter file for timing window
    std::string   mFileNameSignalVelocity;      // name of parameter file for signal velocity
    std::string   mFileNameCalibHistograms;     // name of parameter file for calibration histograms (output of QA maker)
    std::string   mFileNameResetTimeCorr;       // name of parameter file for reset time correction
    std::string   mFileNamePulserTotPeak;       // name of parameter file for pulser peak tot
    std::string   mFileNamePulserTimeDiffGbtx;  // name of parameter file for pulser time diff

    Int_t         mRunYear;                 // "year" of operation by using roughly October 1st as reference
    Float_t       mGet4TotBinWidthNs;       // conversion factor for Get4 chip TOT bin to nanoseconds
    Int_t         mMinDigisPerSlewBin;      // minimal required statistics per channel and TOT bin to apply slewing corrections
    Float_t       mResetTimeCorr;           // additional offset for the whole system in case reset time was not saved correctly

    Double_t      mTriggerTime;             // trigger time in ns
    Double_t      mResetTime;               // reset time in ns 

    Float_t       mPulserPeakTime;          // pulser peak time relative to the trigger time in ns
    Int_t         mReferencePulserIndex;    // index of reference pulser used in the pulser correction to correct time offset between different Gbtx

    std::map< UInt_t, std::pair< Float_t, Float_t > >  mTimingWindow;    // timing window for each AFCK
    std::map< UInt_t, std::pair< Float_t, Float_t > >  mPulserWindow;    // pulser window for each AFCK
    std::map< UInt_t, UInt_t >   mStatus;          // status of each channel: 0 - not existing/not working, 1 - working
    std::map< UInt_t, Float_t >  mSignalVelocity;  // signal velocities in each detector

    std::map< UInt_t, TH1F* >     mDigiTotCorr;     // factor to calibrate TOT  per channel saved in one histogram (64 bins) per counter accessed by detectorId as key
    std::map< UInt_t, TH1F* >     mDigiTimeCorr;    // offset to calibrate time per channel saved in one histogram (64 bins) per counter accessed by detectorId as key
    std::map< UInt_t, TProfile* > mDigiSlewCorr;    // offset to account for slewing corrections per channel saved in a histogram (~30 TOT bins) accessed by channelId as key

    std::map< UInt_t, Float_t >   mPulserPeakTot;       // TOT of pulsers on each side of the RPC counters (as key)
    std::map< UInt_t, Double_t >  mPulserTimeDiff;      // pulser time difference with respect to the reference pulser for each detector side as key
    std::map< UInt_t, Double_t >  mPulserTimeDiffGbtx;  // pulser time difference with inside a Gbtx with respect to counter 1

    std::map< UInt_t, UInt_t >    mJumpingPulsers;      // flag jumping pulsers

    Bool_t        mUsePulserGbtxDiff;
    Bool_t        mDoQA;
    Bool_t        mDebug;
    std::string                    mHistFileName;
    std::map< std::string, TH1* >  mHistograms;


    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }

    ClassDef( StETofCalibMaker, 0 )
};

inline void StETofCalibMaker::setFileNameCalibParam(         const char* fileName )     { mFileNameCalibParam         = fileName; }
inline void StETofCalibMaker::setFileNameElectronicsMap(     const char* fileName )     { mFileNameElectronicsMap     = fileName; }
inline void StETofCalibMaker::setFileNameStatusMap(          const char* fileName )     { mFileNameStatusMap          = fileName; }
inline void StETofCalibMaker::setFileNameTimingWindow(       const char* fileName )     { mFileNameTimingWindow       = fileName; }
inline void StETofCalibMaker::setFileNameSignalVelocity(     const char* fileName )     { mFileNameSignalVelocity     = fileName; }
inline void StETofCalibMaker::setFileNameCalibHistograms(    const char* fileName )     { mFileNameCalibHistograms    = fileName; }
inline void StETofCalibMaker::setFileNameResetTimeCorr(      const char* fileName )     { mFileNameResetTimeCorr      = fileName; }
inline void StETofCalibMaker::setFileNamePulserTotPeak(      const char* fileName )     { mFileNamePulserTotPeak      = fileName; }
inline void StETofCalibMaker::setFileNamePulserTimeDiffGbtx( const char* fileName )     { mFileNamePulserTimeDiffGbtx = fileName; }


inline double StETofCalibMaker::resetTimeCorr() const { return mResetTimeCorr; }

inline void StETofCalibMaker::setDoQA(  const bool doQA  ) { mDoQA  = doQA;  }
inline void StETofCalibMaker::setDebug( const bool debug ) { mDebug = debug; }
inline void StETofCalibMaker::setReferencePulserIndex( const int index ) { mReferencePulserIndex = index; }

#endif // STETOFCALIBMAKER_H