#ifndef STREADLASETEVENT_H__
#define STREADLASETEVENT_H__

class TH1F;

class StReadLaserEvent
{
public:

    StReadLaserEvent();
    ~StReadLaserEvent(){};
    float Make(int runNumber, int eventNumber, char* datap);
    void resetAll();

    
    const TH1* driftVelocityDistribution() const { return mVelGauss; } 

private:
    int nloop;

    float vDrift;

    void getMeanPosition();
    float getMeanDriftVelocity();
    float getTruncatedMeanDriftVelocity(float vMean);
    float getDriftVelocityFromGaussFit();
    float getDriftVelocityFromHistMean();
    void fillDifference(float vmean);
    void showVelocity(float vMean, float eMean);
    //  void writeHistograms();


    void SetRunNumber(int n) {mRun = n;}
    int getRunNumber() {return mRun;};
    void SetFirstEvent(int n) {mFirstEvent = n;}
    int getFirstEvent() {return mFirstEvent;};
    void SetLastEvent(int n) {mLastEvent = n;}
    int getLastEvent() {return mLastEvent;}
    //void SetFrequencyAndPressure();
    //float getFrequency() {return mFrequency;}
    //float getPressure() {return mPressure;}

    void setVDrift(float v) {vDrift = v;}
    float getVDrift() {return vDrift;}
    void writeVDrift();

    int mRun;
    int mFirstEvent;
    int mLastEvent;
    float mFrequency;
    float mPressure;

    TH1F* mSector[24];
    TH1F* mMean;
    TH1F* mVel;
    TH1F* mDeltaVel;
    TH1F* mVelGauss;
};
#endif







/***************************************************************************
 *
 * $Id: StReadLaserEvent.h,v 1.1 2009/01/23 16:10:59 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StReadLaserEvent.h,v $
 * Revision 1.1  2009/01/23 16:10:59  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.6  2008/03/17 22:54:16  fine
 * get rif of the redundant tpc paramater to make it RTS for offline compliant too. Thanks Paul
 *
 * Revision 1.5  2008/02/15 18:51:55  dkettler
 * Updates to laser and TOF reader
 *
 * Revision 1.3  2007/12/13 02:47:39  psoren
 * moved resetAll to public so it can be called in HistoHandler
 *
 * Revision 1.2  2007/05/07 18:58:22  laue
 * Added drift time distribution histograms
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/

