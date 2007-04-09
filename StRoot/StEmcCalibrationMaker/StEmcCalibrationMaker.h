#ifndef STAR_StEmcCalibrationMaker
#define STAR_StEmcCalibrationMaker

#include <TH2.h>
#include <TH1.h>
#include <TString.h>

#include <StMaker.h>

class StEvent;
class StEmcCollection;
class StEmcFilter;
class StEmcPosition;
class StEmcGeom;
class StEmcCluster;
class StTrack;
class StBemcData;

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000

class StEmcCalibrationMaker : public StMaker 
{
  private: 		
              short             mADC[MAXBEMC][MAXCHANNEL];             
              short             mADCPedSub[MAXBEMC][MAXCHANNEL];
              float             mPed[MAXBEMC][MAXCHANNEL];
              float             mPedRms[MAXBEMC][MAXCHANNEL];
              unsigned char     mCap[MAXBEMC][MAXCHANNEL];
              unsigned char     mNTracksTower[MAXCHANNEL];
              bool              mIsIsolatedTower[MAXCHANNEL];
	      bool              mHasDetector[MAXBEMC];
              
              int               mNChannel[MAXBEMC];
              
              StTrack           *mTrack[MAXTRACK];
              float             mTrackP[MAXTRACK];
              float             mTrackPt[MAXTRACK];
              short             mTrackTower[MAXTRACK];
              short             mTrackTowerExit[MAXTRACK];
              short             mTrackNPoints[MAXTRACK];
              int               mNTracks;
              bool              mL3Tracks;
              bool              mIsTriggerOk;
              
              float             mVx;
              float             mVy;
              float             mVz;
							
							long              mCTBSum;
							long              mZDCSum;
              
              StEvent           *mStEvent;
							StBemcData        *mBemcData;
              StEmcCollection   *mEmcCol;
              StEmcGeom         *mGeom[MAXBEMC];
              StEmcPosition     *mPosition;
              
              bool              mDebug;
              
              float             mField;
              
              void              fillEmcArrays();
              void              fillTrackArrays();
                            
                            
  public:
     
                                StEmcCalibrationMaker(const char *name="Calib");
   virtual                      ~StEmcCalibrationMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              zeroAll();              
   
              int               getADC(int det,int id)        { return (int)mADC[det-1][id-1]; }
              int               getADCPedSub(int det,int id)  { return (int)mADCPedSub[det-1][id-1]; }
              int               getPed(int det,int id)        { return (int)mPed[det-1][id-1]; }
              float             getPedRms(int det,int id)     { return mPedRms[det-1][id-1]; }
              unsigned int      getCap(int det,int id)        { return (unsigned int)mCap[det-1][id-1]; }
              int               getNTracksInTower(int id)     { return (int)mNTracksTower[id-1]; }
              bool              isIsolatedTower(int id)       { return mIsIsolatedTower[id-1]; }
							bool              hasDetector(int det)          { return mHasDetector[det-1];}
              
              float             getTrackP(int track)          { return mTrackP[track]; }
              float             getTrackPt(int track)         { return mTrackPt[track]; }
              int               getTrackNPoints(int track)    { return (int)mTrackNPoints[track]; }
              int               getTrackTower(int track)      { return (int)mTrackTower[track]; }
              int               getTrackTowerExit(int track)  { return (int)mTrackTowerExit[track]; }
              StTrack*          getTrack(int track)           { return mTrack[track]; }
              int               getNTracks()                  { return mNTracks; }
              bool              isL3()                        { return (int)mL3Tracks; }
              bool              isTriggerOk()                 { return mIsTriggerOk;}
              float             vx()                          { return mVx; }
              float             vy()                          { return mVy; }
              float             vz()                          { return mVz; }
              float             field()                       { return mField; }
              StEvent*          event()                       { return mStEvent; }
              StEmcCollection*  emc()                         { return mEmcCol; }
							long              getCTBSum()                   { return mCTBSum;}
							long              getZDCSum()                   { return mZDCSum;}
							
							void              makeStatus(bool,bool,bool,bool);
   
              
   ClassDef(StEmcCalibrationMaker, 1)  
};

#endif
