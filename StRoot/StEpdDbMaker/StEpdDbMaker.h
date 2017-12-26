
#ifndef STEPDDBMAKER_H
#define STEPDDBMAKER_H

#include "StMaker.h"

// tables in STAR DB - those will be defined via the includes in the
// .cxx code - no need for forwrd declaration here
//struct epdQTMap_st;
//struct epdFEEMap_st;
//struct epdStatus_st;
//struct epdGain_st;

class StEpdDbMaker : public StMaker{
  public:
    StEpdDbMaker(const Char_t *name="epdDb");
    virtual ~StEpdDbMaker();
    //virtual Int_t Init();
    //virtual Int_t Make();
    //virtual Int_t Finish();
    virtual Int_t InitRun( Int_t runNumber );
    //virtual void Clear(const Char_t *opt);

    void setDebug(Int_t debug){mDebug=debug;} // 0:minimal message, >0 more debug messages

    // epdQtMap table
    short GetCrateAdc(short ew, short pp, short tile){return mCrateAdc[ew][pp-1][tile];}
    short GetBoardAdc(short ew, short pp, short tile){return mBoardAdc[ew][pp-1][tile];}
    short GetChannelAdc(short ew, short pp, short tile){return mChannelAdc[ew][pp-1][tile];}
    short GetCrateTac(short ew, short pp, short tile){return mCrateTac[ew][pp-1][tile];}
    short GetBoardTac(short ew, short pp, short tile){return mBoardTac[ew][pp-1][tile];}
    short GetChannelTac(short ew, short pp, short tile){return mChannelTac[ew][pp-1][tile];}

    // epdFeeMap table
    short GetTuffId(short ew, short pp, short tile){return mTuffId[ew][pp-1][tile];}
    short GetTuffGroup(short ew, short pp, short tile){return mTuffGroup[ew][pp-1][tile];}
    short GetTuffChannel(short ew, short pp, short tile){return mTuffChannel[ew][pp-1][tile];}
    short GetReceiverBoard(short ew, short pp, short tile){return mReceiverBoard[ew][pp-1][tile];}
    short GetReceiverBoardChannel(short ew, short pp, short tile){return mReceiverBoardChannel[ew][pp-1][tile];}
    short GetCamacCrateAddress(short ew, short pp, short tile){return mCamacCrateAddress[ew][pp-1][tile];}
    unsigned int* GetOneWireId(short ew, short pp, short tile){return mWireOneId[ew][pp-1][tile];}

    // epdStatus table
    short GetStatus(short ew, short pp, short tile){return mStatus[ew][pp-1][tile];}

    // epdGain table
    float GetVPed(short ew, short pp, short tile){return mVPed[ew][pp-1][tile];}
    float GetMip(short ew, short pp, short tile){return mMip[ew][pp-1][tile];}
    float GetQtPedestals(short ew, short pp, short tile){return mQtPedestals[ew][pp-1][tile];}
    float GetDarkCurrent(short ew, short pp, short tile){return mDarkCurrent[ew][pp-1][tile];}
    float GetQtPedestalsSigma(short ew, short pp, short tile){return mQtPedestalsSigma[ew][pp-1][tile];}
    float GetOffset(short ew, short pp, short tile){return mOffset[ew][pp-1][tile];}

  protected:

  private:
    void ResetArrays();
    Int_t mDebug;

    short mCrateAdc[2][12][32];
    short mBoardAdc[2][12][32];
    short mChannelAdc[2][12][32];
    short mCrateTac[2][12][32];
    short mBoardTac[2][12][32];
    short mChannelTac[2][12][32];

    short mTuffId[2][12][32];
    short mTuffGroup[2][12][32];
    short mTuffChannel[2][12][32];
    short mReceiverBoard[2][12][32];
    short mReceiverBoardChannel[2][12][32];
    short mCamacCrateAddress[2][12][32];
    unsigned int mWireOneId[2][12][32][2];

    short mStatus[2][12][32];

    float mVPed[2][12][32];
    float mMip[2][12][32];
    float mQtPedestals[2][12][32];
    float mDarkCurrent[2][12][32];
    float mQtPedestalsSigma[2][12][32];
    float mOffset[2][12][32];

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
    ClassDef(StEpdDbMaker,1);
};
#endif
