
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
    short GetCrateAdc(short ew, short pp, short tile){return mCrateAdc[ew][pp][tile];}
    short GetBoardAdc(short ew, short pp, short tile){return mBoardAdc[ew][pp][tile];}
    short GetChannelAdc(short ew, short pp, short tile){return mChannelAdc[ew][pp][tile];}
    short GetCrateTac(short ew, short pp, short tile){return mCrateTac[ew][pp][tile];}
    short GetBoardTac(short ew, short pp, short tile){return mBoardTac[ew][pp][tile];}
    short GetChannelTac(short ew, short pp, short tile){return mChannelTac[ew][pp][tile];}

    // epdFeeMap table
    short GetTuffId(short ew, short pp, short tile){return mTuffId[ew][pp][tile];}
    short GetTuffGroup(short ew, short pp, short tile){return mTuffGroup[ew][pp][tile];}
    short GetTuffChannel(short ew, short pp, short tile){return mTuffChannel[ew][pp][tile];}
    short GetReceiverBoard(short ew, short pp, short tile){return mReceiverBoard[ew][pp][tile];}
    short GetReceiverBoardChannel(short ew, short pp, short tile){return mReceiverBoardChannel[ew][pp][tile];}
    short GetCamacCrateAddress(short ew, short pp, short tile){return mCamacCrateAddress[ew][pp][tile];}
    char* GetOneWireId(short ew, short pp, short tile){return mWireOneId[ew][pp][tile];}

    // epdStatus table
    short GetStatus(short ew, short pp, short tile){return mStatus[ew][pp][tile];}

    // epdGain table
    short GetVPed(short ew, short pp, short tile){return mVPed[ew][pp][tile];}
    short GetMip(short ew, short pp, short tile){return mMip[ew][pp][tile];}
    short GetQtPedestals(short ew, short pp, short tile){return mQtPedestals[ew][pp][tile];}
    short GetDarkCurrent(short ew, short pp, short tile){return mDarkCurrent[ew][pp][tile];}

  protected:

  private:
    void ResetArrays();
    Int_t mDebug;

    short mCrateAdc[2][12][31];
    short mBoardAdc[2][12][31];
    short mChannelAdc[2][12][31];
    short mCrateTac[2][12][31];
    short mBoardTac[2][12][31];
    short mChannelTac[2][12][31];

    short mTuffId[2][12][31];
    short mTuffGroup[2][12][31];
    short mTuffChannel[2][12][31];
    short mReceiverBoard[2][12][31];
    short mReceiverBoardChannel[2][12][31];
    short mCamacCrateAddress[2][12][31];
    char mWireOneId[2][12][31][20];

    short mStatus[2][12][31];

    short mVPed[2][12][31];
    short mMip[2][12][31];
    short mQtPedestals[2][12][31];
    short mDarkCurrent[2][12][31];

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
    ClassDef(StEpdDbMaker,1);
};
#endif
