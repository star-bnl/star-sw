#ifndef STAR_StFtpcCluster
#define STAR_StFtpcCluster

/*#define DEBUG 1*/
/*#define DEBUGFILE 1*/
#include "tables/St_fcl_fppoint_Table.h"

class StFtpcCluster
{
 private:
  int mRow;
  int mSector;
  int mNPads;
  int mNBins;
  int mMaxAdc;
  int mCharge;
  int mFlags;
  float mX;
  float mY;
  float mZ;
  float mSPhi;
  float mSR;
  StFtpcCluster *mNextCluster;
 public:
  StFtpcCluster();
  ~StFtpcCluster();
  int Fill(fcl_fppoint_st *fppoint);

  int getRow() {return mRow;}
  int getSector() {return mSector;}
  int getNPads() {return mNPads;}
  int getNBins() {return mNBins;}
  int getMaxAdc() {return mMaxAdc;}
  int getCharge() {return mCharge;}
  int getFlags() {return mFlags;}
  float getX() {return mX;}
  float getY() {return mY;}
  float getZ() {return mZ;}
  float getSigmaPhi() {return mSPhi;}
  float getSigmaRad() {return mSR;}

  void setRow(int row) {mRow=row;}
  void setSector(int sec) {mSector=sec;}
  void setNPads(int num) {mNPads=num;}
  void setNBins(int num) {mNBins=num;}
  void setMaxAdc(int adc) {mMaxAdc=adc;}
  void setCharge(int charge) {mCharge=charge;}
  void setFlags(int flags) {mFlags=flags;}
  void setX(float x) {mX=x;}
  void setY(float y) {mY=y;}
  void setZ(float z) {mZ=z;}
  void setSigmaPhi(float sigma) {mSPhi=sigma;}
  void setSigmaRad(float sigma) {mSR=sigma;}
};

#endif
