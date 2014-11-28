/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtTrivia
 *
 ***************************************************************************
 *
 * Description: data for individual trivia of the GMT.
 *
 ***************************************************************************/

#ifndef _ST_GMT_TRIVIA_H_
#define _ST_GMT_TRIVIA_H_

#include "StObject.h"

class StGmtTrivia : public StObject {    
 public:
  // constructors
  StGmtTrivia();
  StGmtTrivia(int GeoId , int moduleX, int layerX, int stripX);
  StGmtTrivia(const StGmtTrivia&);
  StGmtTrivia& operator=( const StGmtTrivia& );
  
  // deconstructor
  ~StGmtTrivia();
  
  // accessors
  int   getGeoId()   const{ return mGeoId; };
  int   getCoor()   const{ return mCoor; };//0-127 for each coordinate
  int getMaxAdc() const{ return mMaxAdc; };
  int getRunId() const{ return mRunId; };
  int getMaxPedSubtractedAdc() const{ return mMaxAdcPS; };
  int getMaxPedSubtractedAdcTB() const{ return mMaxAdcPSTB; };
  float getCharge()  const{ return mCharge; };
  float getPed() const{ return mPed; };
  float getCX() const{ return mCX; };
  float getCY() const{ return mCY; };
  float getCAX() const{ return mCAX; };
  float getCAY() const{ return mCAY; };
  int getClustSize() const{ return mClustSize; };
  float getPedDev() const{ return mPedDev; };
  float getPos() const{ return mPos; };    
  double getAdc(int timebin){if(timebin >=0 && timebin<15) return mAdc[timebin];};
  void  getElecCoords( int& rdo, int& arm,  int& apv,  int& chan ){ rdo = mRdo; arm = mArm; apv = mApv; chan = mChan; };
    
  // modifiers
  void setClustSize (int c) {mClustSize=c;};
  void setMaxAdc(int a){mMaxAdc=a;};
  void setRunId(int a){mRunId=a;};
  void setCoor(int a){mCoor=a;};
  void setGeoId(int a){mGeoId=a;};
  void setMaxPedSubtractedAdc(int a){mMaxAdcPS=a;};
  void setMaxPedSubtractedAdcTB(int a){mMaxAdcPSTB=a;};
  void setCharge(double a) {mCharge=a;};
  void setPos(double a){mPos=a;};
  void setPed(double a){mPed=a;};
  void setPedDev(double a){mPedDev=a;};
  void setPedErr(double a){mRMS=a;};    
  void setAdc( int timebin, double adc){if(timebin >=0 && timebin<15)mAdc[timebin]=adc;};//PS
  void setElecCoords( int rdo, int arm,  int apv,  int chan ){ mRdo = rdo; mArm = arm; mApv = apv; mChan = chan; }; 
  
  void setCX(double a){mCX=a;};
  void setCY(double a){mCY=a;};
  void setCAX(double a){mCAX=a;};
  void setCAY(double a){mCAY=a;};

 protected:
  // data members
  
 private:   
  Int_t   mGeoId;                // indexing: 8 modules * 2 APV * 128 channels = 2048
  Int_t   mMaxAdc;
  Int_t   mRunId;
  Int_t   mMaxAdcPS;
  Int_t   mMaxAdcPSTB;
  Float_t mCharge;
  Float_t mPed;
  Float_t mPos;
  Float_t mPedDev;
  Float_t mRMS;
  Float_t mCX, mCY, mCAY, mCAX;
  Int_t    mModuleX ;
  Int_t   mLayerX ;
  Int_t   mStripX ;
  Int_t   mClustSize;
  Int_t   mCoor ;
  Int_t   mRdo, mArm, mApv, mChan; // elec coords, straight out of the DAQ file
  
  Double_t mAdc[15];
  
  ClassDef(StGmtTrivia,2)
    };

// inline functions

// accessors
//inline int   StGmtTrivia::getGeoId()           const { return mGeoId; };
//inline void StGmtTrivia::setGeoId( int geoId )                 { mGeoId = geoId; };
//inline void StGmtStrip::
//inline void StGmtStrip::setElecCoords( int rdo, int arm,  int apv,  int chan )

#endif
