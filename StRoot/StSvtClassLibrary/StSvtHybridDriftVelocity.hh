/***************************************************************************
 *
 * $Id: StSvtHybridDriftVelocity.hh,v 1.1 2000/11/30 20:38:31 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Drift Velocity class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDriftVelocity.hh,v $
 * Revision 1.1  2000/11/30 20:38:31  caines
 * Drift Velocity files
 *
 **************************************************************************/

#ifndef STSVTHYBRIDDRIFTVELOCITY_HH
#define STSVTHYBRIDDRIFTVELOCITY_HH

#include "StSvtHybridObject.hh"

class StSvtHybridDriftVelocity : public StSvtHybridObject
{
public:
  StSvtHybridDriftVelocity();
  StSvtHybridDriftVelocity(int barrel, int ladder, int wafer, int hybrid);
  ~StSvtHybridDriftVelocity();

  StSvtHybridDriftVelocity(const StSvtHybridDriftVelocity&);
  StSvtHybridDriftVelocity& operator = (const StSvtHybridDriftVelocity&);
  float operator [] (int anode);

  int getNumberOfAnodes(){return numberOfAnodes;}
  float getV1(int anode){return mV1[anode-1];}
  float getV2(int anode){return mV2[anode-1];}
  float getV3(int anode){return mV3[anode-1];}
  float getTotalLength(float l){return mTotalLength;}
  float getFocusLength(float l){return mFocusLength;}
  float getNoDriftLength(float l){return mNoDriftLength;}

  void setV1(float v, int anode){mV1[anode-1] = v;}
  void setV2(float v, int anode){mV2[anode-1] = v;}
  void setV3(float v, int anode){mV3[anode-1] = v;}
  void setBilinearConst(float aa, float bb){mA = aa; mB = bb;}
  void setTotalLength(float l){mTotalLength = l;}
  void setFocusLength(float l){mFocusLength = l;}
  void setNoDriftLength(float l){mNoDriftLength = l;}

private:
  int numberOfAnodes;
  float mV1[240];  // focusing region velocity per anode
  float mV2[240];  // drift region velocity per anode
  float mV3[240];  // average velocity (considering just one region) per anode
  float mA, mB;          // v2 = a*v1 - b (model 1)
  float mTotalLength;    // wafer total length (time direction)
  float mFocusLength;    // focusing region length 
  float mAlpha;          // deceleration in focusing region (model 2)
  float mNoDriftLength;  // length of No drift region (model 2)
  float mDriftCorr[4];    // accounts for the PASA response time
  float *mDevLinFit[240]; //! deviations from a straight line fit to the drift curve


  ClassDef(StSvtHybridDriftVelocity,1)
};

#endif
