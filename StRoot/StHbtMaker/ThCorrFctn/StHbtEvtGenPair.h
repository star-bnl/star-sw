/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Create pair from StHbtFsiHiddenInfo
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_EVTGEN_PAIR_HH
#define ST_HBT_EVTGEN_PAIR_HH

#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"

class StHbtEvtGenPair : public StHbtThPair{
 public:
  StHbtEvtGenPair(short aDecoralate=0);
  virtual ~StHbtEvtGenPair();
 protected:
  virtual void setVariables(const StHbtPair* aPair);
  short mDecoralate;

  int mNStoredPos;
  StHbtLorentzVector* mPosArray1;//!
  short* mValidPos1;//!
  StHbtLorentzVector* mPosArray2;//!
  short* mValidPos2;//!

  ClassDef(StHbtEvtGenPair,1)
};


#endif
