/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation of StHbtThPairDummy
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThPairDummy.h"

StHbtThPairDummy::StHbtThPairDummy() : StHbtThPair() 
{mWeightNum=mWeightDen=1.;mWeightOk=true;};

void StHbtThPairDummy::Set(const StHbtPair* aPair){
  mMeasPair=aPair;

}

void StHbtThPairDummy::setVariables(const StHbtPair*){
  
}
