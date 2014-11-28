/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Implementation of StHbtThCFGaussSize
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtThCFGaussSize.h"
#include "StHbtMaker/ThCorrFctn/StHbtThCorrFctnCollection.hh"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"
#include "StHbtMaker/Base/StHbtRoot1DCF.hh"

#include <string.h>
#include <Stsstream.h>

StHbtThCFGaussSize::StHbtThCFGaussSize() 
{cout << "default constructor - must not be called "<<endl ; exit(0);}

StHbtThCFGaussSize::StHbtThCFGaussSize( const char* aName, double aX, double aY,double aZ, double aT) :  mSizeX(aX), mSizeY(aY), mSizeZ(aZ), mTime(aT) {
mName=new char[strlen(aName)+1];
strcpy(mName,aName);
}

StHbtThCFGaussSize::~StHbtThCFGaussSize() 
{ delete mName;};

StHbtThCFGaussSize* StHbtThCFGaussSize::Copy( const char* aName, double aX, double aY,double aZ, double aT)  {
  StHbtThCFGaussSize* NewSize=new  StHbtThCFGaussSize(aName,aX,aY,aZ,aT);
  if (mThCorrFctnColl.size()>0) {
     StHbtThCorrFctnIterator iter;
     for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
       int tBaseNameLen=strlen((*iter)->GetName())-strlen(mName);
       char* tCFName=new char[tBaseNameLen+strlen(aName)+1];
       memcpy(tCFName,(*iter)->GetName(),tBaseNameLen);
       strcpy(tCFName+tBaseNameLen,aName);
       StHbtThCorrFctn *NewCF=(*iter)->ThClone(); 
       NewCF->SetName(tCFName);
       delete [] tCFName;
       NewSize->mThCorrFctnColl.push_back(NewCF);
     };
  };
  return NewSize;
};

void StHbtThCFGaussSize::AddCorrFctn(const StHbtThCorrFctn* aCF){

  StHbtThCorrFctn* NewCF=aCF->ThClone();
  char* tName=new char[strlen(aCF->GetName())+strlen(mName)+1];
  strcpy(tName, aCF->GetName());
  strcat(tName,mName);
  NewCF->SetName(tName);  
  delete [] tName;
  mThCorrFctnColl.push_back(NewCF);
}

void StHbtThCFGaussSize::FillPair(StHbtThPair* aThPair ){
  StHbtThCorrFctnIterator iter;
  for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
    (*iter)->AddNum(aThPair);
    (*iter)->AddDen(aThPair);
  };
}

void StHbtThCFGaussSize::Finish() {
  if (mThCorrFctnColl.size()>0) {
    StHbtThCorrFctnIterator iter;
    for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){
	(*iter)->Finish();
    }
  }  
}

StHbtString StHbtThCFGaussSize::Report() {
  ostrstream tStr; 
  tStr << "Gaussian Size Report - Size=(X="<< mSizeX <<"Y="<< mSizeY 
       <<"Z="<< mSizeZ<< "T="<< mTime << ")" << endl;
  tStr << mThCorrFctnColl.size() << " Correlation Function Plugged " << endl;
  if (mThCorrFctnColl.size()>0) {
    StHbtThCorrFctnIterator iter;
    for (iter=mThCorrFctnColl.begin(); iter!=mThCorrFctnColl.end();iter++){  
	tStr <<	(*iter)->Report() << endl;
    }
  } 
  StHbtString returnThis = tStr.str();
  return returnThis;
}

inline double StHbtThCFGaussSize::GetSizeX() const {return mSizeX;};
inline double StHbtThCFGaussSize::GetSizeY() const {return mSizeY;};
inline double StHbtThCFGaussSize::GetSizeZ() const {return mSizeZ;};
inline double StHbtThCFGaussSize::GetTime() const {return mTime;};

