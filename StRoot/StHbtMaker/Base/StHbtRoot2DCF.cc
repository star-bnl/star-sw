/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation on Root2DCF
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/Base/StHbtRoot2DCF.hh"
#include <Stsstream.h>

#ifdef __ROOT__
ClassImp(StHbtRoot2DCF)
#endif
//____________________________
StHbtRoot2DCF::StHbtRoot2DCF(
   char* aHTitle, int aNBinsX, double aHLoX, double aHHiX,
   int aNBinsY, double aHLoY, double aHHiY)
  : StHbtCorrFctn(),StHbtNamed(),mHLo(aHLoX),mHHi(aHHiX),mHLoY(aHLoY),mHHiY(aHHiY)

{
 
  StHbtNamed::SetName(aHTitle);  
  char* tName=new char[strlen(GetName())+4];
  strcpy(tName+3,GetName());
 
  memcpy(tName,"Num",3);
  mNumerator = new StHbt2DHisto(tName,GetName(),aNBinsX,aHLoX,aHHiX,aNBinsY,mHLoY,mHHiY);

  memcpy(tName,"Den",3);
  mDenominator = new StHbt2DHisto(tName,GetName(),aNBinsX,mHLo,mHHi,aNBinsY,mHLoY,mHHiY);

  memcpy(tName,"Rat",3);
  mRatio = new StHbt2DHisto(tName,GetName(),aNBinsX,mHLo,mHHi,aNBinsY,mHLoY,mHHiY);

  delete [] tName;
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();
}
StHbtRoot2DCF::StHbtRoot2DCF(const StHbtRoot2DCF& cf) 
: StHbtNamed(cf),mHLo(cf.mHLo),mHHi(cf.mHHi),mHLoY(cf.mHLoY),mHHiY(cf.mHHiY) {

mNumerator = new StHbt2DHisto(*(cf.mNumerator));
mDenominator = new StHbt2DHisto(*(cf.mDenominator));
mRatio = new StHbt2DHisto(*(cf.mRatio));

}


//____________________________
StHbtRoot2DCF::~StHbtRoot2DCF(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}


void StHbtRoot2DCF::SetName( const char* aName) {
StHbtNamed::SetName(aName);
int tLen=strlen(GetName());

char* tHName=new char[tLen+4];
memcpy(tHName+3,GetName(),tLen+1);

memcpy(tHName,"Num",3);
mNumerator->SetTitle(GetName());
mNumerator->SetName(tHName);

memcpy(tHName,"Den",3);
mDenominator->SetTitle(GetName());
mDenominator->SetName(tHName);

memcpy(tHName,"Rat",3);
mRatio->SetTitle(GetName());
mRatio->SetName(tHName);

delete [] tHName;
}

//_________________________
void StHbtRoot2DCF::Finish(){
  mRatio->Divide(mNumerator,mDenominator,1.0,1.0);

  if ((int)mNumerator->GetEntries()==(int)mDenominator->GetEntries()){
    // This CF is a Theoretical one : error mus be reprocessed
    int i;
    int tNcell=mRatio->GetNbinsX()*mRatio->GetNbinsY()*mRatio->GetNbinsZ();
    for(i=0;i<tNcell;i++){
      if (mDenominator->GetBinContent(i)) {
	mRatio->SetBinError(i,
			    ::sqrt((::pow(mNumerator->GetBinError(i),2)/mDenominator->GetBinContent(i)-
				  ::pow(mRatio->GetBinContent(i),2))/mDenominator->GetBinContent(i)));
      } else {
	mRatio->SetBinError(i,0.);
      }
    }
  }
}

//____________________________
StHbtString StHbtRoot2DCF::Report(){
  ostrstream tStr; 
  tStr << GetName() << " Correlation Function Report:"<< endl;
  tStr << "Number of entries in numerator: " << mNumerator->GetEntries() << endl;;
  tStr << "Number of entries in denominator: " << mDenominator->GetEntries() << endl;
  StHbtString returnThis = tStr.str();
  return returnThis;
}
