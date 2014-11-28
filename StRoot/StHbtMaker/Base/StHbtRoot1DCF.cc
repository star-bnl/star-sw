/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : implementation of Root1DCF
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/Base/StHbtRoot1DCF.hh"
#include <Stsstream.h>
#include <Stiostream.h>

#ifdef __ROOT__
ClassImp(StHbtRoot1DCF)
#endif
//____________________________
StHbtRoot1DCF::StHbtRoot1DCF(
   char* aHTitle, int aHNBins, double aHLo, double aHHi)
  : StHbtCorrFctn(),StHbtNamed(),mHLo(aHLo),mHHi(aHHi)
{  
  StHbtNamed::SetName(aHTitle);  
  char* tName=new char[strlen(GetName())+4];
  strcpy(tName+3,GetName());
    
  memcpy(tName,"Num",3);
  mNumerator = new StHbt1DHisto(tName,GetName(),aHNBins,mHLo,mHHi);

  memcpy(tName,"Den",3);
  mDenominator = new StHbt1DHisto(tName,GetName(),aHNBins,mHLo,mHHi);

  memcpy(tName,"Rat",3);
  mRatio = new StHbt1DHisto(tName,GetName(),aHNBins,mHLo,mHHi);

  delete [] tName;
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();
}

StHbtRoot1DCF::StHbtRoot1DCF(const StHbtRoot1DCF& cf): 
StHbtNamed(cf),mHLo(cf.mHLo),mHHi(cf.mHHi)
 {
  mNumerator=new StHbt1DHisto(*(cf.mNumerator));
  mDenominator=new StHbt1DHisto(*(cf.mDenominator));
  mRatio=new StHbt1DHisto(*(cf.mRatio));
}


//____________________________
StHbtRoot1DCF::~StHbtRoot1DCF(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}

void StHbtRoot1DCF::SetName( const char* aName) {
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
void StHbtRoot1DCF::Finish(){
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
StHbtString StHbtRoot1DCF::Report(){
  ostrstream tStr; 
  tStr << GetName() << " Correlation Function Report:"<< endl;
  tStr << "Number of entries in numerator: " << mNumerator->GetEntries() << endl;;
  tStr << "Number of entries in denominator: " << mDenominator->GetEntries() << endl;
  StHbtString returnThis = tStr.str();
  return returnThis;
}
