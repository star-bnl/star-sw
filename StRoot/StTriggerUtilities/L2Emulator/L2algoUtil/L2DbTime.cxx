

#include <string>
#include <iostream>
#include <fstream>

#include "L2DbTime.h"

//
// specification of L2DbTime
//



// ---------------------------------------------------------------------------
L2DbTime::L2DbTime()
{

  Int_t mydate=-1111;
  Int_t mytime=-1111;

  mTag=mPedFile=mMaskFile="EMPTY";
  mDateStart=mydate;
  mDateFinish=mydate;
  mTimeStart=mytime;
  mTimeFinish=mytime;
}

Bool_t L2DbTime::valid( Int_t mydate, Int_t mytime )
{

  Bool_t inRange = mydate >= mDateStart && mydate <= mDateFinish;
  if ( mydate==mDateStart ) inRange &= mytime >= mTimeStart;
  if ( mydate==mDateFinish ) inRange &= mytime < mTimeFinish;
  return inRange;

}

Bool_t L2DbTime::eof()
{
  return mTag.Contains("EndOfFile") || mTag.Contains("EOF");
}

Bool_t L2DbTime::comment()
{
  return mTag.Contains("#");
}

std::ifstream &L2DbTime::read( std::ifstream &in )
{
  Char_t buf1[128], buf2[128], buf3[128];
 READ:
  if ( in.eof() ) return in;

  in >> buf1; mTag=buf1;
  if ( !TString(mTag).Contains(":") ) goto READ;
  in >> mDateStart;
  in >> mTimeStart;
  in >> mDateFinish;
  in >> mTimeFinish;
  in >> buf2; mPedFile=buf2;
  in >> buf3; mMaskFile=buf3;

  return in;
}

void L2DbTime::print()
{
  std::cout << Form("  L2DbTime::=%s=\t%7i.%i\t%7i.%i\t%s\t%s",mTag.Data(),mDateStart,mTimeStart,mDateFinish,mTimeFinish,mPedFile.Data(),mMaskFile.Data()) << std::endl;
}

