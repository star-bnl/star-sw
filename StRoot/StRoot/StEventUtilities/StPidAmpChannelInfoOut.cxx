/***************************************************************************
 *
 * $Id: StPidAmpChannelInfoOut.cxx,v 1.2 2000/08/16 12:46:01 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             TObject version of StPidAmpChannelInfo
 ***************************************************************************
 *
 * $Log: StPidAmpChannelInfoOut.cxx,v $
 * Revision 1.2  2000/08/16 12:46:01  aihong
 * bug killed
 *
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.3  2000/04/09 16:36:42  aihong
 * change for adapting NHitDcaNet added
 *
 * Revision 1.2  2000/03/24 15:11:14  aihong
 * add PrintContent()
 *
 * Revision 1.1.1.1  2000/03/09 17:48:33  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpChannelInfoOut.h"

ClassImp(StPidAmpChannelInfoOut)

StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(){}
StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(const StPidAmpChannelInfoOut& infoOut){
    mNHitsStart=infoOut.mNHitsStart;
    mNHitsEnd  =infoOut.mNHitsEnd;
    mPtStart   =infoOut.mPtStart;
    mPtEnd     =infoOut.mPtEnd;
    mDcaStart  =infoOut.mDcaStart;
    mDcaEnd    =infoOut.mDcaEnd;
}


StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd){

   mNHitsStart=nhitsStart;
   mNHitsEnd  =nhitsEnd;
   mPtStart   =ptStart;
   mPtEnd     =ptEnd;
}

StPidAmpChannelInfoOut::~StPidAmpChannelInfoOut(){}

StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd, Double_t dcaStart, Double_t dcaEnd){

   mNHitsStart  = nhitsStart;
   mNHitsEnd    = nhitsEnd;
   mPtStart     = ptStart;
   mPtEnd       = ptEnd;
   mDcaStart    = dcaStart;
   mDcaEnd      = dcaEnd;
}

void StPidAmpChannelInfoOut::SetNHitsRange(Int_t nhitsStart, Int_t nhitsEnd){
    mNHitsStart=nhitsStart;
    mNHitsEnd  =nhitsEnd;
}

void StPidAmpChannelInfoOut::SetPtRange(Double_t ptStart, Double_t ptEnd){
    mPtStart = ptStart;
    mPtEnd   = ptEnd;
}

void  StPidAmpChannelInfoOut::SetDcaRange(Double_t dcaStart, Double_t dcaEnd){
    mDcaStart =dcaStart;
    mDcaEnd   =dcaEnd;
}

Int_t StPidAmpChannelInfoOut::NHitsStart() const {return mNHitsStart;}



Int_t StPidAmpChannelInfoOut::NHitsEnd() const {return mNHitsEnd;}



Double_t StPidAmpChannelInfoOut::PtStart() const {return mPtStart;}



Double_t StPidAmpChannelInfoOut::PtEnd() const {return mPtEnd;}



Double_t StPidAmpChannelInfoOut::DcaStart() const {return mDcaStart;}
Double_t StPidAmpChannelInfoOut::DcaEnd()   const {return mDcaEnd;}

Bool_t StPidAmpChannelInfoOut::IsInChannel(Int_t nhits,Double_t pt){

 return ((nhits<mNHitsEnd)&&(nhits>=mNHitsStart)&&(pt<mPtEnd)&&(pt>=mPtStart));
}

Bool_t StPidAmpChannelInfoOut::IsInChannel(Int_t nhits,Double_t pt,Double_t dca){

 return ((nhits<mNHitsEnd)&&(nhits>=mNHitsStart)&&(pt<mPtEnd)&&(pt>=mPtStart)&&(dca<mDcaEnd)&&(dca>=mDcaStart));
}

void StPidAmpChannelInfoOut::PrintContent(){
  cout<<"NHitsStart: "<<NHitsStart()<<" NHitsEnd: "<<NHitsEnd()<<endl;
  cout<<"PtStart:    "<<PtStart()   <<" PtEnd:    "<<PtEnd()<<endl;
  cout<<"DcaStart:   "<<DcaStart()  <<" DcaEnd:   "<<DcaEnd()<<endl;
  cout<<endl;
}


ostream& operator<<(ostream& s, const StPidAmpChannelInfoOut& infoOut){

  s<<"NHitsStart: "<<infoOut.NHitsStart()<<" NHitsEnd: "<<infoOut.NHitsEnd()<<endl;
  s<<"PtStart:    "<<infoOut.PtStart()   <<" PtEnd:    "<<infoOut.PtEnd()<<endl;

  s<<"DcaStart:   "<<infoOut.DcaStart() <<" DcaEnd:   "<<infoOut.DcaEnd()<<endl;
  s<<endl;
  return s;
}
