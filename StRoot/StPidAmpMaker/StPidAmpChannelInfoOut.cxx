/***************************************************************************
 *
 * $Id: StPidAmpChannelInfoOut.cxx,v 1.1.1.1 2000/03/09 17:48:33 aihong Exp $
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
    //mXStart  =infoOut.mXStart;
    //mXEnd    =infoOut.mXEnd;
}


StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd){

   mNHitsStart=nhitsStart;
   mNHitsEnd  =nhitsEnd;
   mPtStart   =ptStart;
   mPtEnd     =ptEnd;
}

StPidAmpChannelInfoOut::~StPidAmpChannelInfoOut(){}

//StPidAmpChannelInfoOut::StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd, Double_t xStart, Double_t xEnd){

//   mNHitsStart=nhitsStart;
//   mNHitsEnd  =nhitsEnd;
//   mPtStart   =ptStart;
//   mPtEnd     =ptEnd;
//   mXStart    =xStart;
//   mXEnd      =xEnd;
//}

void StPidAmpChannelInfoOut::SetNHitsRange(Int_t nhitsStart, Int_t nhitsEnd){
    mNHitsStart=nhitsStart;
    mNHitsEnd  =nhitsEnd;
}

void StPidAmpChannelInfoOut::SetPtRange(Double_t ptStart, Double_t ptEnd){
    mPtStart = ptStart;
    mPtEnd   = ptEnd;
}

//void  StPidAmpChannelInfoOut::SetXRange(Double_t xStart, Double_t xEnd){
//    mXStart =xStart;
//    mXEnd   =xEnd;
//}

Int_t StPidAmpChannelInfoOut::NHitsStart() const {return mNHitsStart;}



Int_t StPidAmpChannelInfoOut::NHitsEnd() const {return mNHitsEnd;}



Double_t StPidAmpChannelInfoOut::PtStart() const {return mPtStart;}



Double_t StPidAmpChannelInfoOut::PtEnd() const {return mPtEnd;}



//Double_t StPidAmpChannelInfoOut::XStart() const {return mXStart;}
//Double_t StPidAmpChannelInfoOut::XEnd()   conts {return mXEnd;}

Bool_t StPidAmpChannelInfoOut::IsInChannel(Int_t nhits,Double_t pt){

 return ((nhits<=mNHitsEnd)&&(nhits>mNHitsStart)&&(pt<=mPtEnd)&&(pt>mPtStart));
}

//Bool_t StPidAmpChannelInfoOut::IsInChannel(Int_t nhits,Double_t pt,Double_t x){

// return ((nhits<=mNHitsEnd)&&(nHits>mNHitsStart)&&(pt<=mPtEnd)&&(pt>mPtStart)&&(x<=mXEnd)&&(x>mXStart));
//}
