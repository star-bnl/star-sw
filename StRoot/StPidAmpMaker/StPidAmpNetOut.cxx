/***************************************************************************
 *
 * $Id: StPidAmpNetOut.cxx,v 1.1.1.1 2000/03/09 17:48:33 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             This is the object to be written out.
 *             It contains: (if all filled)
 *             1.Parameters for describing the band
 *             2.Parameters for describing the amplitude
 *             3.Parameters for describing the resolution
 *             4.Parameters for calibration
 *             5.Geant ID for the net type
 ***************************************************************************
 *
 * $Log: StPidAmpNetOut.cxx,v $
 * Revision 1.1.1.1  2000/03/09 17:48:33  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpNetOut.h"

ClassImp(StPidAmpNetOut)

StPidAmpNetOut::StPidAmpNetOut(){
    mCalibConst=0.0;
}

StPidAmpNetOut::StPidAmpNetOut(const StPidAmpNetOut& netOut){
   mGeantID     =netOut.mGeantID;
   mBandParArray=netOut.mBandParArray;
   mAmpParArray =netOut.mAmpParArray;
   mResoParArray=netOut.mResoParArray;
   mCalibConst  =netOut.mCalibConst;
   SetName(netOut.GetName());
   SetTitle(netOut.GetTitle());
  }


StPidAmpNetOut::StPidAmpNetOut(Text_t* name,Text_t* title,Int_t id,TArrayD bandParAry, TArrayD ampParAry, TArrayD linrParAry) : TNamed(name,title) {

  mGeantID=id;
  mBandParArray=bandParAry;
  mAmpParArray=ampParAry;
  mResoParArray=linrParAry;
  mCalibConst=0.0;
  }

  StPidAmpNetOut::~StPidAmpNetOut(){}

void StPidAmpNetOut::SetBandParArray(TArrayD bandParAry){
  mBandParArray=bandParAry;
}

void StPidAmpNetOut::SetAmpParArray(TArrayD ampParAry){
  mAmpParArray=ampParAry;
}

void StPidAmpNetOut::SetResoParArray(TArrayD linrParAry){
  mResoParArray=linrParAry;
}

void StPidAmpNetOut::SetGeantID(Int_t id){
  mGeantID=id;
}
void StPidAmpNetOut::SetCalibConst(Double_t cal){
  mCalibConst=cal;
}


TArrayD   StPidAmpNetOut::GetBandParArray(){ return mBandParArray;}
TArrayD   StPidAmpNetOut::GetAmpParArray(){return mAmpParArray;}
TArrayD   StPidAmpNetOut::GetResoParArray(){return mResoParArray;}
Int_t     StPidAmpNetOut::GetNBandPars(){return mBandParArray.GetSize();}
Int_t     StPidAmpNetOut::GetNAmpPars(){return mAmpParArray.GetSize();}
Int_t     StPidAmpNetOut::GetNResoPars(){return mResoParArray.GetSize();}

Int_t     StPidAmpNetOut::GetGeantID(){return mGeantID;}
Double_t  StPidAmpNetOut::GetCalibConst(){return mCalibConst;}
