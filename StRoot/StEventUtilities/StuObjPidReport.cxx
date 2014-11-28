/***************************************************************************
 *
 * $Id: StuObjPidReport.cxx,v 1.1 2000/08/15 23:04:18 aihong Exp $
 *
 * Author:Aihong Tang. Kent State University
 *        Send questions to aihong@cnr.physics.kent.edu 
 ***************************************************************************
 *
 *  An object to hold PID info. in a phase space so that it could be saved.
 *
 ***************************************************************************
 *
 * $Log: StuObjPidReport.cxx,v $
 * Revision 1.1  2000/08/15 23:04:18  aihong
 * speed it up by looking up table
 *
 *
 **************************************************************************/
#include "StuObjPidReport.h"
    
ClassImp(StuObjPidReport)

StuObjPidReport::StuObjPidReport(){

  for (int i=0; i<3; i++){
              PID[i]=-999;
              mProb[i]=0.0;
  }
 
              mExtrap=kFALSE;
}

StuObjPidReport::StuObjPidReport(const StuObjPidReport& report){

     PID[0]=report.PID[0];
     PID[1]=report.PID[1];
     PID[2]=report.PID[2];

     mProb[0]=report.PID[0];
     mProb[1]=report.PID[1];
     mProb[2]=report.PID[2];
     mExtrap =report.mExtrap;

}






StuObjPidReport::StuObjPidReport(Int_t id0,Int_t id1, Int_t id2,Double_t prob0, Double_t prob1, Double_t prob2,Bool_t extrap){

     PID[0]=id0;
     PID[1]=id1;
     PID[2]=id2;

     mProb[0]=prob0;
     mProb[1]=prob1;
     mProb[2]=prob2;
     mExtrap =extrap;
}

StuObjPidReport::~StuObjPidReport(){}

void StuObjPidReport::SetPID(Int_t* idAry){

    PID[0]=idAry[0];
    PID[1]=idAry[1];
    PID[2]=idAry[2];
}

void StuObjPidReport::SetProb(Double_t* probAry){

    mProb[0]=probAry[0];
    mProb[1]=probAry[1];
    mProb[2]=probAry[2];
}


void StuObjPidReport::SetExtrap(Bool_t extrap){
  
     mExtrap=extrap;
}



Int_t*    StuObjPidReport::GetPIDArray(){ return &PID[0];}
Double_t* StuObjPidReport::GetProbArray(){ return &mProb[0];}



Int_t      StuObjPidReport::GetPID(Int_t idx){ return PID[idx];}
Double_t   StuObjPidReport::GetProb(Int_t idx){ return mProb[idx];}



Bool_t    StuObjPidReport::GetExtrapTag(){ return mExtrap;}



//following block for debugging purpose
/* 
void StuObjPidReport::SetCharge(Int_t z){mCharge=z;}
void StuObjPidReport::SetDca(Double_t dca){mDca=dca;}
void StuObjPidReport::SetNHits(Int_t nhits){mNHits=nhits;}
void StuObjPidReport::SetPt(Double_t pt){mPt=pt;}
void StuObjPidReport::SetDedx(Double_t dedx){mDedx=dedx;}
void StuObjPidReport::SetRig(Double_t  rig){mRig=rig;}
     

Int_t    StuObjPidReport::GetCharge(){return mCharge;}
Double_t StuObjPidReport::GetDca(){return mDca;}
Int_t    StuObjPidReport::GetNHits(){return mNHits;}
Double_t StuObjPidReport::GetPt(){return mPt;}
Double_t StuObjPidReport::GetDedx(){return mDedx;}
Double_t StuObjPidReport::GetRig(){return mRig;}
*/     

