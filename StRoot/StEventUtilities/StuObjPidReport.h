/***************************************************************************
 *
 * $Id: StuObjPidReport.h,v 1.2 2003/09/02 17:58:09 perev Exp $
 *
 * Author:Aihong Tang. Kent State University
 *        Send questions to aihong@cnr.physics.kent.edu 
 ***************************************************************************
 *
 *  An object to hold PID info. in a phase space so that it could be saved.
 *
 ***************************************************************************
 *
 * $Log: StuObjPidReport.h,v $
 * Revision 1.2  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/08/15 23:04:18  aihong
 * speed it up by looking up table
 *
 *
 **************************************************************************/
#ifndef ROOT_StuObjPidReport
#define ROOT_StuObjPidReport

#ifndef ROOT_TObject
//*KEEP, TObject.
#include "TObject.h"
//*KEND.
#endif 

#include <Stiostream.h>
#include "TNamed.h"


//a place holder for PID stuff.

class StuObjPidReport : public TNamed {

 public:

      StuObjPidReport();
      StuObjPidReport(const StuObjPidReport&);
      StuObjPidReport(Int_t id0,Int_t id1, Int_t id2,Double_t prob0, Double_t prob1, Double_t prob2,Bool_t extrap);

      virtual ~StuObjPidReport();

      void SetPID(Int_t* idAry);
      void SetProb(Double_t* probAry);
      void SetExtrap(Bool_t extrap);

      //  following block for debugging purppoose
      /*
      void SetCharge(Int_t z);
      void SetDca(Double_t dca);
      void SetNHits(Int_t nhits);
      void SetPt(Double_t pt);
      void SetDedx(Double_t dedx);
      void SetRig(Double_t  rig);
     

      Int_t    GetCharge();
      Double_t GetDca();
      Int_t    GetNHits();
      Double_t GetPt();
      Double_t GetDedx();
      Double_t GetRig();
     */


      
      Int_t*       GetPIDArray();
      Double_t*    GetProbArray();
      Bool_t       GetExtrapTag();

      Int_t      GetPID(Int_t idx);
      Double_t   GetProb(Int_t idx);

 private:

      Int_t      PID[3];
      Double_t   mProb[3];
      Bool_t     mExtrap;

      Int_t    mCharge;
      Double_t mDca;
      Int_t    mNHits;
      Double_t mPt;
      Double_t mDedx;
      Double_t mRig;


 ClassDef(StuObjPidReport,1)


};

#endif
