#ifndef StEvtHddr_h
#define StEvtHddr_h

#include "TDatime.h"
#include "St_DataSet.h"
struct EvtHddr_st;

class StEvtHddr : public St_DataSet
{
public:
  StEvtHddr(St_DataSet *parent=0);
 ~StEvtHddr(){};
  StEvtHddr &operator=(const StEvtHddr &hddr);
//		Get methods

  Int_t 	GetRunNumber()    const {return mRunNumber;};
  Int_t 	GetOldRunNumber() const {return mOldRunNumber;};
  const Char_t *GetEventType()    const {return (const Char_t*)mEventType;};
  UInt_t 	GetTriggerMask()  const {return mTriggerMask;};
  Float_t       GetCenterOfMassEnergy() const {return mCenterOfMassEnergy;};
  Int_t       	GetAEast()        const {return mAEast;};
  Int_t       	GetZEast() 	  const {return mZEast;};
  Int_t       	GetAWest() 	  const {return mAWest;};
  Int_t       	GetZWest() 	  const {return mZWest;};
  Float_t     	GetLuminosity()   const {return mLuminosity;};
  Float_t     	GetBImpact()      const {return mBImpact;};
  Float_t     	GetPhiImpact()    const {return mPhImpact;};
  Float_t     	GetPhImpact()     const {return GetPhiImpact();};
  UInt_t     	GetUTime() 	  const {return mEventTime.Get();};
  Int_t     	GetDate()  	  const {return ((TDatime *)&mEventTime)->GetDate();};
  Int_t     	GetTime()  	  const {return ((TDatime *)&mEventTime)->GetTime();};
  TDatime       GetDateTime()     const {return mEventTime;};
  TDatime       GetProdDateTime() const {return mProdTime;};
  Int_t     	GetEventNumber()  const {return mEventNumber;};
  Int_t     	GetGenerType()    const {return mGenerType;};
  Int_t     	IsNewRun()    const {return (mRunNumber==mOldRunNumber);};
  void          FillTag(EvtHddr_st *tag);  
//		Set methods

  void          SetRunNumber(int run)	{mOldRunNumber=mRunNumber;mRunNumber=run;};
  void		SetEventType(const Char_t *type){mEventType=type;};
  void 		SetTriggerMask(UInt_t tm)	{mTriggerMask=tm;};
  void 		SetInputTriggerMask(UInt_t tm)	{mInputTriggerMask=tm;};
  void       	SetCenterOfMassEnergy(float e)	{mCenterOfMassEnergy=e;};
  void     	SetBImpact  (float b)  	{mBImpact=b;};
  void     	SetPhiImpact(float p)  	{mPhImpact=p;};
  void    	SetPhImpact (float p)  	{SetPhiImpact(p);};
  void       	SetAEast(int a)		{mAEast=a;};
  void     	SetZEast(int z)		{mZEast=z;};
  void     	SetAWest(int a)		{mAWest=a;};
  void       	SetZWest(int z)		{mZWest=z;};
  void     	SetLuminosity(float lu)	{mLuminosity=lu;};
  void     	SetDateTime(int iDate,int iTime){mEventTime.Set(iDate,iTime);};
  void          SetDateTime(TDatime dt)	{mEventTime=dt;};
  void          SetGMTime(UInt_t ut);
  void          SetProdDateTime()	{mProdTime.Set();};
  void     	SetEventNumber(int ev)	{mEventNumber=ev;};
  void     	SetGenerType(int g)	{mGenerType=g;};
  void     	Print();

//		Data Members
protected:
    Int_t	mRunNumber;
    Int_t	mOldRunNumber;
    Int_t       mId;
    UInt_t 	mInputTriggerMask;
    UInt_t 	mTriggerMask;
    Float_t     mCenterOfMassEnergy;
    Int_t       mAEast;
    Int_t       mZEast;
    Int_t       mAWest;
    Int_t       mZWest;
    Float_t     mLuminosity;
    Float_t     mBeamPolarizationEast[3];
    Float_t     mBeamPolarizationWest[3];
    Float_t	mBImpact;			//Impact parameter
    Float_t	mPhImpact;			//Phi angle of impact
    Int_t       mGenerType;			//Gener type see below
    UInt_t	mBunchCrossingNumber;
    Int_t       mEventNumber;    
//
    TDatime     mEventTime;		//DAQ Time
    TDatime     mProdTime;		//Production time
    TString     mEventType;

ClassDef(StEvtHddr,1)
};
struct EvtHddr_st 
{
    Int_t	mRunNumber;
    Int_t	mOldRunNumber;
    Int_t       mId;
    UInt_t 	mInputTriggerMask;
    UInt_t 	mTriggerMask;
    Float_t     mCenterOfMassEnergy;
    Int_t       mAEast;
    Int_t       mZEast;
    Int_t       mAWest;
    Int_t       mZWest;
    Float_t     mLuminosity;
    Float_t     mBeamPolarizationEast[3];
    Float_t     mBeamPolarizationWest[3];
    Float_t	mBImpact;			//Impact parameter
    Float_t	mPhImpact;			//Phi angle of impact
    Int_t       mGenerType;			//Gener type see below
    UInt_t	mBunchCrossingNumber;
    Int_t       mEventNumber;    
//
//    TDatime     mEventTime;		//DAQ Time
    Int_t       mEventTime;
    Int_t       mEventDate;
//    TDatime     mProdTime;		//Production time
    Int_t       mProdTime;
    Int_t       mProdDate;
  //    TString     mEventType;
};
#endif

#if 0
/*  	mGenerType =

*  1 FRITIOF : MINBIAS
*  2 FRITIOF : CENTRAL
* 10 HBT     : MINBIAS
* 11 HBT     : CENTRAL
* 20 HIJET   : RHICEVT : MINBIAS 
* 21 HIJET   : RHICEVT : CENTRAL
* 22 HIJET   : PLASMA
* 23 HIJET   : LANDAU
* 24 HIJET   : SMOKE
* 25 HIJET   : VOLCANO
* 26 HIJET   : CHIRAL
* 27 HIJET   : PTSIM
* 28 HIJET   : STRANGSIM
* 29 HIJET   : HIFLOW
* 30 HIJING  : REGULAR : MINBIAS 
* 31 HIJING  : REGULAR : CENTRAL 
* 32 HIJING  : JET     : MINBIAS  
* 33 HIJING  : JET     : CENTRAL 
* 40 PYTHIA  : MINBIAS
* 41 PYTHIA  : JET
* 42 PYTHIA  : PHOTON
* 43 PYTHIA  : W
* 50 STARLIGHT
* 51 STARLIGHT gamma gamma
* 52 STARLIGHT photnuc : coherent
* 60 VENUS   : MINBIAS 
* 61 VENUS   : CENTRAL  
* 62 VENUS   : CENTRAL : MEV
* 70 VNI     : NOAFTER : MINBIAS 
* 71 VNI     : AFTER   : MINBIAS 
* 72 VNI     : NOAFTER : CENTRAL 
* 73 VNI     : AFTER   : CENTRAL 
* 74 VNI     : NOAFTER : JET
* 75 VNI     : AFTER   : JET
* 76 VNI     : NOAFTER : PHOTON
* 77 VNI     : AFTER   : PHOTON
* 78 VNI     : NOAFTER : CHIRAL
* 79 VNI     : AFTER   : CHIRAL
* 80 VNI     : CENTRAL : MEV
* 90 RQMD    : MINBIAS
* 91 RQMD    : CENTRAL
* 100 BEAMGAS: VENUS   : MINBIAS
*/
#endif
