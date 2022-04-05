#ifndef StEvtHddr_h
#define StEvtHddr_h
#include <TDatime.h>
#include "TDataSet.h"
struct EvtHddr_st;

class StEvtHddr : public TDataSet
{
public:
  StEvtHddr(TDataSet *parent=0);
  StEvtHddr(const StEvtHddr &hddr, const char *name=0);
 ~StEvtHddr(){};
  StEvtHddr &operator=(const StEvtHddr &hddr);
  virtual TObject *Clone(const char *name=0) const {return new StEvtHddr(*this,name);}
  virtual TObject *Clone()      {return ((const StEvtHddr*)this)->Clone();};
//		Get methods

  Int_t 	GetRunNumber()    const {return mRunNumber;};
  Int_t 	GetOldRunNumber() const {return mOldRunNumber;};
  const Char_t *GetEventType()    const {return (const Char_t*)mEventType;};
  UInt_t 	GetTriggerMask()  const {return mTriggerMask;};
  UInt_t 	GetInputTriggerMask()    const {return mInputTriggerMask;};
  Float_t       GetCenterOfMassEnergy()  const {return mCenterOfMassEnergy;};
  UInt_t       	GetBunchCrossingNumber(int i) const {return mBunchCrossingNumber[i];};
  Int_t       	GetAEast()        const {return mAEast;};
  Int_t       	GetZEast() 	  const {return mZEast;};
  Int_t       	GetAWest() 	  const {return mAWest;};
  Int_t       	GetZWest() 	  const {return mZWest;};
  Float_t     	GetLuminosity()   const {return mLuminosity;};
  Float_t     	GetBImpact()      const {return mBImpact;};
  Float_t     	GetPhiImpact()    const {return mPhImpact;};
  Float_t     	GetPhImpact()     const {return GetPhiImpact();};
  UInt_t     	GetUTime() 	  const ;
  Int_t     	GetDate()  	  const {return mEventTime.GetDate();};
  Int_t     	GetTime()  	  const {return mEventTime.GetTime();};
  const TDatime  &GetDateTime()    const {return mEventTime;};
  const TDatime &GetProdDateTime() const {return mProdTime;};
  Int_t     	GetIventNumber()  const {return mIventNumber;};
  Int_t     	GetEventSize()    const {return mEventSize;};
  Int_t     	GetEventNumber()  const {return mEventNumber;};
  Int_t     	GetGenerType()    const {return mGenerType;};
  Int_t     	IsNewRun()    const {return (mRunNumber!=mOldRunNumber);};
  void          FillTag(EvtHddr_st *tag);  
//		Set methods

  void          SetRunNumber(int run)	{mOldRunNumber=mRunNumber;mRunNumber=run;};
  void		SetEventType(const Char_t *type){mEventType=type;};
  void 		SetTriggerMask(UInt_t tm)	{mTriggerMask=tm;};
  void 		SetInputTriggerMask(UInt_t tm)	{mInputTriggerMask=tm;};
  void       	SetBunchCrossingNumber(UInt_t bcn0,UInt_t bcn1)
		{mBunchCrossingNumber[0]=bcn0;mBunchCrossingNumber[1]=bcn1;}
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
  void          SetProdDateTime(UInt_t ut=0);
  void     	SetIventNumber(int iv)	{mIventNumber=iv;};
  void     	SetEventSize(int is)	{mEventSize=is;};
  void     	SetEventNumber(int ev)	{mEventNumber=ev;};
  void     	SetGenerType(int g)	{mGenerType=g;};
  void     	Print(Option_t* option="") const;

//		Data Members
protected:
    Int_t	mRunNumber;
    Int_t	mOldRunNumber;		//!
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
    Float_t	mBImpact;		//Impact parameter
    Float_t	mPhImpact;		//Phi angle of impact
    Int_t       mGenerType;		//Gener type see below
    UInt_t	mBunchCrossingNumber[2];
    Int_t       mIventNumber;    	//sequential number in DAQ/Geant file
    Int_t       mEventSize;    		//size of event
    Int_t       mEventNumber;    
//
    TDatime     mEventTime;		//DAQ Time
    TDatime     mProdTime;		//Production time
    TString     mEventType;

ClassDef(StEvtHddr,3)                   // Event header
};

//__________________________________________________________________________________
inline  StEvtHddr::StEvtHddr(const StEvtHddr &hddr,const char *name):TDataSet(name,"")
{ operator=(hddr); }

//__________________________________________________________________________________
struct EvtHddr_st 
{
    Int_t	mRunNumber;
    Int_t	mOldRunNumber;		//!
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
    Float_t	mBImpact;		//Impact parameter
    Float_t	mPhImpact;		//Phi angle of impact
    Int_t       mGenerType;		//Gener type see below
    UInt_t	mBunchCrossingNumber[2];
    Int_t       mIventNumber;    	//sequential number in DAQ/Geant file
    Int_t       mEventSize;    		//size of event
    Int_t       mEventNumber;    
//
//    TDatime     mEventTime;		//DAQ Time
    Double_t    mEventTime;
//    TDatime     mProdTime;		//Production time
    Double_t    mProdTime;
  //    TString     mEventType;
    char        mEventType[16];
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
