/***************************************************************************
 *
 * $Id: StHiMicroEvent.h,v 1.3 2002/05/31 21:54:00 jklay Exp $                                                         
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This is a uDST for highpt Analysis.               
 *               
 ***************************************************************************
 *
 * $Log: StHiMicroEvent.h,v $
 * Revision 1.3  2002/05/31 21:54:00  jklay
 * Added ZDC Vertex z position
 *
 * Revision 1.2  2002/04/02 23:34:52  jklay
 * Added L3RichTrigger information
 *
 * Revision 1.1  2002/04/02 19:36:15  jklay
 * Bums highpt uDST format
 *
 *
 **************************************************************************/
/* 
   basically copies jeffs, and the flow teams code. 
   allocates the memory for the tracks and hits on the heap
   only the first time a StHiMicroEvent object is created.

*/

#ifndef StHiMicroEvent_H
#define StHiMicroEvent_H

#include "TObject.h"
#include "TClonesArray.h"
#include "StHiMicroTrack.h"
#include "StHiMicroHit.h"

class StHiMicroEvent : public TObject {
 public:
  StHiMicroEvent();
  virtual ~StHiMicroEvent();
  
  void Clear(Option_t* option="");
  void AddTrack(StHiMicroTrack*); 
  void AddHit(StHiMicroHit*);

  TClonesArray* tracks() const { return mTracks; }
  TClonesArray* hits() const { return mHits; }

  Float_t VertexZ()				const { return mVertexZ; }
  Float_t VertexY()				const { return mVertexY; }
  Float_t VertexX()				const { return mVertexX; }	
  Int_t   OriginMult()				const { return mOriginMult; }
  Int_t   CentMult()				const { return mCentMult; }
  Int_t   Centrality()				const { return mCentrality; }
  Int_t   NUncorrectedNegativePrimaries()	const { return mNUncorrectedNegativePrimaries; }
  Int_t   NUncorrectedPrimaries()		const { return mNUncorrectedPrimaries; }
  Int_t   NAllGlobals()				const { return mNAllGlobals; }
  Int_t   NFlagGlobals()			const { return mNFlagGlobals; }
  Int_t   NGoodGlobals()			const { return mNGoodGlobals; }
  Int_t   NGoodGlobalsA()			const { return mNGoodGlobalsA; }
  Int_t   NGoodGlobalsB()			const { return mNGoodGlobalsB; }
  Int_t   NGoodGlobalsC()			const { return mNGoodGlobalsC; }
  Int_t   NGoodGlobalsD()			const { return mNGoodGlobalsD; }
  Int_t	  NGoodGlobalsE()			const { return mNGoodGlobalsE; }
  UInt_t  L0TriggerWord()			const { return mL0TriggerWord; }
  Bool_t  L3UnbiasedTrigger()			const { return mL3UnbiasedTrigger; }
  Bool_t  L3RichTrigger()			const { return mL3RichTrigger; }
  Double_t CenterOfMassEnergy()			const { return mCenterOfMassEnergy; }
  Double_t MagneticField()			const { return mMagneticField; }
  Short_t  BeamMassNumberEast()			const { return mBeamMassNumberEast; }
  Short_t  BeamMassNumberWest()			const { return mBeamMassNumberWest; }
  Int_t    EventId()				const { return mEventId; }
  Int_t    RunId()				const { return mRunId; }
  Int_t    NTrack()				const { return mNTrack; }
  Float_t  CTB()				const { return mCTB; }
  Float_t  ZDCe()				const { return mZDCe; }
  Float_t  ZDCw()				const { return mZDCw; }
  Float_t  ZDCVertexZ()				const { return mZDCVertexZ; }
  Int_t    NHit()				const { return mNHit; }

  void SetCentrality(Int_t);

  void SetVertexZ(Float_t val)				{ mVertexZ=val; }
  void SetVertexY(Float_t val)				{ mVertexY=val; }
  void SetVertexX(Float_t val)				{ mVertexX=val; }
  void SetOriginMult(Int_t val)				{ mOriginMult=val; }
  void SetCentMult(Int_t val)				{ mCentMult=val; }
  void SetNUncorrectedNegativePrimaries(Int_t val)	{ mNUncorrectedNegativePrimaries=val; }
  void SetNUncorrectedPrimaries(Int_t val)		{ mNUncorrectedPrimaries=val; }
  void SetNAllGlobals(Int_t val)			{ mNAllGlobals=val; }
  void SetNFlagGlobals(Int_t val)			{ mNFlagGlobals=val; }
  void SetNGoodGlobals(Int_t val)			{ mNGoodGlobals=val; }
  void SetNGoodGlobalsA(Int_t val)			{ mNGoodGlobalsA=val; }
  void SetNGoodGlobalsB(Int_t val)			{ mNGoodGlobalsB=val; }
  void SetNGoodGlobalsC(Int_t val)			{ mNGoodGlobalsC=val; }
  void SetNGoodGlobalsD(Int_t val)			{ mNGoodGlobalsD=val; }
  void SetNGoodGlobalsE(Int_t val)			{ mNGoodGlobalsE=val; }
  void SetL0TriggerWord(UInt_t val)			{ mL0TriggerWord=val; }
  void SetL3UnbiasedTrigger(Bool_t val)			{ mL3UnbiasedTrigger=val; }
  void SetL3RichTrigger(Bool_t val)			{ mL3RichTrigger=val; }
  void SetCenterOfMassEnergy(Double_t val)		{ mCenterOfMassEnergy=val; }
  void SetMagneticField(Double_t val)			{ mMagneticField=val; }
  void SetBeamMassNumberEast(Short_t val)		{ mBeamMassNumberEast=val; }	
  void SetBeamMassNumberWest(Short_t val)		{ mBeamMassNumberWest=val; }	
  void SetEventId(Int_t val)				{ mEventId=val; }	
  void SetRunId(Int_t val)				{ mRunId=val; }
  void SetNTrack(Int_t val)				{ mNTrack=val; }	
  void SetCTB(Float_t val)				{ mCTB=val; }	
  void SetZDCe(Float_t val)				{ mZDCe=val; }	
  void SetZDCw(Float_t val)				{ mZDCw=val; }
  void SetZDCVertexZ(Float_t val)			{ mZDCVertexZ=val; }
  void SetNHit(Int_t val)				{ mNHit=val; }
  
  private:
  Float_t   mVertexZ;
  Float_t   mVertexX;    		//Self-explanatory
  Float_t   mVertexY;
  Int_t     mOriginMult; 		//stEvent->primaryVertex()->numberOfDaughters()
  Int_t     mCentMult;			//|Eta|<0.5, flag >0, to go with Zhangbu's centrality definition 
  Int_t     mCentrality;		//Uses Zhangbu's numbers for Year 2, based on definition of CentMult above
  Int_t     mNUncorrectedNegativePrimaries; // manuel's h-
  Int_t     mNUncorrectedPrimaries;

  Int_t     mNAllGlobals;
  Int_t     mNFlagGlobals;	//flag > 0
  Int_t     mNGoodGlobals;	//25 fitpts, flag >0, charge != 0
  Int_t     mNGoodGlobalsA;	//10 fitpts, flag >0, charge != 0
  Int_t     mNGoodGlobalsB;	//15 fitpts, flag >0, charge != 0
  Int_t     mNGoodGlobalsC;	//20 fitpts, flag >0, charge != 0
  Int_t     mNGoodGlobalsD;	//30 fitpts, flag >0, charge != 0
  Int_t     mNGoodGlobalsE;	//25 fitpts, all flags, charge != 0

  //** new 01/28/02
  UInt_t    mL0TriggerWord; // l0 trigger word for > y1 data
  Bool_t    mL3UnbiasedTrigger; // for l3 triggered events, unbiased or not?
  Bool_t    mL3RichTrigger; // for l3 Rich triggered events, see http://ikf1.star.bnl.gov/L3doc/algorithms/
  Double_t  mCenterOfMassEnergy;
  Double_t  mMagneticField;
  Short_t   mBeamMassNumberEast;
  Short_t   mBeamMassNumberWest;
  // ** end new
  Int_t     mEventId;
  Int_t     mRunId;
  Int_t     mNTrack;     // numbers of tracks in the track branch
  Float_t   mCTB;        
  Float_t   mZDCe;
  Float_t   mZDCw;
  Float_t   mZDCVertexZ;  //Info from ZDC timing can be used to assess vertex efficiency
  Int_t     mNHit;       //number of hist in the hit branch

//These were always private
  static TClonesArray* mSTracks; //! flag to create clones array
  TClonesArray*        mTracks;
 
  static TClonesArray* mSHits;   //!
  TClonesArray*        mHits;    //

  ClassDef(StHiMicroEvent,1)
  
};

#endif

