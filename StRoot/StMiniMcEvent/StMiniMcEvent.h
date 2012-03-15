/**
 * $Id $
 * $Log $
 * \file  StMiniMcEvent.h
 * \brief Top level class for the MiniMcTree, containing event-wise information and the McTrack, and all TrackPair
 *        collections.
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  
 *
 *  used jeff reid and raimond's makers as references.
*/

#ifndef StMiniMcEvent_H
#define StMiniMcEvent_H

#include "TObject.h"
#include "TClonesArray.h"
#include "StMiniMcPair.h"
#include "StContamPair.h"

enum Category { MC,MATCHED,MERGED,SPLIT,CONTAM,GHOST,MATGLOB};

class StMiniMcEvent : public TObject {
 public:
  StMiniMcEvent();
  virtual ~StMiniMcEvent();
  void Clear(Option_t *option=""); // clear the tracks

  StTinyMcTrack* addMcTrack(StTinyMcTrack* mc=0);
  StMiniMcPair*  addTrackPair(StMiniMcPair* pair=0, Category K=MATCHED);
  StContamPair*  addContamPair(StContamPair* pair=0);

  //  TClonesArray* tracks(Category);
  TClonesArray* tracks(Category) const;

  Int_t		eventId() 			const { return mEventId; }
  Int_t		runId() 			const { return mRunId; }
  Int_t		originMult() 			const { return mOriginMult; }
  Int_t		centralMult() 			const { return mCentralMult; }
  Int_t		centrality() 			const { return mCentrality; }
  Int_t		nUncorrectedNegativePrimaries()	const { return mNUncorrectedNegativePrimaries; }
  Int_t		nUncorrectedPrimaries() 	const { return mNUncorrectedPrimaries; }
  Int_t		nUncorrectedGlobals()  		const { return mNUncorrectedGlobals; }
  Int_t		nFtpcWUncorrectedPrimaries()	const { return mNFtpcWUncorrectedPrimaries; }
  Int_t		nFtpcEUncorrectedPrimaries()	const { return mNFtpcEUncorrectedPrimaries; }
  Int_t		mcMult() 			const { return mMcMult; }
  Int_t		nMcNch() 			const { return mNMcNch; }
  Int_t		nMcFtpcWNch() 			const { return mNMcFtpcWNch; }
  Int_t		nMcFtpcENch() 			const { return mNMcFtpcENch; }
  Int_t		nMcHminus() 			const { return mNMcHminus; }
  Int_t		nMcGlobal() 			const { return mNMcGlobal; }
  Int_t		nMcGoodGlobal20() 		const { return mNMcGoodGlobal20; }
  Int_t		nRcGlobal() 			const { return mNRcGlobal; }
  Int_t		nRcGoodGlobal20() 		const { return mNRcGoodGlobal20; }
  Float_t       vertexX() 			const { return mVertexX; }  
  Float_t       vertexY() 			const { return mVertexY; }
  Float_t       vertexZ() 			const { return mVertexZ; }
  const Float_t*vertexCovMatrix()               const { return &mVertexCovMatrix[0];}
  Float_t       mcVertexX() 			const { return mMcVertexX	; }
  Float_t       mcVertexY() 			const { return mMcVertexY	; }
  Float_t       mcVertexZ() 			const { return mMcVertexZ	; }
  Float_t	centerOfMassEnergy() 		const { return mCenterOfMassEnergy; }
  Float_t	magneticField() 		const { return mMagField	; }
  Float_t	backgroundRate() 		const { return mBackgroundRate	; }
  Short_t	beamMassNumberEast() 		const { return mBeamMassNumberEast; }
  Short_t	beamMassNumberWest() 		const { return mBeamMassNumberWest; }
  Float_t       ctb() 				const { return mCtb		; }
  Float_t       zdcE()				const { return mZdcE		; }
  Float_t       zdcW() 				const { return mZdcW		; }
  Int_t		nMcTrack() 			const { return mNMcTrack	; }     
  Int_t		nMatchedPair() 			const { return mNMatchedPair	; }
  Int_t		nMergedPair()			const { return mNMergedPair	; }
  Int_t		nSplitPair() 			const { return mNSplitPair	; }
  Int_t 	nGhostPair() 			const { return mNGhostPair	; }
  Int_t 	nContamPair() 			const { return mNContamPair	; }

  float impact()				const { return mImpact		; }
  float impactPhi()				const { return mImpactPhi	; }
  float timeOffset()				const { return mTimeOffset	; }

  void setEventId(Int_t val)				{ mEventId=val; }
  void setRunId(Int_t val)				{ mRunId=val; }
  void setOriginMult(Int_t val)				{ mOriginMult=val; }
  void setCentralMult(Int_t val)			{ mCentralMult=val; }
  void setCentrality(Int_t val)         		{ mCentrality=val; }
  void setNUncorrectedNegativePrimaries(Int_t val)	{ mNUncorrectedNegativePrimaries=val; }
  void setNUncorrectedPrimaries(Int_t val)		{ mNUncorrectedPrimaries=val; }
  void setNUncorrectedGlobals(Int_t val)   { mNUncorrectedGlobals=val; }
  void setNFtpcWUncorrectedPrimaries(Int_t val)		{ mNFtpcWUncorrectedPrimaries=val; }
  void setNFtpcEUncorrectedPrimaries(Int_t val)		{ mNFtpcEUncorrectedPrimaries=val; }
  void setMcMult(Int_t val)				{ mMcMult=val; }
  void setNMcNch(Int_t val)				{ mNMcNch=val; }
  void setNMcFtpcWNch(Int_t val)			{ mNMcFtpcWNch=val; }
  void setNMcFtpcENch(Int_t val)			{ mNMcFtpcENch=val; }
  void setNMcHminus(Int_t val)				{ mNMcHminus=val; }
  void setNMcGlobal(Int_t val)				{ mNMcGlobal=val; }
  void setNMcGoodGlobal20(Int_t val)			{ mNMcGoodGlobal20=val; }
  void setNRcGlobal(Int_t val)				{ mNRcGlobal=val; }
  void setNRcGoodGlobal20(Int_t val)			{ mNRcGoodGlobal20=val; }
  void setVertexX(Float_t val)				{ mVertexX=val; }
  void setVertexY(Float_t val)				{ mVertexY=val; }
  void setVertexZ(Float_t val)				{ mVertexZ=val; }
  void setVertexCovMatrix(Float_t *cov) {for (Int_t i = 0; i < 6; i++) mVertexCovMatrix[i] = cov[i];}
  void setMcVertexX(Float_t val)			{ mMcVertexX=val; }
  void setMcVertexY(Float_t val)			{ mMcVertexY=val; }
  void setMcVertexZ(Float_t val)			{ mMcVertexZ=val; } 
  void setCenterOfMassEnergy(Float_t val)		{ mCenterOfMassEnergy=val; }
  void setMagField(Float_t val) 			{ mMagField=val; }
  void setBackgroundRate(Float_t val)			{ mBackgroundRate=val; }
  void setBeamMassNumberEast(Short_t val)		{ mBeamMassNumberEast=val; }
  void setBeamMassNumberWest(Short_t val)		{ mBeamMassNumberWest=val; }
  void setCtb(Float_t val)				{ mCtb=val; }
  void setZdcE(Float_t val)				{ mZdcE=val; }
  void setZdcW(Float_t val)				{ mZdcW=val; }
  void setNMcTrack(Int_t val)				{ mNMcTrack	=val; }
  void setNMatchedPair(Int_t val)			{ mNMatchedPair	=val; }
  void setNMergedPair(Int_t val)			{ mNMergedPair	=val; }
  void setNSplitPair(Int_t val)				{ mNSplitPair	=val; }
  void setNGhostPair(Int_t val)				{ mNGhostPair	=val; }
  void setNContamPair(Int_t val)			{ mNContamPair	=val; }

  void setImpact(float imp)				{ mImpact	=imp  ;}
  void setImpactPhi(float imphi)			{ mImpactPhi	=imphi;}
  void setTimeOffset(float time)			{ mTimeOffset	=time ;}


  virtual void Print(Option_t *option="") const;
private:
  //
  // data members
  //
  Int_t         mEventId;
  Int_t         mRunId;       // set to 0 for simulations
  Int_t         mOriginMult;  // StEvent::primaryVertex(0)->numberOfDaughters()
  Int_t         mCentralMult; // reco, primary trk, flag>0, |eta|<0.75 
  Int_t         mCentrality;  // centrality bin, Nch cuts, P02gd, 2k2
  Int_t         mNUncorrectedNegativePrimaries; // from StuRefMult
  Int_t         mNUncorrectedPrimaries; // from StuRefMult
  Int_t         mNUncorrectedGlobals; // reco, globals, glTrk->helix->dist(vtx)<3, gl.fitPts>=10, -0.5 < eta < 0.5  
  Int_t         mNFtpcWUncorrectedPrimaries; // reco, primaries, flag>0, glTrk->helix->dist(vtx)<3, prim.pt<3, gl.fitPts>=5, 2.8 < eta < 3.8
  Int_t         mNFtpcEUncorrectedPrimaries; // reco, primaries, flag>0, glTrk->helix->dist(vtx)<3, prim.pt<3, gl.fitPts>=5,-2.8 > eta >-3.8
  Int_t         mMcMult;      // embedding: n mc tracks; (StMcEvent::numberOfPrimaryTracks()
                              // simulation: same as mOriginMult 
  Int_t         mNMcNch;      // mc, primary, charge!=0, |eta|<0.5
  Int_t         mNMcFtpcWNch; // mc, primary, charge!=0, 2.8 < eta < 3.8
  Int_t         mNMcFtpcENch; // mc, primary, charge!=0,-2.8 > eta >-3.8
  Int_t         mNMcHminus;   // mc, primary, charg  <0, |eta|<0.5

  Int_t		mNMcGlobal;             // mc, primary, |eta|<4
  Int_t		mNMcGoodGlobal20;	// mc, primary, |eta|<4, MC TPC hits >=20
  Int_t         mNRcGlobal;     	// reco, primaries flag > 0
  Int_t         mNRcGoodGlobal20; 	// reco, primaries flag > 0, 20 fit hits

  Float_t       mImpact;  	// Impact parameter
  Float_t       mImpactPhi;  	// Phi Impact parameter, azimuth of reaction plane 
  Float_t       mTimeOffset;	// time offset in seconds wrt trigger event

  Float_t       mVertexX;  
  Float_t       mVertexY;     // 
  Float_t       mVertexZ;
  Float_t       mVertexCovMatrix[6];
  Float_t       mMcVertexX;
  Float_t       mMcVertexY;
  Float_t       mMcVertexZ;
    
  Float_t       mMagField;    // in kGauss

  Float_t   mCenterOfMassEnergy;
  Float_t   mBackgroundRate;
  Short_t   mBeamMassNumberEast;
  Short_t   mBeamMassNumberWest;

  Float_t       mCtb;
  Float_t       mZdcE;
  Float_t       mZdcW;

  //
  // for root reasons, even though ghosts are just the rc tracks,
  // save it as a StMiniMcPair ... 

  Int_t mNMcTrack;     
  Int_t mNMatchedPair;
  Int_t mNMergedPair;
  Int_t mNSplitPair;
  Int_t mNGhostPair;
  Int_t mNContamPair;
  Int_t mNMatGlobPair;
    
  TClonesArray* mMcTracks;
  TClonesArray* mMatchedPairs;
  TClonesArray* mMergedPairs;
  TClonesArray* mSplitPairs;
  TClonesArray* mGhostPairs; 
  TClonesArray* mContamPairs;
  TClonesArray* mMatGlobPairs;
  static Int_t mSFirst; //!

  ClassDef(StMiniMcEvent,5)
};

#endif

  
//
// $Log: StMiniMcEvent.h,v $
// Revision 1.9  2012/03/15 23:37:20  perev
// Uncorrected globals added(Chris)
//
// Revision 1.8  2011/03/22 00:31:48  perev
// Added impact,phi impact & trigger time
//
// Revision 1.7  2007/05/21 16:17:16  fisyak
// Increament ClassDef, thanks to Adam Kocoloski
//
// Revision 1.6  2007/02/23 17:07:00  fisyak
// Add Ssd and DCA
//
// Revision 1.5  2004/01/26 13:58:18  calderon
// Introduction of global matched branch.
//
// Revision 1.4  2003/07/09 01:05:51  calderon
// Added the FTPC reference multiplicity, East and West
// Added various other multiplicity definitions, good globals reco and MC,
// and MC Nch and Nh-.
// Keep data members private and provide setters and getters
//
// Revision 1.3  2003/05/08 02:09:20  calderon
// Added data members for svt and ftpc fit points for StTinyRcTrack.
// Added data members for svt and ftpc hits for StTinyMcTrack.
// Added methods to calculate px, py, and p from the available pt,  phi and pz, for
// global and primary momenta and also for monte carlo momentum.
// Cleaned up includes in StMiniMcEvent.
//
// Revision 1.2  2002/06/06 15:14:13  calderon
// the comment about the magnetic field said it is in tesla, but it is actually
// in kGauss, so I modified the comment to reflect this
//
// Revision 1.1  2002/05/30 01:20:57  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
