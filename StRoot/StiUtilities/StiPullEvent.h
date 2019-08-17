/*!
 * \class StiPullEvent 
 * \author Victor Perev, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StiPullEvent.h,v 1.9 2018/01/16 22:46:01 smirnovd Exp $
 *
 * Author: Victor Perev, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StiPullEvent.h,v $
 * Revision 1.9  2018/01/16 22:46:01  smirnovd
 * Removed declared but undefined functions
 *
 * Revision 1.8  2012/11/09 18:52:33  perev
 * Erros added
 *
 * Revision 1.7  2009/10/24 20:35:33  perev
 * Remove redundante definition of StiPullEvent::~StiPullEvent()
 *
 * Revision 1.6  2009/10/18 22:02:12  perev
 * Propagate primary info into globals(Finish())
 *
 * Revision 1.5  2009/10/15 03:28:58  perev
 * Add primary vertex number and charge(GVB)
 *
 * Revision 1.4  2007/10/16 20:56:00  fisyak
 * Add pull entries for Pxl and Ist
 *
 * Revision 1.3  2006/12/19 19:44:41  perev
 * tracks added
 *
 * Revision 1.2  2006/12/18 01:33:35  perev
 * + branch mHitsR(nd) +nHitCand +iHitCand
 *
 * Revision 1.1  2006/04/07 17:34:41  perev
 * StiPullEvent class added
 *
 * Revision 1.1  2006/02/14 19:02:09  perev
 * Svt self alignment maker
 *
 *
 **************************************************************************/
#ifndef StiPullEvent_hh
#define StiPullEvent_hh
#include "TObject.h"
#include "TClonesArray.h"
#include "TDatime.h"


class StiPullTrk : public TObject {
public:
    StiPullTrk();
   ~StiPullTrk(){}
    void Clear(const char *opt = "");
    void Print(const char* option = "") const;
public:
char mBeg[1];			//|NoIO
short mTrackNumber; 		//track number 
unsigned char mVertex; 		//vertex number for primary track
unsigned char nAllHits; 	//number of all hits in track
unsigned char nTpcHits; 	//number of tpc hits in track
unsigned char nSvtHits; 	//number of svt hits in track
unsigned char nSsdHits; 	//number of ssd hits in track
unsigned char nPxlHits; 	//number of pxl hits in track
unsigned char nIstHits; 	//number of ist hits in track
unsigned char mL; 		//Length of track

float mChi2;
float mCurv;        		//curvature
float mPt;        		//pt
float mPsi;			//track Psi(around beam)  in global  Sti frame
float mDip;			//track Dip in global  Sti frame
float mRxy;			//Rxy of track begining 
float mPhi;			//Phi angle of track begining
float mZ;
//				Errors
float mPtErr;        		//pt error
float mPsiErr;			//track Psi error
float mDipErr;			//track Dip error
float mRxyErr;			//Rxy error 
float mZErr;			//z error
short int mIdTruTk;
short int mQaTruTk;
char mEnd[1];			//|NoIO
  ClassDef(StiPullTrk,3);
};

class StiPullHit : public TObject {
public:
    StiPullHit();
   ~StiPullHit(){}
    void Clear(const char *opt = "");
    void Print(const char* option = "") const;
int TestIt();
public:
char mBeg[1];
short mTrackNumber; 		//track number of hit
unsigned char mVertex; 		//vertex number for primary track
unsigned char nAllHits; 	//number of all hits in track
unsigned char nTpcHits; 	//number of tpc hits in track
unsigned char nSvtHits; 	//number of svt hits in track
unsigned char nSsdHits; 	//number of ssd hits in track
unsigned char nPxlHits; 	//number of pxl hits in track
unsigned char nIstHits; 	//number of ist hits in track
unsigned char mDetector;	//see StHit.h
unsigned char nHitCand;	        //number of Hit Candidates
unsigned char iHitCand;	        //number of selected  Hit Candidate.
				// 0=smallest Xi2


UInt_t mHardwarePosition;	//see StHit.h
float mNormalRefAngle;	//rotation angle in Sti style
float mNormalRadius;    // >= 0
float mNormalYOffset;
float mZCenter;
float mChi2;
float mCurv;        		//curvature
float mPt;        		//pt
float mCharge;                  //charge (Q)
//		locals
float lXHit;			// x of  Hit  in local  Sti frame
float lYHit;			// y of  Hit  in local  Sti frame
float lZHit;			// z of  Hit  in local  Sti frame
float lYHitErr;			// y Hit Err in local  Sti frame
float lZHitErr;			// z Hit Err in local  Sti frame
float lHitEmx[3];		// hit error mtx:yy,yz,zz

float lXFit;			// x of  Fit  in local  Sti frame
float lYFit;			// y of  Fit  in local  Sti frame
float lZFit;			// z of  Fit  in local  Sti frame
float lYFitErr;			// y Fit Err in local  Sti frame
float lZFitErr;			// z Fit Err in local  Sti frame
float lFitEmx[3];		// hit error mtx:yy,yz,zz

float lYPul;			// dy of  Pul  in local  Sti frame
float lZPul;			// dz of  Pul  in local  Sti frame
float lYPulErr;			// dy Pul Err in local  Sti frame
float lZPulErr;			// dz Pul Err in local  Sti frame
float lPulEmx[3];			// hit error mtx:yy,yz,zz

float lPsi;			//  track Psi in local  Sti frame
float lDip;			//  track Dip in local  Sti frame

//		Globals
float gRHit;			//  Rxy of Hit  in global Sti frame
float gPHit;			//  Phi of Hit  in global Sti frame
float gZHit;			//  Z   of Hit  in global Sti frame
float gPHitErr;			//  Phi Hit err in global Sti frame
float gZHitErr;			//  Z   Hit err in global Sti frame
float gHitEmx[3];		//  hit error mtx:PhiPhi,PhiZ,ZZ

float gRFit;			//  Rxy of Fit  in global Sti frame
float gPFit;			//  Phi of Fit  in global Sti frame
float gZFit;			//  Z   of Fit  in global Sti frame
float gPFitErr;			//  Phi Fit err in global Sti frame
float gZFitErr;			//  Z   Fit err in global Sti frame
float gFitEmx[3];			//  hit error mtx:PhiPhi,PhiZ,ZZ

float gPPul;			//  dPhi*Rxy of Pul  in global Sti frame
float gZPul;			//  dZ       of Pul  in global Sti frame
float gPPulErr;			//  dPhi Pul err in global Sti frame
float gZPulErr;			//  dZ   Pul err in global Sti frame
float gPulEmx[3];			//  hit error mtx:PhiRPhiR,PhiRZ,ZZ

float gPsi;			//  track Psi in global  Sti frame
float gDip;			//  track Dip in global  Sti frame
short int mIdTruth;
short int mQaTruth;
char mEnd[1];
  ClassDef(StiPullHit,3);
};

class StiPullEvent : public TObject {
public:
   StiPullEvent();
void Clear(const char *opt = "");	
void Finish();	
void Add(StiPullHit &ph,int gloPrim);
void Add(StiPullTrk &pt,int gloPrim=0);
const int *GetNHits() const;
public:
   int   mRun;
   int   mEvt;
   TDatime mDate;	//DAQ time (GMT)

   float mVtx[3];	//Primary vertex position in global frame
   float mEtx[6];	//errors xx,yx,yy,zx,zy,zz
   float mChi2;         //Chi square of vertex fit
   int   mNTrks[2];     //N glob,N Prim tracks 
   int   mNHits[6];     //nTpc,nSvt,nSsd,nPxl,nIst,nRnd hits

TClonesArray mTrksG;	//global  tracks
TClonesArray mTrksP;	//primary  tracks
TClonesArray mHitsG;	//StiPullHits for global  tracks
TClonesArray mHitsP;	//StiPullHits for primary tracks
TClonesArray mHitsR;	//StiPullHits for Rnd detectors
  ClassDef(StiPullEvent,5);
};

  
#endif
