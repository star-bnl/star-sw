/*!
 * \class StvPullEvent 
 * \author Victor Perev, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StvPullEvent.h,v 1.9 2016/12/28 16:49:11 perev Exp $
 *
 * Author: Victor Perev, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StvPullEvent.h,v $
 * Revision 1.9  2016/12/28 16:49:11  perev
 * Global X,Y,Z for hit added
 *
 * Revision 1.8  2013/07/02 04:08:09  perev
 * add dca00 to pull tree
 *
 * Revision 1.7  2013/03/08 03:42:55  perev
 * Define empty Finish()
 *
 * Revision 1.6  2012/10/21 22:59:41  perev
 * Add IdTruth into pulls
 *
 * Revision 1.5  2012/04/10 22:44:15  perev
 * Chi2 for primary track + vertex added
 *
 * Revision 1.4  2011/08/19 16:27:06  perev
 * track errors added
 *
 * Revision 1.3  2011/07/19 20:10:31  perev
 * Dca00 added
 *
 * Revision 1.2  2011/04/03 20:53:54  perev
 *  Type of end tracking added
 *
 * Revision 1.1  2010/09/25 17:52:07  perev
 * NewDir
 *
 * Revision 1.3  2010/08/01 00:13:19  perev
 * Pull errors added
 *
 * Revision 1.2  2010/07/30 02:54:30  perev
 * Length fr hit added
 *
 * Revision 1.1  2010/07/06 20:27:43  perev
 * Alpha version of Stv (Star Tracker Virtual)
 *
 * Revision 1.7  2009/10/24 20:35:33  perev
 * Remove redundante definition of StvPullEvent::~StvPullEvent()
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
 * StvPullEvent class added
 *
 * Revision 1.1  2006/02/14 19:02:09  perev
 * Svt self alignment maker
 *
 *
 **************************************************************************/
#ifndef StvPullEvent_hh
#define StvPullEvent_hh
#include "TObject.h"
#include "TClonesArray.h"
#include "TDatime.h"


class StvPullTrk : public TObject {
public:
    StvPullTrk();
   ~StvPullTrk(){}
    void Finish(){;}
    void Clear(const char *opt = "");
    void Print(const char* option = "") const;
public:
char mBeg[1];
short mTrackNumber; 		//track number 
unsigned char mVertex; 		//vertex number for primary track
unsigned char nAllHits; 	//number of all hits in track
unsigned char nTpcHits; 	//number of tpc hits in track
unsigned char nFtpcHits; 	//number of tpc hits in track
unsigned char nSsdHits; 	//number of ssd hits in track
unsigned char nRndHits; 	//number of RND hits in track
unsigned char mL; 		//Length of track
unsigned char mTypeEnd; 	//Type of end tracking

float mChi2;
float mChi2P;
float mCurv;        		//curvature
float mPt;        		//pt
float mPsi;			//track Psi(around beam)  in global  Stv frame
float mDip;			//track Dip in global  Stv frame
float mRxy;			//Rxy of track begining 
float mPhi;			//Phi angle of track begining
float mZ;
float mDca00;
//				Errors
float mPtErr;        		//pt error
float mPsiErr;			//track Psi error
float mDipErr;			//track Dip error
float mRxyErr;			//Rxy error 
float mZErr;			//z error
short int mIdTruTk;
short int mQaTruTk;
char mEnd[1];
  ClassDef(StvPullTrk,4);
};

class StvPullHit : public TObject {
public:
    StvPullHit();
   ~StvPullHit(){}
    void Clear(const char *opt = "");
    void Print(const char* option = "") const;
int TestIt();
public:
char mBeg[1];			//! No IO
short mTrackNumber; 		//track number of hit
unsigned char mVertex; 		//vertex number for primary track
unsigned char nAllHits; 	//number of all hits in track
unsigned char nTpcHits; 	//number of tpc hits in track
unsigned char nFtpcHits; 	//number of ftpc hits in track
unsigned char nSsdHits; 	//number of ssd hits in track
unsigned char nRndHits; 	//number of RND hits in track
unsigned char mDetector;	//see StHit.h
				// 0=smallest Xi2


float mChi2;
float mCurv;        		//curvature
float mPt;        		//pt
float mCharge;                  //charge (Q)
float mX;                       //global X
float mY;                       //global Y
float mZ;                       //global Z
//		locals
//		local Stv frame is a StvTrtack frame
float lYHit;			// y of  Hit  in local  Stv frame
float lZHit;			// z of  Hit  in local  Stv frame
float lYHitErr;			// y Hit Err in local  Stv frame
float lZHitErr;			// z Hit Err in local  Stv frame
float lLen;			// length of Hit

float lYPul;			// lYHit/lYHitErr
float lZPul;			// lZHit/lZHitErr
float lYPulErr;			// y pull Err in local  Stv frame
float lZPulErr;			// z pull Err in local  Stv frame

//		Globals
float gPhiHP;			//  phi    of normal vector of hit plane in global Stv frame
float gLamHP;			//  lambda of normal vector of hit plane in global Stv frame
float gRHit;			//  Rxy of Hit  in global Stv frame
float gPHit;			//  Phi of Hit  in global Stv frame
float gZHit;			//  Z   of Hit  in global Stv frame
float gPHitErr;			//  Phi Hit err in global Stv frame
float gZHitErr;			//  Z   Hit err in global Stv frame

float gRFit;			//  Rxy of Fit  in global Stv frame
float gPFit;			//  Phi of Fit  in global Stv frame
float gZFit;			//  Z   of Fit  in global Stv frame
float gPFitErr;			//  Phi Fit err in global Stv frame
float gZFitErr;			//  Z   Fit err in global Stv frame

float gPPul;			//  dPhi*Rxy of Pul  in global Stv frame
float gZPul;			//  dZ       of Pul  in global Stv frame
float gPPulErr;			//  dPhi Pul err in global Stv frame
float gZPulErr;			//  dZ   Pul err in global Stv frame

float gPsi;			//  track Psi in global  Stv frame
float gDip;			//  track Dip in global  Stv frame
short int mIdTruth;
short int mQaTruth;
char mEnd[1];			//!No IO
  ClassDef(StvPullHit,2);
};

class StvPullEvent : public TObject {
public:
   StvPullEvent();
void Clear(const char *opt = "");	
void Finish();	
void Add(StvPullHit &ph,int gloPrim);
void Add(StvPullTrk &pt,int gloPrim=0);
const int *GetNHits() const;
public:
   int   mRun;
   int   mEvt;
   TDatime mDate;	//DAQ time (GMT)

     int mIVtx;	        //Primary vertex id 
   float mVtx[3];	//Primary vertex position in global frame
   float mEtx[6];	//errors xx,yx,yy,zx,zy,zz
   float mChi2;         //Chi square of vertex fit
   int   mNTrks[2];     //N glob,N Prim tracks 
   int   mNHits[6];     //nTpc,nSvt,nSsd,nPxl,nIst,nRnd hits

TClonesArray mTrksG;	//global  tracks
TClonesArray mTrksP;	//primary  tracks
TClonesArray mHitsG;	//StvPullHits for global  tracks
TClonesArray mHitsP;	//StvPullHits for primary tracks
TClonesArray mHitsR;	//StvPullHits for Rnd detectors
  ClassDef(StvPullEvent,5);
};

  
#endif
