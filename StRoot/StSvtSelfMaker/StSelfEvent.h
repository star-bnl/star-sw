/*!
 * \class StSelfEvent 
 * \author Victor Perev, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StSelfEvent.h,v 1.1 2006/02/14 19:02:09 perev Exp $
 *
 * Author: Victor Perev, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSelfEvent.h,v $
 * Revision 1.1  2006/02/14 19:02:09  perev
 * Svt self alignment maker
 *
 *
 **************************************************************************/
#ifndef StSelfEvent_hh
#define StSelfEvent_hh
#include "TObject.h"
#include "TClonesArray.h"
#include "TDatime.h"


class StSelfHit : public TObject {
public:
    StSelfHit();
   ~StSelfHit(){}
    void Print(const char* option = "") const;
int TestIt();
int Prepare();
public:
int mTrackNumber; 		//track number of hit
UInt_t mHardwarePosition;	//see StHit.h
float mNormalRefAngle;	//rotation angle in Sti style
float mNormalRadius;    // >= 0
float mNormalYOffset;
float mZCenter;
float mXg[3];			//ideal  x,y,z    in global Sti frame
float mXl[3];			//ideal  x,y,z    in local  Sti frame, 
                                //BUT in TTree
                                //for y: mNormalYoffset subtructed 
                                //for z: mZCenter       subtructed 
float mFg[3];			//fitted x,y,z - ideal  in global Sti frame
                                //BUT in TTree mXg subtracted
float mFl[3];			//fitted x,y,z - ideal  in local  Sti frame
                                //BUT in TTree mXl subtracted
float mDg[3];			//track direction in global Sti frame
float mDl[3];			//track direction in local  Sti frame
int mId;                        //1000*layer+ 100*wafer + ladder
  ClassDef(StSelfHit,1);
};

class StSelfEvent : public TObject {
public:
   StSelfEvent();
  ~StSelfEvent()			{;}
void Clear(const char *opt = "")	{mHits.Clear();}
public:
   int   mRun;
   int   mEvt;
   TDatime mDate;	//DAQ time (GMT)

   float mVtx[3];	//Primary vertex position in global frame
   float mEtx[6];	//errors xx,yx,yy,zx,zy,zz
   float mChi2;         //Chi square of vertex fit
   float mVtxOld[3];	//Sti Primary vertex position in global frame
   float mEtxOld[6];	//Sti errors xx,yx,yy,zx,zy,zz
TClonesArray mHits;
  ClassDef(StSelfEvent,1);
};

  
#endif
