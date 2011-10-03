#ifndef __StvConst_h_
#define __StvConst_h_
#include "TNamed.h"


class StvConst : public TNamed {
public:	

StvConst(const char *name="DefaultConst");
static StvConst *Inst() {return mgConst;}	

public:
      char 	mBeg[1];
      float 	mXi2Hit;		//Xi2 to accept new hit
      float    	mXi2Vtx;		//Xi2 to accept vertex
      float 	mXi2Joi;		//Xi2 in Refit join left & right subtrack
      float 	mXi2Hlx;		//Xi2 in Helix, .
      float	mRxyMax;		//Max radius for tracking
      float	mZMax;			//Max Z      for tracking
      float     mDca2dZeroXY;		//max 2d dca to X=Y=0  for primary track
      float     mDca3dVertex;  		//max 3d dca to vertex for primary track
      float     mMaxCurv;		//Maximal allowed curvature
      float     mMinP2;		        //Minimal allowed momentum**2
      float     mMaxWindow;		//Maximal window to search hits
        int     mMinHits;		//Min number of hits allowed
        int     mGoodHits;		//Good number of hits in track
      char   	mEnd[1];
static StvConst *mgConst;
ClassDef(StvConst,0)//
};
#endif //__StvConst_h_


