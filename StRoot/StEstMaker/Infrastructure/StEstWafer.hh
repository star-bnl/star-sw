/***************************************************************************
 *
 * $Id: StEstWafer.hh,v 1.1 2000/12/07 11:14:28 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstBranch Class
 *
 ***************************************************************************
 *
 * $Log: StEstWafer.hh,v $
 * Revision 1.1  2000/12/07 11:14:28  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstWafer_hh
#define StEstWafer_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StThreeVectorD.hh"

class StEstHit;

class StEstWafer {

protected:

  int mShape;
  int mLayer;
  long int mNHits;
  long int mMaxHits;
  StEstHit **mHits;
  StThreeVectorD* x;
  StThreeVectorD* n;
public:

  char mPreprojection;  //flag for Preprojection method
  long int mId; // should be private !!!
  StEstWafer* neighbour[8]; // table of neighbours
  
  StEstWafer(long int nr, long int mh, StThreeVectorD* xx,
	       StThreeVectorD* nn, int shape) {
    mNHits=0; 
    mId = nr;
    mShape   = shape;
    mMaxHits = mh;
    mHits = new StEstHit*[mh];
    if (!mHits)
      cerr << "ERROR StEstWafer::StEstWafer mHits = new StEstHit*["<<mh<<"] = NULL" <<endl;
    mLayer=mId/1000;
    mPreprojection = 0;
    
    //find layer number
    switch(mLayer)
      {
      case 8:	
      case 7: mLayer=3;
	break;
      case 6:
      case 5: mLayer=2;
	break;
      case 4:
      case 3: mLayer=1;
	break;
      case 2:
      case 1: mLayer=0;
	break;
      }	    
    x=xx;
    n=nn;
  };
  ~StEstWafer() {
    delete [] mHits;
    delete x;
    delete n;
  };

  StThreeVectorD* StEstWafer::GetX();
  StThreeVectorD* StEstWafer::GetN();
  StEstHit* GetHit(long nr);
  int AddHit(StEstHit *hit);
  int GetLayer();
  int GetShape();
  long GetNHits();
  long int GetId();

};

inline StThreeVectorD* StEstWafer::GetX() {return x;};
inline StThreeVectorD* StEstWafer::GetN() {return n;};
inline int StEstWafer::AddHit(StEstHit *hit) {
  if (mNHits>=mMaxHits) return 1; 
  else mHits[mNHits]=hit;mNHits++; return 0;};
inline long StEstWafer::GetNHits() {return mNHits;};
inline StEstHit* StEstWafer::GetHit(long nr) {
  if (nr<0) {
    cerr << "ERROR StEstWafer::GetHit nr<0" << endl;
    return NULL;
  }
  if (nr>=mNHits) {
    cerr << "ERROR StEstWafer::GetHit nr>=mNHits" << endl;
    return NULL;
  }
  return mHits[nr];
};
inline int StEstWafer::GetLayer() {return mLayer;};
inline int StEstWafer::GetShape() {return mShape;};
inline long StEstWafer::GetId() {return mId;};

#endif


