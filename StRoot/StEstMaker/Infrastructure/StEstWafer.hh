/***************************************************************************
 *
 * $Id: StEstWafer.hh,v 1.2 2001/01/25 18:22:35 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstBranch Class
 *
 ***************************************************************************
 *
 * $Log: StEstWafer.hh,v $
 * Revision 1.2  2001/01/25 18:22:35  lmartin
 * StEstIndexGeom class moved from the StEstTracker files.
 *
 * Revision 1.1  2000/12/07 11:14:28  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstWafer_hh
#define StEstWafer_hh
#include "StMaker.h"
//#include "StEstConst.h"
#include "StThreeVectorD.hh"

class StEstHit;

class StEstWafer {


protected:
  int mShape;
  int mLayer;
  long int mNHits;
  long int mMaxHits;
  StThreeVectorD* x;
  StThreeVectorD* n;
public:

  StEstHit **mHits;
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
    int i;
    for (i=0;i<8;i++) 
      this->neighbour[i]=NULL;
    //    cout<<"Wafer_id="<<this->GetId()<<" mNHits="<<mNHits<<endl;
    delete [] mHits;
    delete x;
    delete n;
  };

  StThreeVectorD* StEstWafer::GetX();
  StThreeVectorD* StEstWafer::GetN();
  StEstHit* GetHit(long nr);
  int AddHit(StEstHit *hit);
  int RemoveHit(StEstHit *hit);
  int GetLayer();
  int GetShape();
  long GetNHits();
  long GetMaxHits();
  long int GetId();

};

inline StThreeVectorD* StEstWafer::GetX() {return x;};
inline StThreeVectorD* StEstWafer::GetN() {return n;};
inline int StEstWafer::AddHit(StEstHit *hit) {
  if (mNHits>=mMaxHits) return 1; 
  else mHits[mNHits]=hit;mNHits++; return 0;};
inline int StEstWafer::RemoveHit(StEstHit *hit) {
  int ihit;
  for (int i=0;i<mNHits;i++) 
    if (mHits[i]==hit) ihit=i;
  for (int i=ihit;i<mNHits-1;i++)
    mHits[i]=mHits[i+1];
  mHits[mNHits-1]=0;
  mNHits--;
  return 0;
} 
inline long StEstWafer::GetNHits() {return mNHits;};
inline long StEstWafer::GetMaxHits() {return mMaxHits;};
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

// class for preprojection
class StEstIndexGeom{
private:

  int nphibins;
  int nzbins;

public:
  int*** nWaf; //number of wafers
  StEstWafer *****pWaf; 
  StEstIndexGeom::StEstIndexGeom(int np, int nz) {

    nphibins=np;
    nzbins=nz;

    pWaf = new StEstWafer****[nphibins];
    if (!pWaf) cout<<"Warning problem creating pWaf !"<<endl;
    nWaf = new int**[nphibins];
    if (!nWaf) cout<<"Warning problem creating nWaf !"<<endl;
    for (int i=0;i<nphibins;i++) {
      pWaf[i] = new StEstWafer***[nzbins];
      if (!pWaf[i]) cout<<"Warning problem creating pWaf[i] !"<<endl;
      nWaf[i] = new int*[nzbins];
      if (!nWaf[i]) cout<<"Warning problem creating nWaf[i] !"<<endl;
      for (int j=0;j<nzbins;j++) {
	pWaf[i][j] = new StEstWafer**[4];
	if (!pWaf[i][j]) cout<<"Warning problem creating pWaf[i][j] !"<<endl;
	nWaf[i][j] = new int[4];
	if (!nWaf[i][j]) cout<<"Warning problem creating nWaf[i][j] !"<<endl;
	for (int k=0;k<4;k++) {
	  pWaf[i][j][k] = new StEstWafer*[4];
	  if (!pWaf[i][j][k]) cout<<"Warning problem creating pWaf[i][j][k] !"<<endl;
	  nWaf[i][j][k] = 0;
	}
      }
    }
  }

  StEstIndexGeom::~StEstIndexGeom() {
    for (int i=0;i<nphibins;i++) {
      for (int j=0;j<nzbins;j++) {
	for (int k=0;k<4;k++) {
	  delete [] pWaf[i][j][k];
	}
	delete [] pWaf[i][j];
	delete [] nWaf[i][j];
      }
      delete [] pWaf[i];
      delete [] nWaf[i];
    }
    delete [] pWaf;
    delete [] nWaf;
  }
  
  int setWafTab(int phi, int z, int slay, StEstWafer* waf) { 
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return 1;
    if(nWaf[phi][z][slay]>=4) return 2;
    pWaf[phi][z][slay][nWaf[phi][z][slay]++]=waf;
    return 0;
  }

  int ResetWafTab() { 
    int slay,phi,z,i;
    for (phi=0;phi<nphibins;phi++)
      for (z=0;z<nzbins;z++) 
	for (slay=0;slay<4;slay++){
	  nWaf[phi][z][slay]=0;
	  for(i=0;i<4;i++){
	    pWaf[phi][z][slay][i]=NULL;
	  }
	}
    return 0;
  }

  int getNWaf(int phi, int z, int slay) {
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return -1;
    return nWaf[phi][z][slay];
  }

  StEstWafer** getWafTab(int phi, int z, int slay) { 
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return NULL;
    return pWaf[phi][z][slay];
  }
  
}; 

#endif
