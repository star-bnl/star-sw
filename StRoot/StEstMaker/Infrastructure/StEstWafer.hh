/***************************************************************************
 *
 * $Id: StEstWafer.hh,v 1.5 2001/04/25 17:28:35 perev Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstBranch Class
 *
 ***************************************************************************
 *
 * $Log: StEstWafer.hh,v $
 * Revision 1.5  2001/04/25 17:28:35  perev
 * HPcorrs
 *
 * Revision 1.4  2001/02/23 14:19:11  lmartin
 * cout replaced by gMessMgr.
 *
 * Revision 1.3  2001/01/26 10:24:17  lmartin
 * Minor changes. Commented statement removed. Short description of the data members added.
 *
 * Revision 1.2  2001/01/25 18:22:35  lmartin
 * StEstIndexGeom class moved from the StEstTracker files.
 *
 * Revision 1.1  2000/12/07 11:14:28  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef StEstWafer_hh
#define StEstWafer_hh
#include "StMessMgr.h"
#include "StMaker.h"
#include "StThreeVectorD.hh"

class StEstHit;

class StEstWafer {


protected:
  int mShape; // shape type of the wafer from the geom table
  int mLayer; // layer of the wafer
  long int mNHits; // number of hits in the wafer
  long int mMaxHits; // maximum number of hits in the wafer (should match mNHits)
  StThreeVectorD* x; // global coordinates of the wafer center
  StThreeVectorD* n; // global orientation of the wafer

public:
  StEstHit **mHits; // list of pointer to the hits of the wafer
  char mPreprojection;  //flag for Preprojection method
  long int mId; // should be private !!! Id from the geom table
  StEstWafer* neighbour[8]; // table of wafer neighbours
  
  StEstWafer(long int nr, long int mh, StThreeVectorD* xx,
	       StThreeVectorD* nn, int shape) {
    mNHits=0; 
    mId = nr;
    mShape   = shape;
    mMaxHits = mh;
    mHits = new StEstHit*[mh];
    if (!mHits)
      gMessMgr->Error()<<"StEstWafer::StEstWafer mHits = new StEstHit*["<<mh<<"] = NULL"<<endm;
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
    delete [] mHits;
    delete x;
    delete n;
  };

  StThreeVectorD* GetX();
  StThreeVectorD* GetN();
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
  {for (int i=0;i<mNHits;i++) 
    if (mHits[i]==hit) ihit=i;}
  {for (int i=ihit;i<mNHits-1;i++)
    mHits[i]=mHits[i+1];}
  mHits[mNHits-1]=0;
  mNHits--;
  return 0;
} 
inline long StEstWafer::GetNHits() {return mNHits;};
inline long StEstWafer::GetMaxHits() {return mMaxHits;};
inline StEstHit* StEstWafer::GetHit(long nr) {
  if (nr<0) {
    gMessMgr->Error()<<"StEstWafer::GetHit nr<0"<<endm;
    return NULL;
  }
  if (nr>=mNHits) {
    gMessMgr->Error()<<"StEstWafer::GetHit nr>=mNHits"<<endm;
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
  StEstIndexGeom(int np, int nz) {

    nphibins=np;
    nzbins=nz;

    pWaf = new StEstWafer****[nphibins];
    if (!pWaf) gMessMgr->Error()<<"Problem creating pWaf !"<<endm;
    nWaf = new int**[nphibins];
    if (!nWaf) gMessMgr->Error()<<"Problem creating nWaf !"<<endm;
    for (int i=0;i<nphibins;i++) {
      pWaf[i] = new StEstWafer***[nzbins];
      if (!pWaf[i]) gMessMgr->Error()<<"Problem creating pWaf[i] !"<<endm;
      nWaf[i] = new int*[nzbins];
      if (!nWaf[i]) gMessMgr->Error()<<"Problem creating nWaf[i] !"<<endm;
      for (int j=0;j<nzbins;j++) {
	pWaf[i][j] = new StEstWafer**[4];
	if (!pWaf[i][j]) gMessMgr->Error()<<"Problem creating pWaf[i][j] !"<<endm;
	nWaf[i][j] = new int[4];
	if (!nWaf[i][j]) gMessMgr->Error()<<"Problem creating nWaf[i][j] !"<<endm;
	for (int k=0;k<4;k++) {
	  pWaf[i][j][k] = new StEstWafer*[4];
	  if (!pWaf[i][j][k]) gMessMgr->Error()<<"Problem creating pWaf[i][j][k] !"<<endm;
	  nWaf[i][j][k] = 0;
	}
      }
    }
  }

  ~StEstIndexGeom() {
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
