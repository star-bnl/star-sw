// $Id: StSsdPoint.hh,v 1.2 2007/03/27 23:11:48 bouchet Exp $
//
// $Log: StSsdPoint.hh,v $
// Revision 1.2  2007/03/27 23:11:48  bouchet
// Add a method to use the gain calibration for the Charge Matching between pulse of p and n sides
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.3  2005/03/18 16:29:08  lmartin
//  new members mIdClusterP and mIdClusterN and associated methods
//
// Revision 1.2  2005/03/18 14:19:44  lmartin
// missing CVS header added
//

#ifndef STSSDPOINT_HH
#define STSSDPOINT_HH
#include "Rtypes.h"
class StSsdPoint
{
 public:
  StSsdPoint(Int_t rNPoint, Int_t rNWafer, Int_t rNumPackage, Int_t rKindPackage);
  StSsdPoint(Int_t rNId , Int_t rMcHit , Int_t rMcTrack , Float_t *rXg , Float_t rDe, Float_t *rAngle);
  StSsdPoint(const StSsdPoint & originalPoint);
  ~StSsdPoint() {}

  StSsdPoint& operator=(const StSsdPoint originalPoint);

  void        setAngle(Float_t rAngle, Int_t iR) { mAngle[iR] = rAngle; }
  void        setDe(Float_t rEnergyLoss, Int_t iR) {   mDe[iR] = rEnergyLoss; }
  void        setEnergyLoss(Float_t adcP, Float_t adcN) {setDe((adcP + adcN)/2.,0);  setDe((adcP - adcN)/2.,1); }
  void        setEnergyLossCorrected(Float_t adcP,Float_t adcN, Float_t gain);
  void        setFlag(Int_t rFlag) {  mFlag = rFlag; }
  void        setIdClusterP(Int_t iIdClusterP) {  mIdClusterP = iIdClusterP; }
  void        setIdClusterN(Int_t iIdClusterN) {  mIdClusterN = iIdClusterN; }
  void        setMcHit(Int_t rMcHit, Int_t i = 0) {setNMchit(rMcHit,i);}
  void        setMcTrack(Int_t rMcTrack) { mMcTrack = rMcTrack; }
  void        setNextPoint(StSsdPoint *rNextPoint) {  mNextPoint = rNextPoint; }
  void        setNId(Int_t rNId) { mNId = rNId; }
  void        setNPoint(Int_t rNPoint)  {  mNPoint = rNPoint; }
  void        setNCluster(Int_t rNCluster) {  mNCluster = rNCluster; }
  void        setNMatched(Int_t rNMatched) {  mNMatched = rNMatched; }
  void        setNMchit(Int_t rNMchit, Int_t iR=0) {  mMcHit[iR] = rNMchit; }
  void        setNWafer(Int_t rNWafer)         {  mNWafer = rNWafer; }
  void        setPrevPoint(StSsdPoint *rPrevPoint) {  mPrevPoint = rPrevPoint; }
  void        setPositionU(Float_t rPositionU, Int_t iR) {  mPositionU[iR] = rPositionU; }
  void        setUpos(Float_t rUpos, Int_t iR) {setPositionU(rUpos,iR);}
  void        setXg(Float_t rXg, Int_t iR) {  mXg[iR] = rXg; }
  void        setXl(Float_t rXl, Int_t iR) {  mXl[iR] = rXl; }
  //void        setXg(float rXg,int iR);

  Float_t       getDe(Int_t iR=0)          { return mDe[iR]; }	    
  Int_t         getIdClusterP() 	   { return mIdClusterP; }	    
  Int_t         getIdClusterN() 	   { return mIdClusterN; }	    
  Int_t         getFlag()       	   { return mFlag; }	    
  Int_t         getMcHit(Int_t i=0)        { return getNMchit(i); }	    
  Int_t         getMcTrack()    	   { return mMcTrack; }	    
  Int_t         getNId()        	   { return mNId; }	    
  Int_t         getNPoint()     	   { return mNPoint; }	    
  Int_t         getNCluster()   	   { return mNCluster; }	    
  Int_t         getNMatched()   	   { return mNMatched; }       
  Int_t         getNMchit(Int_t iR)    { return mMcHit[iR]; }	    
  Int_t         getNWafer()          { return mNWafer; }         
  Float_t       getPositionU(Int_t iR) { return mPositionU[iR]; }
  Float_t       getXg(Int_t iR)        { return mXg[iR]; }
  //float         getXg(int iR);
  Float_t       getXl(Int_t iR)        { return mXl[iR]; }

  StSsdPoint* getPrevPoint()       { return mPrevPoint; }
  StSsdPoint* getNextPoint()       { return mNextPoint; }

  StSsdPoint* giveCopy();
  
 private:
  Char_t      first[1];
  Int_t       mNId;
  Int_t       mMcHit[5]; // mcHit
  Int_t       mMcTrack;
  Int_t       mFlag;
  Int_t       mNPoint;
  Int_t       mNCluster;
  Int_t       mNMatched;
  Int_t       mIdClusterP;
  Int_t       mIdClusterN;
  Int_t       mNWafer;
  Float_t     mDe[2];
  Float_t     mPositionU[2]; // *mUpos;
  Float_t     mAngle[2];
  Float_t     mXg[3];
  //float       *mXg;
  Float_t     mXl[3];

  StSsdPoint *mPrevPoint;
  StSsdPoint *mNextPoint;
  Char_t      last[1];
};

#endif
