#ifndef StiTrackNodeHelper_H
#define StiTrackNodeHelper_H 1


#include "StiKalmanTrackNode.h"
class StiDetector;
class StiMcsErrs {
public:
void reset()		{memset(this,0,sizeof(*this));}
public:
double _cEE ;		//add err to <eta*eta> eta crossing angle
double _cCC;    	//add err to <curv*curv>
double _cTC;    	//add err to <tanL*curv>
double _cTT;    	//add err to <tanL*tanL>
double _curvCorr;	//curv correction factor -1
};

class StiHitErrs{
public:
void reset()			{memset(this,0,sizeof(*this));}
StiHitErrs &operator*=(double f) {for (int i=0;i<6;i++){A[i]*=f;};return *this;}
union{
  double hXX;		double A[1];};
  double hYX,hYY;                       
  double hZX,hZY, hZZ;                 
};

class QaFit 
{
public:
QaFit()					{reset();}
void reset()				{memset(this,0,sizeof(*this));}
void add(double val);
void finish();
int    getTally() const			{return mTally;}
double getAccu(int kValCorr);
double getMaxi(int kValCorr) 		{return mMaxi[kValCorr];}
double getNStd(int kValCorr);
double getNSgn(int kValCorr);
void   getInfo(double *info);
private:
double mPrev;
double mAver[2];
double mErrr[2];
double mMaxi[2];
int    mNega[2];

int mEnded;
int mTally;
};


class StiTrackNodeHelper
{ 
public:

      StiTrackNodeHelper(){}    
 void set(double chi2max,double errConfidence,int iter);    
 void set(StiKalmanTrackNode *pNode,StiKalmanTrackNode *sNode);		
  int makeFit(int smooth);
  StiKalmanTrackNode *getWorst()    	const 	{return mWorstNode;}
  StiKalmanTrackNode *getFlipFlop() 	const 	{return mFlipFlopNode;}
  StiKalmanTrackNode *getVertexNode()	const 	{return mVertexNode;}
  int                 getUsed()     	const	{return mUsed;}

static int isCutStep()				{return mgCutStep;}
private:
  void reset();
  int propagateBest();       
  int propagateFitd();
  int propagateMtx();       
  int propagateError();
  int setHitErrs();
  int propagateMCS();
  double evalChi2();
  double joinChi2();
  int updateNode();
  void resetError(double fk=0.);
  int nudge();
  int join();
  int save();
  int cutStep(StiNodePars *pars,StiNodePars *base);
  double pathIn(const StiDetector *det,StiNodePars *pars);
private:
double mChi2Max;
double mErrConfidence;
double mErrConfiDefault;
int    mIter;				//current iter number
StiKalmanTrackNode *mWorstNode;		//node with the worst Chi2
StiKalmanTrackNode *mFlipFlopNode;	//node with the worst flip/flop ratio
StiKalmanTrackNode *mVertexNode;	//vertex node
char mBeg[1];
StiKalmanTrackNode *mParentNode;		
StiKalmanTrackNode *mTargetNode;		
const StiDetector  *mDetector;
const StiHit       *mHit;
StiNodePars mBestParentPars; 	//Best parent params 
StiNodePars mBestParentRotPars;	//Best rotated parent params into target node frame
StiNodePars mBestPars; 		//Propagated  best parent params into target node frame
double      mBestDelta;		//maximal step in x,y,z allowed

StiNodePars mFitdParentPars;	//Fitted parent params in own coordianate frame node frame
StiNodePars mPredPars;	//Fitted parent params propagated into target frame
StiNodePars mFitdPars;	//Fitted target params 
StiNodePars mJoinPars;	//Joined target params 

StiNodePars mSavdParentPars;	//Saved params from the previous fit
double      mSavdDelta;         //Saved delta to select the best Best

StiNodeErrs mFitdParentErrs;    //fitted parenterrors
StiNodeErrs mPredErrs;    	//predicted errors
StiNodeErrs mFitdErrs;    	//fitted target errors
StiNodeErrs mJoinErrs;    	//joined target errors
StiNodeErrs mBestParentErrs;    //best parent errors

StiNodeMtx mMtx;
StiMcsErrs mMcs;
StiHitErrs mHrr;
StiNode2Pars mUnTouch;
// locals
  double alpha,ca,sa;
  double x1;
  double x2;
  double dx;
  double rho;
  double dsin;
  double sinCA2; 
  double cosCA2;
  double sumSin;
  double sumCos;
  double dy;
  double y2;
  double dl0;
  double sind;
  double dl;
  
  double mDetm;
  double mChi2;
  int    mState;
  int    mUsed;
  char   mEnd[1];
public:
  QaFit  mCurvQa;
  QaFit  mTanlQa;
  static int mgCutStep;
};


	
#endif



