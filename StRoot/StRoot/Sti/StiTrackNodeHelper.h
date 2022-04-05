#ifndef StiTrackNodeHelper_H
#define StiTrackNodeHelper_H 1


#include "StiKalmanTrackNode.h"
class StiDetector;
class StiMcsErrs {
public:
void reset()		{memset(this,0,sizeof(*this));}
public:
double _cEE ;		//add err to <eta*eta> eta crossing angle
double _cPP;    	//add err to <ptin*ptin>
double _cTP;    	//add err to <tanL*ptin>
double _cTT;    	//add err to <tanL*tanL>
double _ptinCorr;	//ptin correction factor -1
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
 void set(double chi2max,double chi2vtx,double errConfidence,int iter);    
 void set(StiKalmanTrackNode *pNode,StiKalmanTrackNode *sNode);		
  int makeFit(int smooth);
  StiKalmanTrackNode *getWorst()    	const 	{return mWorstNode;}
  StiKalmanTrackNode *getFlipFlop() 	const 	{return mFlipFlopNode;}
  StiKalmanTrackNode *getVertexNode()	const 	{return mVertexNode;}
  int                 getUsed()     	const	{return mUsed;}
  void                setDir(int dir) { mDir = dir;};
static int isCutStep()				{return mgCutStep;}
private:
  void reset();
  int propagatePars(const StiNodePars &parPars
                   ,      StiNodePars &rotPars
		   ,      StiNodePars &proPars);       
  int propagateFitd();
  int propagateMtx();       
  int propagateError();
  int setHitErrs();
  int propagateMCS();
  double evalChi2();
  double joinChi2();
  double recvChi2(); 
  int updateNode();
  void resetError(double fk=0.);
  int nudge();
  int join();
  int save();
  int cutStep(StiNodePars *pars,StiNodePars *base);
  double pathIn(const StiDetector *det,StiNodePars *pars);
//	Static methods
public:
static double joinTwo(int nP1,const double *P1  ,const double *E1
                     ,int nP2,const double *P2  ,const double *E2
		             ,      double *PJ=0,      double *EJ=0);

#if 0
static double joinVtx(        const double *P1  ,const double *E1
                             ,const double *P2  ,const double *E2
	                     ,      double *PJ=0,      double *EJ=0);
#endif			    
double        joinVtx(const double      *Y,const StiHitErrs  &B
                     ,const StiNodePars &X,const StiNodeErrs &A
	             ,      StiNodePars *M=0,    StiNodeErrs *C=0);
static int getHitErrors(const StiHit *hit,const StiNodePars *pars, StiHitErrs *hrr);
static Int_t  debug() 		{return _debug;}
static void   setDebug(Int_t m) {_debug = m;}

private:
double mChi2Max;
double mChi2Vtx;
double mNodeErrFactor;
double mHitsErrFactor;
double mParentHz;
double mTargetHz;
int    mIter;				//current iter number
int    mDir;				//current iter number
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
double     mHitPars[3];
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
  static int  _debug;
public:
  QaFit  mCurvQa;
  QaFit  mTanlQa;
  static int mgCutStep;
};


	
#endif



