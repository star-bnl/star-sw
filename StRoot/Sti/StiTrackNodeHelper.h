#ifndef StiTrackNodeHelper_H
#define StiTrackNodeHelper_H 1


#include "StiKalmanTrackNode.h"
class StiDetector;



class StiTrackNodeHelper
{ 
public:

  StiTrackNodeHelper(double chi2max);    
  void setNodes(StiKalmanTrackNode *pNode,StiKalmanTrackNode *sNode)		
       					{mPNode=pNode;mSNode=sNode;}
  int propagate(StiNodePars *ipars);       
  int propagateMtx();       
  int propagateError(StiNodeErrs &lastFE);
  int fake1Fit();
  int fake2Fit();
  int fit(){return 0;}       
private:
  int join(StiNodeErrs &a,StiNodeErrs &b);
private:
double mChi2Max;

StiKalmanTrackNode *mPNode;		
StiKalmanTrackNode *mSNode;		
StiNodePars mAP; 	//Best parent params 
StiNodePars mBP;	//Best rotated parent params in son node frame
StiNodePars mCP;        //Propagated  best parent params into son node frame
StiNodePars mDP;	//Fitted parent params propagated into son node frame
StiNodePars mXP; 	//Best son params

StiNodeErrs mPE; 	
StiNodeErrs mFE; 	
StiNodeErrs mBigE; 	
StiNodeErrs mLE; 	
StiNodeErrs mJE; 	

StiNodeMtx mMtx;
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


};


	
#endif



