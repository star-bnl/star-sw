#include "StSceWafer.hh"
StSceWafer::StSceWafer(int nid)
{
  mId       = nid;
  mD        = new float[3];
  mT        = new float[3];
  mN        = new float[3];
  mX        = new float[3];
  mPoint    = new StSceListPoint();
  mSimPoint = new StSceListPoint();
  mRecPoint = new StSceListPoint();
  mClusterP = new StSceListCluster();
  mClusterN = new StSceListCluster();
  mComPoint = new StSceListComp();

  for (int i = 0; i < 3; i++)
    {
      mD[i] =  0. ;
      mT[i] =  0. ;
      mN[i] =  0. ;
      mX[i] =  0. ;
    }
}

StSceWafer::~StSceWafer()
{
  delete [] mD;
  delete [] mT;
  delete [] mN;
  delete [] mX;
  delete    mPoint;
  delete    mSimPoint;
  delete    mRecPoint;
  delete    mClusterP;
  delete    mClusterN;
  delete    mComPoint;
}

void StSceWafer::init(int rId, float *rD, float *rT, float *rN, float *rX)
{
  if (rId != mId)
    cout<<" Can not initialize wafer number : "<<mId<<endl;
  else
    {
      for (int i = 0; i<3; i++)
	{
	  mD[i] = rD[i];
	  mT[i] = rT[i];
	  mN[i] = rN[i];
	  mX[i] = rX[i];
	}
    }
}

void StSceWafer::addHit(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *p)
{
  float *alpha = new float[2];
  alpha[0]  = 0.;
  alpha[1]  = 0.;
// next has to be modified:
  StScePoint *tmpPoint = new StScePoint(rNId, rMcHit, rMcTrack, mId, rXg, rDe, this->findAngle(p,alpha));
  (this->mPoint)->addNewPoint(tmpPoint);
  delete [] alpha;
}
  
int StSceWafer::convertGlobalToLocal()
{
  int localSize = (this->mPoint)->getSize();

  if (!localSize) return 0;

  float *xtemp = new float[3];  
  StScePoint *temp = mPoint->first();
  for (int i = 0; i < localSize; i++)
    {
      xtemp[0] = temp->getXg(0) - mX[0];
      xtemp[1] = temp->getXg(1) - mX[1];
      xtemp[2] = temp->getXg(2) - mX[2];
	
      temp->setXl((xtemp[0] * mD[0]) + (xtemp[1] * mD[1]) + (xtemp[2] * mD[2]), 0) ;
      temp->setXl((xtemp[0] * mT[0]) + (xtemp[1] * mT[1]) + (xtemp[2] * mT[2]), 1) ;
      temp->setXl((xtemp[0] * mN[0]) + (xtemp[1] * mN[1]) + (xtemp[2] * mN[2]), 2) ;
      
      temp = mPoint->next(temp);
    }
  delete [] xtemp;
  return 1;
}

int StSceWafer::convertLocalToUFrame(float ActiveLargeEdge, float ActiveSmallEdge, float Theta) 
{
  int localSize = (this->mPoint)->getSize();
  if (!localSize) return 0;

  StScePoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)

    //maybe a pb with the gard ring especially for checking....?
    {
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))-(temp->getXl(1)+(ActiveSmallEdge/2.))*tan(Theta), 0);
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))+(temp->getXl(1)-(ActiveSmallEdge/2.))*tan(Theta), 1); 
      temp = mPoint->next(temp);
    }
  return 1;
} 

StSceListPoint* StSceWafer::getDeadHits(float ActiveLargeEdge, float ActiveSmallEdge,float Test)
{
  StSceListPoint *listDeadBorder   = this->getNonActivePointBorder(ActiveLargeEdge,ActiveSmallEdge);
  StSceListPoint *listDeadTriangle = this->getNonActivePointTriangle(Test);
  StSceListPoint *listDeadTotal    = new StSceListPoint();
  listDeadTotal = listDeadTotal->addListPoint(listDeadBorder);
  listDeadTotal = listDeadTotal->addListPoint(listDeadTriangle);
  listDeadTotal = listDeadTotal->removeMultipleCount();

  (this->mPoint)->substractListPoint(listDeadTotal);
  delete listDeadBorder;
  delete listDeadTriangle;
  return listDeadTotal;
}

void StSceWafer::addCluster(StSceCluster *ptr, int iSide)
{
  if (iSide)
    { mClusterN->addNewCluster(ptr); }
  else
    { mClusterP->addNewCluster(ptr); }
}

void StSceWafer::addSimPoint(StScePoint *ptr)
{  mSimPoint->addNewPoint(ptr); }

void StSceWafer::addRecPoint(StScePoint *ptr)
{  mRecPoint->addNewPoint(ptr); }

void StSceWafer::addComPoint(StSceComp *ptr)
{  mComPoint->addNewComp(ptr); }

StSceListPoint*  StSceWafer::getPoint()
{  return mPoint; }

StSceListPoint*  StSceWafer::getSimPoint()
{  return mSimPoint; }

StSceListPoint*  StSceWafer::getRecPoint()
{  return mRecPoint; }

StSceListCluster* StSceWafer::getClusterP()
{  return mClusterP; }   

StSceListCluster* StSceWafer::getClusterN()
{  return mClusterN; }
   
StSceListComp*  StSceWafer::getComPoint()
{  return mComPoint; }

int StSceWafer::getId()
{  return mId; }
   
StSceListPoint*  StSceWafer::getNonActivePointBorder(float ActiveLargeEdge, float ActiveSmallEdge)
{
  int localSize = (this->mPoint)->getSize();

  StSceListPoint *deadPoints = new StSceListPoint();
  if (!localSize) return deadPoints;
  
  StScePoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)
    {
      if((temp->getXl(0) >(ActiveLargeEdge/2.)) || (temp->getXl(0) < (-ActiveLargeEdge/2.)) || 
	 (temp->getXl(1) >(ActiveSmallEdge/2.)) || (temp->getXl(1) < (-ActiveSmallEdge/2.)))
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StScePoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (this->mPoint)->next(temp);
    }
  return deadPoints;
} 

StSceListPoint* StSceWafer::getNonActivePointTriangle(float Test)
  //typically, test=pitch
{
  int localSize = (this->mPoint)->getSize();
  StSceListPoint *deadPoints = new StSceListPoint();  

  if (!localSize) return deadPoints;

  StScePoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)
    {
      if (temp->getUpos(0) < -1.*Test && temp->getUpos(1) < -1.*Test) 
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StScePoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (this->mPoint)->next(temp);
    }
  return deadPoints;
} 

int StSceWafer::doEvaluateCluster(sce_ctrl_st *ctrl){
   int nEvaluatedCluster  = 0;
   StSceCluster *currentClusterP = 0;
   StSceCluster *currentClusterN = 0;
   StScePoint   *currentSimPoint = 0;
   int p_true  = 0;
   int n_true  = 0;
   int p_ghost = 0;
   int n_ghost = 0;
   int p_lost  = 0;
   int n_lost  = 0;
   int p_checked = 0;
   int n_checked = 0;
   currentClusterP = mClusterP->first();
   currentClusterN = mClusterN->first();
   currentSimPoint = mSimPoint->first();

   while(currentClusterP)
     {
       if(!currentClusterP->getIdMcHit(0)) p_ghost++;
       nEvaluatedCluster++;
       currentClusterP = mClusterP->next(currentClusterP);
     }

   while(currentClusterN)
     {
       if(!currentClusterN->getIdMcHit(0)) n_ghost++;
       nEvaluatedCluster++;
       currentClusterN = mClusterN->next(currentClusterN);
     }

   while(currentSimPoint)
     {
       p_checked = 0;
       n_checked = 0;

       currentClusterP = mClusterP->first();
       while(currentClusterP)
	 {
	   if ( (currentClusterP->getIdMcHit(0) == currentSimPoint->getIdMatch())
		||(currentClusterP->getIdMcHit(1) == currentSimPoint->getIdMatch()) )
	     {
	       p_checked = 1;
	     }
	   currentClusterP = mClusterP->next(currentClusterP);
	 }
       if (p_checked) 
	 {p_true++;}
       else
	 {p_lost++;}

       currentClusterN = mClusterN->first();
       while(currentClusterN)
	 {
	   if ( (currentClusterN->getIdMcHit(0) == currentSimPoint->getIdMatch())
	       ||(currentClusterN->getIdMcHit(1) == currentSimPoint->getIdMatch()) )
	     {
	       n_checked = 1;
	     }
	   currentClusterN = mClusterN->next(currentClusterN);
	 }
       if (n_checked) 
	 {n_true++;}
       else
	 {n_lost++;}

       currentSimPoint = mSimPoint->next(currentSimPoint);
     }

   ctrl[0].TrueClusterP  += p_true;
   ctrl[0].GhostClusterP += p_ghost;
   ctrl[0].LostClusterP  += p_lost;
   ctrl[0].TrueClusterN  += n_true;
   ctrl[0].GhostClusterN += n_ghost;
   ctrl[0].LostClusterN  += n_lost;

   return nEvaluatedCluster;
 }

int StSceWafer::doEvaluateSpt(sce_ctrl_st *ctrl){
//   printf("In doEvaluateSpt  in %d \n",mId);
  int nCompared     = 0;  
  int nEvaluatedSpt = 0;
  StScePoint   *scanRecPoint    = 0;
  StScePoint   *currentRecPoint = 0;
  StScePoint   *scanSimPoint    = 0;
  StScePoint   *currentSimPoint = 0;

  int numPackage     = 0;
  int okHits         = 0;
  int diffProb       = 0;

  int e              = 0;
  int compOk         = 0;
  int prob           = 0;
  int ghostOrTrue    = 0;
  int idHit          = 0;
  float *d2e         = new float[2];
  for (e = 0; e < 2; e++)   d2e[e] = 0;
  float *dXg         = new float[3];
  float *dXl         = new float[3];
  for (e = 0; e < 3; e++)
    { dXg[e] = 0;
      dXl[e] = 0; }

   int  true11  = 0;
   int  ghost11 = 0;
   int  lost11  = 0;
   int  true12  = 0;
   int  ghost12 = 0;
   int  lost12  = 0;
   int  true22  = 0;
   int  ghost22 = 0;
   int  lost22  = 0;
   int  true23  = 0;
   int  ghost23 = 0;
   int  lost23  = 0;
   int  true33  = 0;
   int  ghost33 = 0;
   int  lost33  = 0;

   currentRecPoint = mRecPoint->first();
   while(currentRecPoint){

//!  printf("Point in current treatment %d \n",currentRecPoint->getIdMatch());
     numPackage   = currentRecPoint->getIdCluster();
     switch(currentRecPoint->getIdMatch()){
      
     case 11://CASE 11 !!!
       if(!currentRecPoint->getIdMcHit(0))
	 {
	   ghostOrTrue = 0;
	   prob = currentRecPoint->getFlag();
	   compOk = 1;
	   ghost11++;
	 }
       else if ((currentRecPoint->getIdMcHit(0))&&(!currentRecPoint->getIdMcHit(1)))
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true11++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	 }
       else
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true11++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	   for(e=1;e<5;e++)
	     {
	       if (currentRecPoint->getIdMcHit(e))
		 lost11++; 
	     }
	 }
       if(compOk)
	 {
	   StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
	   mComPoint->addNewComp(newComp);
	   nCompared++;
	 }
       compOk = 0;
       currentRecPoint = mRecPoint->next(currentRecPoint);
       nEvaluatedSpt++;
       break;
       
     case 12:// CASE 12 AND CASE 21 !!!
     case 21:
       if(!(numPackage == (mRecPoint->next(currentRecPoint))->getIdCluster()))
	 {
	   printf("Warning: two points from case 12 or 21 are not consecutive!!\n") ;
	   currentRecPoint = mRecPoint->next(currentRecPoint);
	   break;
	 }
       if(!currentRecPoint->getIdMcHit(0))
	 {
	   ghostOrTrue = 0;
	   prob = currentRecPoint->getFlag();
	   compOk = 1;
	   ghost12++;
	 }
       else if ((currentRecPoint->getIdMcHit(0))&&(!currentRecPoint->getIdMcHit(1)))
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		{
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		  true12++;
		}
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	 }
       else
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true12++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	   for(e=1;e<5;e++)
	     {
	       if (
		   (currentRecPoint->getIdMcHit(e))
		   &&(!(currentRecPoint->getIdMcHit(e)==((mRecPoint->next(currentRecPoint))->getIdMcHit(0))))
		   &&(!(currentRecPoint->getIdMcHit(e)==((mRecPoint->next(currentRecPoint))->getIdMcHit(1))))
		   )
		 lost12++; 
	     }
	 }
       if(compOk)
	 {
	   StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
	   mComPoint->addNewComp(newComp);
	   nCompared++;
	 }
       compOk = 0;
       if(!((mRecPoint->next(currentRecPoint))->getIdMcHit(0)))
	 {
	   ghostOrTrue = 0;
	   prob = (mRecPoint->next(currentRecPoint))->getFlag();
	   compOk = 1;
	   ghost12++;
	 }
       else if (((mRecPoint->next(currentRecPoint))->getIdMcHit(0))&&(!(mRecPoint->next(currentRecPoint))->getIdMcHit(1)))
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if ((mRecPoint->next(currentRecPoint))->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = (mRecPoint->next(currentRecPoint))->getFlag();
		   idHit = (mRecPoint->next(currentRecPoint))->getIdMcHit(0);
		   d2e[0] = 100*((mRecPoint->next(currentRecPoint))->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = (mRecPoint->next(currentRecPoint))->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = (mRecPoint->next(currentRecPoint))->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = (mRecPoint->next(currentRecPoint))->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true12++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	 }
       else
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if ((mRecPoint->next(currentRecPoint))->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = (mRecPoint->next(currentRecPoint))->getFlag();
		   idHit = (mRecPoint->next(currentRecPoint))->getIdMcHit(0);
		   d2e[0] = 100*((mRecPoint->next(currentRecPoint))->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = (mRecPoint->next(currentRecPoint))->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = (mRecPoint->next(currentRecPoint))->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = (mRecPoint->next(currentRecPoint))->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true12++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	   for(e=1;e<5;e++)
	     {
	       if (
		   ((mRecPoint->next(currentRecPoint))->getIdMcHit(e))
		   &&(!((mRecPoint->next(currentRecPoint))->getIdMcHit(e)==currentRecPoint->getIdMcHit(0)))
		   &&(!((mRecPoint->next(currentRecPoint))->getIdMcHit(e)==currentRecPoint->getIdMcHit(1)))
		   )
		 {
		   lost12++;
		 }
	     }
	 }
       if(compOk)
	 {
	   StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
	   mComPoint->addNewComp(newComp);
	   nCompared++;
	 }
       compOk = 0;
       currentRecPoint = mRecPoint->next(currentRecPoint);
       currentRecPoint = mRecPoint->next(currentRecPoint);
       nEvaluatedSpt   = nEvaluatedSpt+2;
       break;
       
     case 221:// CASE 221 AND CASE 222 !!!
     case 222:
       if(!(numPackage == (mRecPoint->next(currentRecPoint))->getIdCluster()))
	 printf("Warning: two points from case 221 or 222 are not consecutive") ;
       
       if(!currentRecPoint->getIdMcHit(0))
	 {
	   ghostOrTrue = 0;
	   prob = currentRecPoint->getFlag();
	   compOk = 1;
	   ghost22++;
	 }
       else if ((currentRecPoint->getIdMcHit(0))&&(!currentRecPoint->getIdMcHit(1)))
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true22++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	 }
       else
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = currentRecPoint->getFlag();
		   idHit = currentRecPoint->getIdMcHit(0);
		   d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = currentRecPoint->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true22++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	   for(e=1;e<5;e++)
	     {
	       if (
		   (currentRecPoint->getIdMcHit(e))
		   &&(!(currentRecPoint->getIdMcHit(e)==((mRecPoint->next(currentRecPoint))->getIdMcHit(0))))
		   &&(!(currentRecPoint->getIdMcHit(e)==((mRecPoint->next(currentRecPoint))->getIdMcHit(1))))
		   )
		 {
		   lost22++;
		 }
	     }
	 }
       if(compOk)
	 {
	   StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
	   mComPoint->addNewComp(newComp);
	   nCompared++;
	 }
       compOk = 0;
       
       if(!((mRecPoint->next(currentRecPoint))->getIdMcHit(0)))
	 {
	   ghostOrTrue = 0;
	   prob = (mRecPoint->next(currentRecPoint))->getFlag();
	   compOk = 1;
	   ghost22++;
	 }
       else if (((mRecPoint->next(currentRecPoint))->getIdMcHit(0))&&(!(mRecPoint->next(currentRecPoint))->getIdMcHit(1)))
	 {
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if ((mRecPoint->next(currentRecPoint))->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = (mRecPoint->next(currentRecPoint))->getFlag();
		   idHit = (mRecPoint->next(currentRecPoint))->getIdMcHit(0);
		   d2e[0] = 100*((mRecPoint->next(currentRecPoint))->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = (mRecPoint->next(currentRecPoint))->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = (mRecPoint->next(currentRecPoint))->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = (mRecPoint->next(currentRecPoint))->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true22++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	 }
       else{
	   currentSimPoint = mSimPoint->first();
	   while(currentSimPoint)
	     {
	       if ((mRecPoint->next(currentRecPoint))->getIdMcHit(0) == currentSimPoint->getIdMatch())
		 {
		   ghostOrTrue = 1;
		   prob = (mRecPoint->next(currentRecPoint))->getFlag();
		   idHit = (mRecPoint->next(currentRecPoint))->getIdMcHit(0);
		   d2e[0] = 100*((mRecPoint->next(currentRecPoint))->getDe(0)/currentSimPoint->getDe(0)-1);
		   d2e[1] = (mRecPoint->next(currentRecPoint))->getDe(1);
		   for (e = 0; e < 3; e++)
		     {
		       dXg[e] = (mRecPoint->next(currentRecPoint))->getXg(e)-currentSimPoint->getXg(e);
		       dXl[e] = (mRecPoint->next(currentRecPoint))->getXl(e)-currentSimPoint->getXl(e);
		     }
		   compOk = 1;
		   true22++;
		 }
	       currentSimPoint = mSimPoint->next(currentSimPoint);
	     }
	   for(e=1;e<5;e++)
	     {
	       if (
		   ((mRecPoint->next(currentRecPoint))->getIdMcHit(e))
		   &&(!((mRecPoint->next(currentRecPoint))->getIdMcHit(e)==currentRecPoint->getIdMcHit(0)))
		   &&(!((mRecPoint->next(currentRecPoint))->getIdMcHit(e)==currentRecPoint->getIdMcHit(1)))
		   )
		 {
		   lost22++;
		 }
	     }
	 }
       if(compOk)
	 {
	   StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, (mRecPoint->next(currentRecPoint))->getIdMatch(), idHit, mId, d2e, dXg, dXl);
	   mComPoint->addNewComp(newComp);
	   nCompared++;
	 }
       compOk = 0;
       
       currentRecPoint = mRecPoint->next(currentRecPoint);
       currentRecPoint = mRecPoint->next(currentRecPoint);
       nEvaluatedSpt   = nEvaluatedSpt+2;
       break;
       
     case 223:// CASE 223 !!!

//! 	Checking loop!!!
//! 	if(currentRecPoint)
//! 	  {
//!         scanRecPoint = currentRecPoint;
//! 	    while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
//! 	      {
//!  		printf("Point scanne = %d, proba= %d, Id0 = %d, Id1 = %d \n",scanRecPoint->getNPoint(), scanRecPoint->getFlag(), scanRecPoint->getIdMcHit(0), scanRecPoint->getIdMcHit(1));
//! 		scanRecPoint = mRecPoint->next(scanRecPoint);
//! 	      }
//! 	  }

	if(currentRecPoint)
	  {
	    scanRecPoint = currentRecPoint;
	    while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	      {
		if(!(scanRecPoint->getFlag()==50))
		  {diffProb = 1;}
		scanRecPoint = mRecPoint->next(scanRecPoint);
	      }
	    if(!diffProb)
	      {
		true22  = true22  + 2;
		ghost22 = ghost22 + 2;
//! 		printf("Warning: all probabilities are the same: 2 true et 2 ghost \n");
	      }
	  }
	if(diffProb)
	  {	    
	    scanRecPoint = currentRecPoint;
	    int indexHit = 0;
	    int indexTab = 0;
	    int tabHit[2];
	    tabHit[0] = 0;
	    tabHit[1] = 0;
	    while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	      {
		for(indexHit = 0;indexHit<5;indexHit++)
		  {
		    if(
		       (indexTab<2)&&
		       (scanRecPoint->getIdMcHit(indexHit))&&
		       (tabHit[0]!=scanRecPoint->getIdMcHit(indexHit))&&
		       (tabHit[1]!=scanRecPoint->getIdMcHit(indexHit))
		      )
		       {
			 tabHit[indexTab] = scanRecPoint->getIdMcHit(indexHit);
			 indexTab++;
		       }
		  }
		scanRecPoint = mRecPoint->next(scanRecPoint);
	      }
	    if((tabHit[0])&&(tabHit[1])) okHits = 1;
//!  	    printf("tabHit[0]= %d, tabHit[1]= %d, 2Hits= %d \n", tabHit[0] , tabHit[1], okHits); !! Checking
	    if(okHits)
	      {
		StScePoint   *keepPoint1 = 0;
		StScePoint   *keepPoint2 = 0;
		StScePoint   *downPoint1 = 0;
		StScePoint   *downPoint2 = 0;
		scanRecPoint = currentRecPoint ; 
		while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
		  {
		    if((scanRecPoint->getFlag()>=50)&&(!keepPoint1))
		      {keepPoint1=scanRecPoint;}
		    if((scanRecPoint->getFlag()>=50)&&(keepPoint1))
		      {keepPoint2=scanRecPoint;}
		    if((scanRecPoint->getFlag()<50)&&(!downPoint1))
		      {downPoint1=scanRecPoint;}
		    if((scanRecPoint->getFlag()<50)&&(downPoint1))
		      {downPoint2=scanRecPoint;}
		    scanRecPoint = mRecPoint->next(scanRecPoint);
		  }

		StScePoint   *simPoint1 = 0;
		StScePoint   *simPoint2 = 0;
		scanSimPoint = mSimPoint->first();
		while(scanSimPoint)
		  {
		    if((scanSimPoint->getIdMatch()==tabHit[0])&&(!simPoint1))
		      simPoint1=scanSimPoint;
		    scanSimPoint = mSimPoint->next(scanSimPoint);
		  }
		scanSimPoint = mSimPoint->first();
		while(scanSimPoint)
		  {
		    if((scanSimPoint->getIdMatch()==tabHit[1])&&(simPoint1))
		      simPoint2=scanSimPoint;
		    scanSimPoint = mSimPoint->next(scanSimPoint);
		  }
		if((!keepPoint1)||(!keepPoint2)||(!downPoint1)||(!downPoint2)||(!simPoint1)||(!simPoint2))
		  {
		    ghost22 = ghost22 +2; 
		    lost22  = lost22  +2; 
		  }
		else
		  {
		    if((keepPoint1->getIdMcHit(0)==tabHit[0])&&(keepPoint2->getIdMcHit(0)==tabHit[1])){
		      if((downPoint1->getIdMcHit(0)==tabHit[0])&&(downPoint2->getIdMcHit(0)==tabHit[1]))
			{
			  if(
			     (fabs(keepPoint1->getXl(0)-simPoint1->getXl(0))+fabs(keepPoint2->getXl(0)-simPoint2->getXl(0))
			      <fabs(downPoint1->getXl(0)-simPoint1->getXl(0))+fabs(downPoint2->getXl(0)-simPoint2->getXl(0)))
			     &&(fabs(keepPoint1->getXl(1)-simPoint1->getXl(1))+fabs(keepPoint2->getXl(1)-simPoint2->getXl(1))
				<fabs(downPoint1->getXl(1)-simPoint1->getXl(1))+fabs(downPoint2->getXl(1)-simPoint2->getXl(1)))
			     )
			    {
			      ghostOrTrue = 1;
			      true22 = true22 + 2;
			    }
			  else
			    {
			      ghostOrTrue = 0;
			      ghost22 = ghost22 +2; 
			      lost22  = lost22  +2; 
			    }

			  prob = keepPoint1->getFlag();
			  idHit = keepPoint1->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint1->getDe(0)/simPoint1->getDe(0)-1);
			  d2e[1] = keepPoint1->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint1->getXg(e)-simPoint1->getXg(e);
			      dXl[e] = keepPoint1->getXl(e)-simPoint1->getXl(e);
			    }
			  StSceComp *newComp1 = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp1);
			  nCompared++;
			  
			  prob = keepPoint2->getFlag();
			  idHit = keepPoint2->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint2->getDe(0)/simPoint2->getDe(0)-1);
			  d2e[1] = keepPoint2->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint2->getXg(e)-simPoint2->getXg(e);
			      dXl[e] = keepPoint2->getXl(e)-simPoint2->getXl(e);
			    }
			  StSceComp *newComp2 = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp2);
			  nCompared++;
			}
		      else if((downPoint1->getIdMcHit(0)==tabHit[1])&&(downPoint2->getIdMcHit(0)==tabHit[0]))
			{
			  if(
			     (fabs(keepPoint1->getXl(0)-simPoint1->getXl(0))+fabs(keepPoint2->getXl(0)-simPoint2->getXl(0))
			      <fabs(downPoint1->getXl(0)-simPoint2->getXl(0))+fabs(downPoint2->getXl(0)-simPoint1->getXl(0)))
			     &&(fabs(keepPoint1->getXl(1)-simPoint1->getXl(1))+fabs(keepPoint2->getXl(1)-simPoint2->getXl(1))
				<fabs(downPoint1->getXl(1)-simPoint2->getXl(1))+fabs(downPoint2->getXl(1)-simPoint1->getXl(1)))
			     )
			    {
			      ghostOrTrue = 1;
			      true22 = true22 + 2;
			    }
			  else
			    {
			      ghostOrTrue = 0;
			      ghost22 = ghost22 +2; 
			      lost22  = lost22  +2; 
			    }
			  prob = keepPoint1->getFlag();
			  idHit = keepPoint1->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint1->getDe(0)/simPoint1->getDe(0)-1);
			  d2e[1] = keepPoint1->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint1->getXg(e)-simPoint1->getXg(e);
			      dXl[e] = keepPoint1->getXl(e)-simPoint1->getXl(e);
			    }
			  StSceComp *newComp1 = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp1);
			  nCompared++;
			  
			  prob = keepPoint2->getFlag();
			  idHit = keepPoint2->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint2->getDe(0)/simPoint2->getDe(0)-1);
			  d2e[1] = keepPoint2->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint2->getXg(e)-simPoint2->getXg(e);
			      dXl[e] = keepPoint2->getXl(e)-simPoint2->getXl(e);
			    }
			  StSceComp *newComp2 = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp2);
			  nCompared++;
			}
		      else
			true22 = true22 +2; 
		    }
		    
		    else if((keepPoint1->getIdMcHit(0)==tabHit[1])&&(keepPoint2->getIdMcHit(0)==tabHit[0]))
		      {
			if((downPoint1->getIdMcHit(0)==tabHit[0])&&(downPoint2->getIdMcHit(0)==tabHit[1])) 
			  {
			    if(
			       (fabs(keepPoint1->getXl(0)-simPoint2->getXl(0))+fabs(keepPoint2->getXl(0)-simPoint1->getXl(0))
				<fabs(downPoint1->getXl(0)-simPoint1->getXl(0))+fabs(downPoint2->getXl(0)-simPoint2->getXl(0)))
			       &&(fabs(keepPoint1->getXl(1)-simPoint2->getXl(1))+fabs(keepPoint2->getXl(1)-simPoint1->getXl(1))
				  <fabs(downPoint1->getXl(1)-simPoint1->getXl(1))+fabs(downPoint2->getXl(1)-simPoint2->getXl(1)))
			       )
			      {
				ghostOrTrue = 1;
				true22 = true22 + 2;
			      }
			    else
			      {
				ghostOrTrue = 0;
				ghost22 = ghost22 +2; 
				lost22  = lost22  +2; 
			      }
			  prob = keepPoint1->getFlag();
			  idHit = keepPoint1->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint1->getDe(0)/simPoint2->getDe(0)-1);
			  d2e[1] = keepPoint1->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint1->getXg(e)-simPoint2->getXg(e);
			      dXl[e] = keepPoint1->getXl(e)-simPoint2->getXl(e);
			    }
			  StSceComp *newComp1 = new StSceComp(nCompared, prob, ghostOrTrue, keepPoint1->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp1);
			  nCompared++;
			  
			  prob = keepPoint2->getFlag();
			  idHit = keepPoint2->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint2->getDe(0)/simPoint1->getDe(0)-1);
			  d2e[1] = keepPoint2->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint2->getXg(e)-simPoint1->getXg(e);
			      dXl[e] = keepPoint2->getXl(e)-simPoint1->getXl(e);
			    }
			  StSceComp *newComp2 = new StSceComp(nCompared, prob, ghostOrTrue, keepPoint2->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp2);
			  nCompared++;
			  }
			else if((downPoint1->getIdMcHit(0)==tabHit[1])&&(downPoint2->getIdMcHit(0)==tabHit[0])) 
			  {
			    if(
			       (fabs(keepPoint1->getXl(0)-simPoint2->getXl(0))+fabs(keepPoint2->getXl(0)-simPoint1->getXl(0))
				<fabs(downPoint1->getXl(0)-simPoint2->getXl(0))+fabs(downPoint2->getXl(0)-simPoint1->getXl(0)))
			       &&(fabs(keepPoint1->getXl(1)-simPoint2->getXl(1))+fabs(keepPoint2->getXl(1)-simPoint1->getXl(1))
				  <fabs(downPoint1->getXl(1)-simPoint2->getXl(1))+fabs(downPoint2->getXl(1)-simPoint1->getXl(1)))
			       )
			      {
				ghostOrTrue = 1;
				true22 = true22 + 2;
			      }
			    else
			      {
				ghostOrTrue = 0;
				ghost22 = ghost22 +2; 
				lost22  = lost22  +2; 
			      }
			  prob = keepPoint1->getFlag();
			  idHit = keepPoint1->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint1->getDe(0)/simPoint2->getDe(0)-1);
			  d2e[1] = keepPoint1->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint1->getXg(e)-simPoint2->getXg(e);
			      dXl[e] = keepPoint1->getXl(e)-simPoint2->getXl(e);
			    }
			  StSceComp *newComp1 = new StSceComp(nCompared, prob, ghostOrTrue, keepPoint1->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp1);
			  nCompared++;
			  
			  prob = keepPoint2->getFlag();
			  idHit = keepPoint2->getIdMcHit(0);
			  d2e[0] = 100*(keepPoint2->getDe(0)/simPoint1->getDe(0)-1);
			  d2e[1] = keepPoint2->getDe(1);
			  for (e = 0; e < 3; e++)
			    {
			      dXg[e] = keepPoint2->getXg(e)-simPoint1->getXg(e);
			      dXl[e] = keepPoint2->getXl(e)-simPoint1->getXl(e);
			    }
			  StSceComp *newComp2 = new StSceComp(nCompared, prob, ghostOrTrue, keepPoint2->getIdMatch(), idHit, mId, d2e, dXg, dXl);
			  mComPoint->addNewComp(newComp2);
			  nCompared++;
			  }
			else
			  true22 = true22 +2; 
		      }
		    
		    else
		      {
			if(
			   (downPoint1->getIdMcHit(0)==tabHit[0])&&(downPoint2->getIdMcHit(0)==tabHit[1]) 
			   ||(downPoint1->getIdMcHit(0)==tabHit[1])&&(downPoint2->getIdMcHit(0)==tabHit[0]) 
			   )
			  {
			    ghost22 = ghost22 +2; 
			    lost22  = lost22  +2; 
			  }
			else
			  ghost22 = ghost22 +2; 
		      }
		  }
	      }
	  }
	
	currentRecPoint = mRecPoint->next(currentRecPoint);
	currentRecPoint = mRecPoint->next(currentRecPoint);
	currentRecPoint = mRecPoint->next(currentRecPoint);
	currentRecPoint = mRecPoint->next(currentRecPoint);
	nEvaluatedSpt   = nEvaluatedSpt+4;
	break;
       
     case 23:// CASES 23 & 32!!!
     case 32:
       if(currentRecPoint){
	 int okHit = 0;
	 if((currentRecPoint->getFlag()>=50)&&(currentRecPoint->getIdMcHit(0))) 
	   { okHit=1; }
	 else if ((currentRecPoint->getFlag()>=50)&&(!(currentRecPoint->getIdMcHit(0))))
	   {
	     ghostOrTrue = 0;
	     prob = currentRecPoint->getFlag();
	     compOk = 1;
	     ghost23++;
	   }
	 else if ((currentRecPoint->getFlag()<50)&&(currentRecPoint->getIdMcHit(0)))
	   { lost23++; }
	 if(okHit)
	   {
	     scanRecPoint=currentRecPoint;
	     while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	       {
		 scanRecPoint = mRecPoint->prev(scanRecPoint);
	       }
	     if(scanRecPoint)
	       {scanRecPoint = mRecPoint->next(scanRecPoint);}
	     else
	       {scanRecPoint = currentRecPoint;}
	     while((okHit)&&(scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	       {
		 if(
		    (!(scanRecPoint->getNPoint()==currentRecPoint->getNPoint()))
		    &&(scanRecPoint->getIdMcHit(0)==currentRecPoint->getIdMcHit(0)) 
		    &&(scanRecPoint->getFlag()>=50) 
		    )
		   { okHit=0; }
		 scanRecPoint = mRecPoint->next(scanRecPoint);
	       }
	   }
	 if(okHit)
	   {
	     currentSimPoint = mSimPoint->first();
	     while(currentSimPoint)
	       {
		 if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		   {
		     ghostOrTrue = 1;
		     prob = currentRecPoint->getFlag();
		     idHit = currentRecPoint->getIdMcHit(0);
		     d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		     d2e[1] = currentRecPoint->getDe(1);
		     for (e = 0; e < 3; e++)
		       {
			 dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
			 dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		       }
		     StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
		     mComPoint->addNewComp(newComp);
		     nCompared++;
		   }
		 currentSimPoint = mSimPoint->next(currentSimPoint);
	       }
	     true23++;
	     if(currentRecPoint->getIdMcHit(1)) lost23++;
	   }
       }
       currentRecPoint = mRecPoint->next(currentRecPoint);
       nEvaluatedSpt++;
       break;
       
     case 33:// CASE 33 !!!
       if(currentRecPoint){
	 int okHit = 0;
	 if((currentRecPoint->getFlag()>=50)&&(currentRecPoint->getIdMcHit(0))) 
	   { okHit=1;}
	 else if ((currentRecPoint->getFlag()>=50)&&(!(currentRecPoint->getIdMcHit(0))))
	   {
	     ghostOrTrue = 0;
	     prob = currentRecPoint->getFlag();
	     compOk = 1;
	     ghost33++;
	   }
	 else if ((currentRecPoint->getFlag()<50)&&(currentRecPoint->getIdMcHit(0)))
	   { lost33++;
	   }
	 
	 if(okHit)
	   {
	     scanRecPoint=currentRecPoint;
	     while((scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	       {scanRecPoint = mRecPoint->prev(scanRecPoint);}
	     if(scanRecPoint)
	       {scanRecPoint = mRecPoint->next(scanRecPoint);}
	     else
	       {scanRecPoint = currentRecPoint;}
	     while((okHit)&&(scanRecPoint)&&(scanRecPoint->getIdCluster()==numPackage))
	       {
		 if(
		    (!(scanRecPoint->getNPoint()==currentRecPoint->getNPoint()))
		    &&(scanRecPoint->getIdMcHit(0)==currentRecPoint->getIdMcHit(0)) 
		    &&(scanRecPoint->getFlag()>=50) 
		    )
		   { okHit=0;}
		 scanRecPoint = mRecPoint->next(scanRecPoint);
	       }
	   }
	 if(okHit)
	   {
	     currentSimPoint = mSimPoint->first();
	     while(currentSimPoint)
	       {
		 if (currentRecPoint->getIdMcHit(0) == currentSimPoint->getIdMatch())
		   {
		     ghostOrTrue = 1;
		     prob = currentRecPoint->getFlag();
		     idHit = currentRecPoint->getIdMcHit(0);
		     d2e[0] = 100*(currentRecPoint->getDe(0)/currentSimPoint->getDe(0)-1);
		     d2e[1] = currentRecPoint->getDe(1);
		     for (e = 0; e < 3; e++)
		       {
			 dXg[e] = currentRecPoint->getXg(e)-currentSimPoint->getXg(e);
			 dXl[e] = currentRecPoint->getXl(e)-currentSimPoint->getXl(e);
		       }
		     StSceComp *newComp = new StSceComp(nCompared, prob, ghostOrTrue, currentRecPoint->getIdMatch(), idHit, mId, d2e, dXg, dXl);
		     mComPoint->addNewComp(newComp);
		     nCompared++;
		   }
		 currentSimPoint = mSimPoint->next(currentSimPoint);
	       }
	     true33++;
	     if(currentRecPoint->getIdMcHit(1))
	       lost33++;
	   }
       }
       currentRecPoint = mRecPoint->next(currentRecPoint);
       nEvaluatedSpt++;
       break;
       
     default:
       printf("Warning: point undefined %d \n",currentRecPoint->getIdMatch());
       currentRecPoint = mRecPoint->next(currentRecPoint);
       
     }
   }
   
   ctrl[0].TrueSpt11  += true11;
   ctrl[0].GhostSpt11 += ghost11;
   ctrl[0].LostSpt11  += lost11;
   ctrl[0].TrueSpt12  += true12;
   ctrl[0].GhostSpt12 += ghost12;
   ctrl[0].LostSpt12  += lost12;
   ctrl[0].TrueSpt22  += true22;
   ctrl[0].GhostSpt22 += ghost22;
   ctrl[0].LostSpt22  += lost22;
   ctrl[0].TrueSpt23  += true23;
   ctrl[0].GhostSpt23 += ghost23;
   ctrl[0].LostSpt23  += lost23;
   ctrl[0].TrueSpt33  += true33;
   ctrl[0].GhostSpt33 += ghost33;
   ctrl[0].LostSpt33  += lost33;

   delete [] d2e;
   delete [] dXg;
   delete [] dXl;
   return nEvaluatedSpt;
 }


float* StSceWafer::findAngle(float *p, float *alpha)
{

  int i = 0;

  float pT[3],pN[3],pD[3];

  float spT = 0.;
  float spN = 0.;
  float spD = 0.;

  float npT = 0.;
  float npN = 0.;
  float npD = 0.;

  for (i = 0; i < 3; i++)
    {
      spN  += mN[i]*p[i]  ;
      spT  += mT[i]*p[i]  ;
      spD  += mD[i]*p[i]  ;
    }
  for (i = 0; i < 3; i++)
    {
      pN[i] = mN[i]*spN  ;
      pT[i] = mT[i]*spT  ;
      pD[i] = mD[i]*spD  ;
    }

  npT = sqrt(pT[0]*pT[0]+pT[1]*pT[1]+pT[2]*pT[2]);
  npD = sqrt(pD[0]*pD[0]+pD[1]*pD[1]+pD[2]*pD[2]);
  npN = sqrt(pN[0]*pN[0]+pN[1]*pN[1]+pN[2]*pN[2]);

//   float np    = sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  float npDN  = sqrt((pN[0]+pD[0])*(pN[0]+pD[0])+(pN[1]+pD[1])*(pN[1]+pD[1])+(pN[2]+pD[2])*(pN[2]+pD[2]));
  float npTD  = sqrt((pT[0]+pD[0])*(pT[0]+pD[0])+(pT[1]+pD[1])*(pT[1]+pD[1])+(pT[2]+pD[2])*(pT[2]+pD[2]));

  alpha[0] = acos(npN/npDN);
  float sSign = 0.;
  sSign = pD[0]*pN[0]+pD[1]*pN[1]+pD[2]*pN[2]+npN*npN;
  if (sSign<0.) alpha[0] = -1.*alpha[0];

  alpha[1] = acos(npD/npTD);
  sSign = pD[0]*pT[0]+pD[1]*pT[1]+pD[2]*pT[2]+npD*npD;
  if (sSign<0.) alpha[1] = -1.*alpha[1];

  return alpha;
}
