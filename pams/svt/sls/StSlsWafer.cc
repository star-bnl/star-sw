#include "StSlsWafer.hh"

StSlsWafer::StSlsWafer(int nid)
{
  mId      = nid;
  mD       = new float[3];
  mT       = new float[3];
  mN       = new float[3];
  mX       = new float[3];
  mPoint   = new StSlsListPoint();
  mStripP  = new StSlsListStrip();
  mStripN  = new StSlsListStrip();

  for (int i = 0; i < 3; i++)
    {
      mD[i] =  0. ;
      mT[i] =  0. ;
      mN[i] =  0. ;
      mX[i] =  0. ;
    }
}

StSlsWafer::~StSlsWafer()
{
  delete [] mD;
  delete [] mT;
  delete [] mN;
  delete [] mX;
  delete    mPoint;
  delete    mStripP;
  delete    mStripN;
}


void StSlsWafer::init(int rId, float *rD, float *rT, float *rN, float *rX)
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

void StSlsWafer::addHit(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *p)
{
  float *alpha = new float[2];
  alpha[0]  = 0.;
  alpha[1]  = 0.;
  StSlsPoint *tmpPoint = new StSlsPoint(rNId, rMcHit, rMcTrack, rXg, rDe, this->findAngle(p,alpha));
  (this->mPoint)->addNewPoint(tmpPoint);
  delete [] alpha;
}
  
int StSlsWafer::convertGlobalToLocal()
{
  int localSize = (this->mPoint)->getSize();

  if (!localSize) return 0;

  float *xtemp = new float[3];  
  StSlsPoint *temp = mPoint->first();
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

int StSlsWafer::convertLocalToUFrame(float ActiveLargeEdge, float ActiveSmallEdge, float Theta) 
{
  int localSize = (this->mPoint)->getSize();
  if (!localSize) return 0;

  StSlsPoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)

    {
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))-(temp->getXl(1)+(ActiveSmallEdge/2.))*tan(Theta), 0);
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))+(temp->getXl(1)-(ActiveSmallEdge/2.))*tan(Theta), 1); 
      temp = mPoint->next(temp);
    }
  return 1;
} 

StSlsListPoint* StSlsWafer::getDeadHits(float ActiveLargeEdge, float ActiveSmallEdge,float Test)
{
  StSlsListPoint *listDeadBorder   = this->getNonActivePointBorder(ActiveLargeEdge,ActiveSmallEdge);
  StSlsListPoint *listDeadTriangle = this->getNonActivePointTriangle(Test);
  StSlsListPoint *listDeadTotal    = new StSlsListPoint();
  listDeadTotal = listDeadTotal->addListPoint(listDeadBorder);
  listDeadTotal = listDeadTotal->addListPoint(listDeadTriangle);
  listDeadTotal = listDeadTotal->removeMultipleCount();

  (this->mPoint)->substractListPoint(listDeadTotal);
  delete listDeadBorder;
  delete listDeadTriangle;
  return listDeadTotal;
}
  
void StSlsWafer::convertToStrip(float Pitch, 
				int nStripPerSide,
				double PairCreationEnergy,
				int NStripInACluster,
				double ParDiffP,
				double ParDiffN,
				double ParIndRightP,
				double ParIndRightN,
				double ParIndLeftP,
				double ParIndLeftN)
{
  this->convertHitToStrip(Pitch, nStripPerSide,
			  NStripInACluster,
			  ParDiffP,
			  ParDiffN,
			  ParIndRightP,
			  ParIndRightN,
			  ParIndLeftP,
			  ParIndLeftN);
  this->convertAnalogToDigit(PairCreationEnergy);
  (this->mStripP)->sortStrip();
  (this->mStripN)->sortStrip();
}

int StSlsWafer::getId()
{  return mId; }

float* StSlsWafer::getD()
{  return mD; }

float* StSlsWafer::getT()
{  return mT; }

float* StSlsWafer::getN()
{  return mN; }

float* StSlsWafer::getX()
{  return mX; }

StSlsListPoint*  StSlsWafer::getPoint()
{  return mPoint; }

StSlsListStrip* StSlsWafer::getStripP()
{  return mStripP; }   

StSlsListStrip*  StSlsWafer::getStripN()
{  return mStripN; }   


StSlsListPoint*  StSlsWafer::getNonActivePointBorder(float ActiveLargeEdge, float ActiveSmallEdge)
{
  int localSize = (this->mPoint)->getSize();

  StSlsListPoint *deadPoints = new StSlsListPoint();
  if (!localSize) return deadPoints;
  
  StSlsPoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)
    {
      if((temp->getXl(0) >(ActiveLargeEdge/2.)) || (temp->getXl(0) < (-ActiveLargeEdge/2.)) || 
	 (temp->getXl(1) >(ActiveSmallEdge/2.)) || (temp->getXl(1) < (-ActiveSmallEdge/2.)))
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StSlsPoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (this->mPoint)->next(temp);
    }
  return deadPoints;
} 

StSlsListPoint* StSlsWafer::getNonActivePointTriangle(float Test)
  //typically, test=pitch
{
  int localSize = (this->mPoint)->getSize();
  StSlsListPoint *deadPoints = new StSlsListPoint();  

  if (!localSize) return deadPoints;

  StSlsPoint *temp = (this->mPoint)->first();
  for (int i = 0; i < localSize; i++)
    {
      if (temp->getUpos(0) < -1.*Test && temp->getUpos(1) < -1.*Test) 
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StSlsPoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (this->mPoint)->next(temp);
    }
  return deadPoints;
} 


double StSlsWafer::myErf(double x)
{
  const double a1 = -1.26551223,   a2 = 1.00002368,
               a3 =  0.37409196,   a4 = 0.09678418,
               a5 = -0.18628806,   a6 = 0.27886807,
               a7 = -1.13520398,   a8 = 1.48851587,
               a9 = -0.82215223,  a10 = 0.17087277;
  
  double v = 1.;
  double z = ((x) < 0. ? -(x) : (x));
  
  if (z <= 0) return (1.-v); // erfc(0)=1
  double t = 1./(1.+0.5*z);
  v = t*exp((-z*z) +a1+t*(a2+t*(a3+t*(a4+t*(a5+t*(a6+t*(a7+t*(a8+t*(a9+t*a10)))))))));
  
  if (x < 0) v = 2.-v; // erfc(-x)=2-erfc(x)
  return (1.-v);
}

void StSlsWafer::convertHitToStrip(float Pitch, 
				   int nStripPerSide,
				   int NStripInACluster,
				   double ParDiffP,
				   double ParDiffN,
				   double ParIndRightP,
				   double ParIndRightN,
				   double ParIndLeftP,
				   double ParIndLeftN)
{
  const double parDiff[2]={ParDiffP/Pitch,ParDiffN/Pitch};
  const double parIndRight[2]={ParIndRightP,ParIndRightN};
  const double parIndLeft[2]={ParIndLeftP,ParIndLeftN};

  int   *tabInd   = new int[NStripInACluster];
  float *tabDe    = new float[NStripInACluster];

  StSlsPoint *ptr = (this->mPoint)->first();
  int localSize = (this->mPoint)->getSize();
  for (int iPoint = 0; iPoint < localSize; iPoint++)
    {
      for (int iSide = 0; iSide < 2; iSide++)
	{
	  for (int v = 0 ; v < NStripInACluster; v++) 
	    {
	      tabInd[v] = 0 ;
	      tabDe[v]  = 0.;
	    }
	  tabInd[0] = (int)(ptr->getUpos(iSide)/Pitch + 1.);
	  tabInd[1] = tabInd[0]+1;
	  tabInd[2] = tabInd[0]-1;
	  tabInd[3] = tabInd[0]+2;
	  
	  double rest = (double)(ptr->getUpos(iSide)/Pitch) - (double)(tabInd[0]-1);
	  double Result=0.5*(1.+myErf((rest-0.5)/sqrt(2.)/parDiff[iSide]) );
	  float TmpDe0 = 0.;
	  float TmpDe1 = 0.;
	  tabDe[0] = (1.-Result)*ptr->getDe();
	  tabDe[1] = Result*ptr->getDe();
	  tabDe[2] = tabDe[0]*parIndLeft[iSide];
	  tabDe[3] = tabDe[1]*parIndRight[iSide];
	  TmpDe0 = tabDe[1]*parIndLeft[iSide];
	  TmpDe1 = tabDe[0]*parIndRight[iSide];
	  tabDe[0] += TmpDe0;
	  tabDe[1] += TmpDe1;
	  for (int st = 0; st <  NStripInACluster; st++)
	    {
	      if ( tabInd[st] > 0 && tabInd[st] < nStripPerSide+1 )
		{
		  StSlsStrip *newStrip = new StSlsStrip(tabInd[st], ptr->getNId(), ptr->getMcHit(), ptr->getMcTrack(), tabDe[st]);

		  switch (iSide)
		    {
		    case 0:
		      (this->mStripP)->updateStrip(newStrip);
		      break;
		    case 1:
		      (this->mStripN)->updateStrip(newStrip);
		      break;
		    }
		}
	    }
	}
      ptr = (this->mPoint)->next(ptr);
    }
  delete [] tabInd;
  delete [] tabDe;
}

void StSlsWafer::convertAnalogToDigit(double PairCreationEnergy)
{
  const double ConversionFactor = 1./PairCreationEnergy;//GeV

  int localSize    = (this->mStripP)->getSize();
  StSlsStrip *curr = (this->mStripP)->first();
  int i = 0;
  for (i = 0; i < localSize; i++)
    {
      curr->setDigitSig((int)(curr->getAnalogSig()*ConversionFactor));
      curr = (this->mStripP)->next(curr);
    }
  localSize  = (this->mStripN)->getSize();
  curr = (this->mStripN)->first();
  for (i = 0; i < localSize; i++)
    {
      curr->setDigitSig((int)(curr->getAnalogSig()*ConversionFactor));
      curr = (this->mStripN)->next(curr);
    }
}

float* StSlsWafer::findAngle(float *p, float *alpha)
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
