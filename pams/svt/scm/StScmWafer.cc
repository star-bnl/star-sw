#include "StScmWafer.hh"

// StScmWafer::StScmWafer(int nid ,int *nDeadStripP, int *nDeadStripN)
StScmWafer::StScmWafer(int nid )
{
  mId         = nid;
  mD          = new float[3];
  mT          = new float[3];
  mN          = new float[3];
  mX          = new float[3];
//   mDeadStripP = nDeadStripP;
//   mDeadStripN = nDeadStripN;

  mClusterP   = new StScmListCluster();
  mClusterN   = new StScmListCluster();
  mPackage    = new StScmListPackage();
  mPoint      = new StScmListPoint();

  for(int i = 0; i < 3; i++)
    {
      mD[i]   = 0; 
      mT[i]   = 0; 
      mN[i]   = 0; 
      mX[i]   = 0;
    } 
}

StScmWafer::~StScmWafer()
{
  delete[]  mD;
  delete[]  mT;
  delete[]  mN;
  delete[]  mX;

//   delete    mDeadStripP;
//   delete    mDeadStripN;

  delete    mClusterP;
  delete    mClusterN;
  delete    mPackage;
  delete    mPoint;
}

void StScmWafer::init(int rId, float *rD, float *rT, float *rN, float *rX)
{
  if (rId != mId)
    {
      cout<<" Can not initialize wafer number : "<<mId<<"\n";
    }
  else
    {
      for (int i = 0; i < 3; i++)
	{
	  mD[i] = rD[i];
	  mT[i] = rT[i];
	  mN[i] = rN[i];
	  mX[i] = rX[i];
	}
    }
}

StScmListCluster* StScmWafer::getClusterP()
{  return mClusterP; }   

StScmListCluster* StScmWafer::getClusterN()
{  return mClusterN; }   

StScmListPackage* StScmWafer::getPackage()
{  return mPackage; }   

StScmListPoint*   StScmWafer::getPoint()
{  return mPoint; }  


void StScmWafer::addCluster(StScmCluster *ptr, int iSide)
{
  if (iSide)
    { mClusterN->addNewCluster(ptr); }
  else
    { mClusterP->addNewCluster(ptr); }
}

void StScmWafer::addPackage(StScmPackage *ptr)
{  mPackage->addNewPackage(ptr); }

void StScmWafer::addPoint(StScmPoint *ptr)
{  mPoint->addNewPoint(ptr); }


void StScmWafer::sortCluster()
{  mClusterP->sortCluster();
   mClusterN->sortCluster(); }

void StScmWafer::sortPoint()
{  mPoint->sortPoint(); }


int StScmWafer::doFindPackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl)
{
  StScmListPackage *currentListPackage = 0;
  StScmCluster     *currentClusterP    = 0;
  StScmCluster     *currentClusterN    = 0;
  StScmCluster     *scanClusterP       = 0;
  StScmCluster     *scanClusterN       = 0;
  StScmCluster     *lastMatchedN       = 0;
  StScmCluster     *nextMatchedN       = 0;

  int maxMatchedInPackage = scm_ctrl[0].clusterTreat;
  int numPackage         = 0;
  int numUnMatched       = 0;
  int numCurrentClusterP = 0;
  int numCurrentClusterN = 0;
  int numScanClusterP    = 0;
  int numScanClusterN    = 0;
  int numLastMatchedN    = 0;
  int numNextMatchedN    = 0;
  int matchedOk          = 0;
  int keepPackage        = 0;

  currentListPackage = mPackage;
  currentClusterP    = mClusterP->first();
  currentClusterN    = mClusterN->first();

  if (!mClusterP->getSize() || !mClusterN->getSize()) return 0;
  StScmPackage *currentPackage = new StScmPackage(0, scm_ctrl);

  while (currentClusterP)
    {
      matchedOk = 0;
      keepPackage = 0;
      scanClusterP = currentClusterP ;
      currentPackage->addNewMatched(currentClusterP, maxMatchedInPackage);
      currentPackage->addKindPackage(numCurrentClusterP+1,0, maxMatchedInPackage);
      scanClusterN = currentClusterN ;
      while (scanClusterN)
	{
	  if (geoMatched(geom_par, scanClusterP, scanClusterN))
	    {
	      matchedOk++;
	      currentPackage->addNewMatched(scanClusterN, maxMatchedInPackage);
	      currentPackage->addKindPackage(numLastMatchedN+1,1, maxMatchedInPackage);
	      lastMatchedN = scanClusterN;
	      numLastMatchedN++;
	    }
	  scanClusterN = mClusterN->next(scanClusterN);
	}
      if (!(numScanClusterP == mClusterP->getSize()-1))
	{ 
	  scanClusterP = mClusterP->next(scanClusterP);
	  numScanClusterP++;
	  scanClusterN = lastMatchedN;
	  numScanClusterN = numLastMatchedN;
	  while (scanClusterN)
	    {
	      if (geoMatched(geom_par, scanClusterP, scanClusterN)) 
		{
		  keepPackage = 1;
		  nextMatchedN = scanClusterN;
		  numScanClusterN--;
		  numNextMatchedN = numScanClusterN;
		}
	      scanClusterN = mClusterN->prev(scanClusterN);
	    }
	  if (!keepPackage)
	    {
	      numCurrentClusterP = 0;
	      numCurrentClusterN = 0;
	      numScanClusterP    = 0;
	      numScanClusterN    = 0;
	      numLastMatchedN    = 0;
	      if (!matchedOk)
		{
		  numUnMatched++;
		  currentPackage->purgePackage();
		}
	      else
		{
		  currentClusterN = mClusterN->next(lastMatchedN);
		  if (currentPackage)
		    {
		      StScmPackage *newPackage = new StScmPackage(currentListPackage->getSize(), currentPackage->getSize());
		      newPackage->takeMatcheds(currentPackage);
		      currentListPackage->addNewPackage(newPackage);
		      currentPackage->purgePackage();
		      numPackage++;
		    }
		}
	    }
	  else
	    {
	      currentClusterN = nextMatchedN;
	      numCurrentClusterP++;
	      numCurrentClusterN++;
	      numLastMatchedN = numNextMatchedN;              
	    } 
	}
      else
	{
	  if (currentPackage)
	    {
	      StScmPackage *newPackage = new StScmPackage(currentListPackage->getSize(), currentPackage->getSize());
	      newPackage->takeMatcheds(currentPackage);
	      currentListPackage->addNewPackage(newPackage);
	      currentPackage->purgePackage();
	      numPackage++;
	    }
	}
      currentClusterP = mClusterP->next(currentClusterP);
    }
  delete currentPackage;
  return numPackage;
}

int StScmWafer::doSolvePerfect(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl)
{
  int nPerfect = 0;
  StScmPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      int numMatched = strlen(currentKind)/2;
      float *Adc     = new float[numMatched];
      for(int i=0;i<numMatched;i++) 
	Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 *********************************************************************
      if(!strcmp(currentKind,"1p1n"))//                   case (1-1) checked
	{
          StScmPoint *newPoint = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 11);
          newPoint->setFlag(100);
          setMatcheds(geom_par, newPoint, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPoint->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPoint);
          nPerfect++;
	}
//  *********************************************************************
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nPerfect;
}

void StScmWafer::doStatPerfect(int nPerfectPoint, scm_ctrl_st *scm_ctrl)
{
  float store = 0;
  StScmPoint *currentPerfect = 0;
  currentPerfect = mPoint->first();
  while(currentPerfect)
    {
      store += currentPerfect->getDe(1);
      currentPerfect = mPoint->next(currentPerfect);
    }
  mPerfectMean = store/nPerfectPoint;

        store = 0;
  currentPerfect = mPoint->first();
  while(currentPerfect)
    {
      store += (currentPerfect->getDe(1)-mPerfectMean)*(currentPerfect->getDe(1)-mPerfectMean);
      currentPerfect=mPoint->next(currentPerfect);
    }
  mPerfectSigma = store/nPerfectPoint;
}

int StScmWafer::doSolvePackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl)
{
  int nSolved = 0;
  StScmPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      int numMatched = strlen(currentKind)/2;
      float *Adc     = new float[numMatched];
      for(int i=0;i<numMatched;i++) Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 ********************************************************************
      if(!strcmp(currentKind,"1p1n"))//  case (1-1) done in doSolvePerfect 
	{
	}
// 2 ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n"))// case (1-2)A final check Ok
 	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 12);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  12);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
 	}
// 3 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n"))// case (1-2)AS final check Ok
  	{
 	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
//  ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n3n"))// case (1-3)A
 	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 13);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2]-Adc[3], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1]-Adc[3], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0]-Adc[1]-Adc[2], Adc[3]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);
	  nSolved++;
 	}
//  *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n"))// case (1-3)AS
  	{
 	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]-Adc[3]);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]-Adc[3]);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]-Adc[0]-Adc[2]);
          newPointC->setFlag(100);
 	  mPoint->addNewPoint(newPointC);
          nSolved++;
  	}
// 4 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n"))//        case (2-2)A checked
  	{
 	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[3], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
  	}
// 5 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n"))//        case (2-2)AP checked
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointB->setEnergyLoss(Adc[2], Adc[4]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
// 6 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n"))//        case (2-2)B checked
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
 	  double setA[2], setB[2], setC[2], setD[2];
          double probAD, probBC;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
	  if ((setA[1]*setD[1])||(setB[1]*setC[1]))
	    {
	      probAD = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	      probBC = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	    }
	  else
	    {
	      probAD = 0.5;
	      probBC = 0.5;
	    }
	  newPointA->setFlag(int(100*probAD));
	  newPointB->setFlag(int(100*probBC));
	  newPointC->setFlag(int(100*probBC));
	  newPointD->setFlag(int(100*probAD));
          nSolved++;

  	}
// 7 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n"))//        case (2-3)A checked
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointC->setEnergyLoss(Adc[3]-Adc[5], Adc[2]);
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2];
	  double setAB[2], setCD[2];
          double probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setAB[0] = (Adc[0] - (Adc[1]+Adc[2]))/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setCD[0] = (Adc[3] - (Adc[2]+Adc[5]))/sqrt(2);
          setCD[1] = matchDistr(scm_ctrl, setCD[0]);
          setD[0]  = (Adc[3] - Adc[5])/sqrt(2);
          setD[1]  = matchDistr(scm_ctrl, setD[0]);
	  probABD  = (setAB[1]*setD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  probACD  = (setA[1]*setCD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointD->setEnergyLoss(Adc[3], Adc[5]);
	    }
	  else
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]);
	      newPointD->setEnergyLoss(Adc[3]-Adc[2], Adc[5]);
	    }
          nSolved++;
  	}
// 8 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n"))//        case (2-3)AP checked
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
   	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
   	  newPointC->setEnergyLoss(Adc[2], Adc[4]-Adc[5]);
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(4));
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2];
	  double setAB[2], setCD[2];
          double probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setAB[0] = (Adc[0] + Adc[2] - Adc[1])/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setCD[0] = (Adc[2] + Adc[5] - Adc[4])/sqrt(2);
          setCD[1] = matchDistr(scm_ctrl, setCD[0]);
          setD[0]  = (Adc[5] - Adc[4])/sqrt(2);
          setD[1]  = matchDistr(scm_ctrl, setD[0]);
	  probABD  = (setAB[1]*setD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  probACD  = (setA[1]*setCD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
	      newPointD->setEnergyLoss(Adc[5], Adc[4]);
	    }
	  else
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]);
	      newPointD->setEnergyLoss(Adc[5], Adc[4]-Adc[2]);
	    }
          nSolved++;
  	}
// 9 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n"))//        case (2-3)B
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
  	  newPointC->setEnergyLoss(Adc[0]-Adc[1], Adc[3]);
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2], setE[2];
	  double setAB[2], setAC[2], setDE[2];
          double probABE, probACD, probADE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setAB[0] = (Adc[0] - (Adc[1] + Adc[2]))/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setAC[0] = (Adc[0] - (Adc[1] + Adc[3]))/sqrt(2);
          setAC[1] = matchDistr(scm_ctrl, setAC[0]);
          setDE[0] = (Adc[4] - (Adc[2] + Adc[3]))/sqrt(2);
          setDE[1] = matchDistr(scm_ctrl, setDE[0]);
          setD[0]  = (Adc[4] - Adc[2])/sqrt(2);
          setD[1]  = matchDistr(scm_ctrl, setD[0]);
          setE[0]  = (Adc[4] - Adc[3])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  probACD  = (setAC[1]*setD[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  probADE  = (setA[1]*setDE[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*probACD));
	  newPointD->setFlag(int(100*(probACD+probADE)));
	  newPointE->setFlag(int(100*(probABE+probADE)));
	  if ((probABE > probACD)&&(probABE > probADE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointE->setEnergyLoss(Adc[4],Adc[3]);
	    }
	  else if ((probACD > probABE)&&(probACD > probADE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[3],Adc[1]);
	      newPointD->setEnergyLoss(Adc[4],Adc[2]);
	    }
	  else if ((probADE > probABE)&&(probADE > probACD))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointD->setEnergyLoss(Adc[4]-Adc[3],Adc[2]);
	      newPointE->setEnergyLoss(Adc[4]-Adc[2],Adc[3]);
	    }
          nSolved++;
  	}
// 10 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n"))//        case (3-2)BP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
  	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(1));
  	  newPointD->setEnergyLoss(Adc[5], Adc[1]-Adc[0]);
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(4));
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setC[2], setE[2];
          double setAB[2], setAD[2], setCE[2];
          double probABE, probACD, probACE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setAB[0] = (Adc[0] + Adc[2] - Adc[1])/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setAD[0] = (Adc[0] + Adc[5] - Adc[1])/sqrt(2);
          setAD[1] = matchDistr(scm_ctrl, setAD[0]);
          setCE[0] = (Adc[2] + Adc[5] + Adc[4])/sqrt(2);
          setCE[1] = matchDistr(scm_ctrl, setCE[0]);
          setC[0]  = (Adc[2] - Adc[4])/sqrt(2);
          setC[1]  = matchDistr(scm_ctrl, setC[0]);
          setE[0]  = (Adc[5] - Adc[4])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  probACD  = (setAD[1]*setC[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  probACE  = (setA[1]*setCE[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*(probACD+probACE)));
	  newPointD->setFlag(int(100*probACD));
	  newPointE->setFlag(int(100*(probABE+probACE)));
	  if ((probABE > probACD)&&(probABE > probACE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[2]);
	      newPointE->setEnergyLoss(Adc[5],Adc[4]);
	    }
	  else if ((probACD > probABE)&&(probACD > probACE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[5]);
	      newPointC->setEnergyLoss(Adc[2],Adc[4]);
	    }
	  else if ((probACE > probABE)&&(probACE > probACD))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointC->setEnergyLoss(Adc[2],Adc[4]-Adc[5]);
	      newPointE->setEnergyLoss(Adc[5],Adc[4]-Adc[2]);
	    }
          nSolved++;
  	}
// 11 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n"))//        case (2-3)BS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
  	  newPointC->setEnergyLoss(Adc[3]-Adc[6],Adc[1]);
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointD->setEnergyLoss(Adc[3]-Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
          newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setE[2];
          double setAB[2], setCE[2], setDE[2];
          double probABE, probADE, probBCE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2);
          setB[1]  = matchDistr(scm_ctrl, setB[0]);
          setAB[0] = (Adc[0] - (Adc[1] + Adc[2]))/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setCE[0] = (Adc[3] - (Adc[1] - Adc[6]))/sqrt(2);
          setCE[1] = matchDistr(scm_ctrl, setCE[0]);
          setDE[0] = (Adc[3] - (Adc[2] + Adc[6]))/sqrt(2);
          setDE[1] = matchDistr(scm_ctrl, setDE[0]);
          setE[0]  = (Adc[3] - Adc[6])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  probADE  = (setA[1]*setDE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  probBCE  = (setB[1]*setCE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  newPointA->setFlag(int(100*(probABE+probADE)));
	  newPointB->setFlag(int(100*(probABE+probBCE)));
	  newPointC->setFlag(int(100*probBCE));
	  newPointD->setFlag(int(100*probADE));
	  if ((probABE > probADE)&&(probABE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointB->setEnergyLoss(Adc[0]-Adc[1],Adc[2]);
	      newPointE->setEnergyLoss(Adc[3],Adc[6]);
	    }
	  else if ((probADE > probABE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointE->setEnergyLoss(Adc[3]-Adc[2],Adc[6]);
	    }
	  else if ((probBCE > probABE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLoss(Adc[0],Adc[2]);
	      newPointE->setEnergyLoss(Adc[3]-Adc[1],Adc[6]);
	    }
          nSolved++;
  	}
// 12 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n"))//        case (3-2)BSP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[6], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3]-Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
          newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setC[2], setE[2];
          double setAC[2], setBE[2], setDE[2];
          double probACE, probADE, probBCE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setAC[0] = (Adc[0] + Adc[3] - Adc[1])/sqrt(2);
          setAC[1] = matchDistr(scm_ctrl, setAC[0]);
          setBE[0] = (Adc[0] + Adc[6] - Adc[2])/sqrt(2);
          setBE[1] = matchDistr(scm_ctrl, setBE[0]);
          setC[0]  = (Adc[3] - Adc[1])/sqrt(2);
          setC[1]  = matchDistr(scm_ctrl, setC[0]);
          setDE[0] = (Adc[3] + Adc[6] - Adc[2])/sqrt(2);
          setDE[1] = matchDistr(scm_ctrl, setDE[0]);
          setE[0]  = (Adc[6] - Adc[2])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
	  probACE  = (setAC[1]*setE[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  probADE  = (setA[1]*setDE[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  probBCE  = (setBE[1]*setC[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  newPointA->setFlag(int(100*(probACE+probADE)));
	  newPointB->setFlag(int(100*probBCE));
	  newPointC->setFlag(int(100*(probACE+probBCE)));
	  newPointD->setFlag(int(100*probADE));
	  if ((probACE > probADE)&&(probACE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[3]);
	      newPointC->setEnergyLoss(Adc[3],Adc[1]-Adc[0]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]);
	    }
	  else if ((probADE > probACE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]-Adc[3]);
	    }
	  else if ((probBCE > probACE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLoss(Adc[3],Adc[1]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]-Adc[0]);
	    }
          nSolved++;
  	}
// 13 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n"))//        case (2-3)C
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2]-Adc[5], Adc[4]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2]-Adc[4], Adc[5]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 14 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n"))//       case (3-2)CP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]-Adc[5]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[5], Adc[2]-Adc[3]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 15 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p3n"))//        case (2-3)CS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[4], Adc[3]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 16 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n2n"))//      case (3-2)CPS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(6));
 	  newPointD->setEnergyLoss(Adc[4], Adc[6]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 17 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n"))//        case (2-3)D
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointE);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          double setAB[2], setAC[2], setBC[2], setDE[2], setDF[2], setEF[2];
          double prob[6];

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2);
          setB[1]  = matchDistr(scm_ctrl, setB[0]);
          setC[0]  = (Adc[0] - Adc[3])/sqrt(2);
          setC[1]  = matchDistr(scm_ctrl, setC[0]);
          setD[0]  = (Adc[4] - Adc[1])/sqrt(2);
          setD[1]  = matchDistr(scm_ctrl, setD[0]);
          setE[0]  = (Adc[4] - Adc[2])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
          setF[0]  = (Adc[4] - Adc[3])/sqrt(2);
          setF[1]  = matchDistr(scm_ctrl, setF[0]);
          setAB[0] = (Adc[0] - Adc[1] - Adc[2])/sqrt(2);
          setAB[1] = matchDistr(scm_ctrl, setAB[0]);
          setAC[0] = (Adc[0] - Adc[1] - Adc[3])/sqrt(2);
          setAC[1] = matchDistr(scm_ctrl, setAC[0]);
          setBC[0] = (Adc[0] - Adc[2] - Adc[3])/sqrt(2);
          setBC[1] = matchDistr(scm_ctrl, setBC[0]);
          setDE[0] = (Adc[4] - Adc[1] - Adc[2])/sqrt(2);
          setDE[1] = matchDistr(scm_ctrl, setDE[0]);
          setDF[0] = (Adc[4] - Adc[1] - Adc[3])/sqrt(2);
          setDF[1] = matchDistr(scm_ctrl, setDF[0]);
          setEF[0] = (Adc[4] - Adc[2] - Adc[3])/sqrt(2);
          setEF[1] = matchDistr(scm_ctrl, setEF[0]);
	  prob[0]  = (setAC[1]*setE[1]+setAB[1]*setF[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[1]  = (setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[2]  = (setAC[1]*setE[1]+setBC[1]*setD[1]+setDE[1]*setC[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[3]  = (setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[4]  = (setAC[1]*setE[1]+setDE[1]*setC[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[5]  = (setAB[1]*setF[1]+setDF[1]*setB[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
          nSolved++;
  	}
// 18 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n"))//        case (3-2)DP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointE);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          double setAC[2], setBD[2], setAE[2], setBF[2], setCE[2], setDF[2];
          double prob[6];

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2);
          setA[1]  = matchDistr(scm_ctrl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2);
          setB[1]  = matchDistr(scm_ctrl, setB[0]);
          setC[0]  = (Adc[3] - Adc[1])/sqrt(2);
          setC[1]  = matchDistr(scm_ctrl, setC[0]);
          setD[0]  = (Adc[3] - Adc[2])/sqrt(2);
          setD[1]  = matchDistr(scm_ctrl, setD[0]);
          setE[0]  = (Adc[6] - Adc[1])/sqrt(2);
          setE[1]  = matchDistr(scm_ctrl, setE[0]);
          setF[0]  = (Adc[6] - Adc[2])/sqrt(2);
          setF[1]  = matchDistr(scm_ctrl, setF[0]);
          setAC[0] = (Adc[0] + Adc[3] - Adc[1])/sqrt(2);
          setAC[1] = matchDistr(scm_ctrl, setAC[0]);
          setBD[0] = (Adc[0] + Adc[3] - Adc[2])/sqrt(2);
          setBD[1] = matchDistr(scm_ctrl, setBD[0]);
          setAE[0] = (Adc[0] + Adc[6] - Adc[1])/sqrt(2);
          setAE[1] = matchDistr(scm_ctrl, setAE[0]);
          setBF[0] = (Adc[0] + Adc[6] - Adc[2])/sqrt(2);
          setBF[1] = matchDistr(scm_ctrl, setBF[0]);
          setCE[0] = (Adc[3] + Adc[6] - Adc[1])/sqrt(2);
          setCE[1] = matchDistr(scm_ctrl, setCE[0]);
          setDF[0] = (Adc[3] + Adc[6] - Adc[2])/sqrt(2);
          setDF[1] = matchDistr(scm_ctrl, setDF[0]);
	  prob[0]  = (setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[1]  = (setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[2]  = (setAC[1]*setF[1]+setCE[1]*setB[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[3]  = (setAE[1]*setD[1]+setCE[1]*setB[1]+setBD[1]*setE[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[4]  = (setAE[1]*setD[1]+setDF[1]*setA[1]+setBD[1]*setE[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[5]  = (setAC[1]*setF[1]+setDF[1]*setA[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
          nSolved++;
  	}
// 19 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p3n"))//        case (3-3)A
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(3));
 	  newPointE->setEnergyLoss(Adc[6], Adc[3]);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 20 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n3n"))//        case (3-3)AP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLoss(Adc[5], Adc[7]);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 21 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p2n3n"))//    case (3-3)B
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[3], Adc[5]);
	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointE);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLoss(Adc[6], Adc[5]);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setE[2], setF[2];
          double probACF, probADE;

          setC[0] = (Adc[3] - Adc[2])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[5])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[6] - Adc[2])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
	  probACF = (setC[1]*setF[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  probADE = (setD[1]*setE[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));
          nSolved++;
  	}
// 22 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p2n3n"))//    case (3-3)BP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2], Adc[5]);
	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointE->setEnergyLoss(Adc[6], Adc[4]);
 	  mPoint->addNewPoint(newPointE);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLoss(Adc[6], Adc[5]);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setE[2], setF[2];
          double probACF, probADE;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[6] - Adc[4])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
	  probACF = (setC[1]*setF[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  probADE = (setD[1]*setE[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));
          nSolved++;
  	}
// 23 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n3n"))//    case (3-3)BS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(8));
 	  newPointF->setEnergyLoss(Adc[6], Adc[8]);
 	  newPointF->setFlag(100);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
	  probADF = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCF = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
          nSolved++;
  	}
// 24 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p3n"))//    case (3-3)BSP
 	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

 	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointF->setEnergyLoss(Adc[7], Adc[6]);
          newPointF->setNMatched(33);
 	  newPointF->setFlag(100);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
	  probADF = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCF = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
          nSolved++;
  	}
// 25 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p3n"))//    case (3-3)C
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[4], Adc[2]);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[7], Adc[3]);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 26 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n3n"))//    case (3-3)CS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointD->setEnergyLoss(Adc[2], Adc[4]);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(5), currentPackage->getMatched(8));
 	  newPointF->setEnergyLoss(Adc[5], Adc[8]);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 27 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p2n3n"))//   case (3-3)D
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointE->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointG->setEnergyLoss(Adc[7], Adc[3]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setD[2], setE[2], setF[2], setG[2];
          double probADG, probAEF;

          setD[0] = (Adc[4] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[4] - Adc[3])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[7] - Adc[2])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[7] - Adc[3])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
	  probADG = (setD[1]*setG[1])/(setD[1]*setG[1]+setE[1]*setF[1]);
	  probAEF = (setE[1]*setF[1])/(setD[1]*setG[1]+setE[1]*setF[1]);
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*probADG));

          nSolved++;
  	}
// 28 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p1n2n3n"))// case (3-3)DP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2], Adc[5]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointF->setEnergyLoss(Adc[6], Adc[4]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointG->setEnergyLoss(Adc[6], Adc[5]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setF[2], setG[2];
          double probACG, probADF;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setF[0] = (Adc[6] - Adc[4])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[6] - Adc[5])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
	  probACG = (setC[1]*setG[1])/(setC[1]*setG[1]+setD[1]*setF[1]);
	  probADF = (setD[1]*setF[1])/(setC[1]*setG[1]+setD[1]*setF[1]);
	  newPointC->setFlag(int(100*probACG));
	  newPointD->setFlag(int(100*probADF));
	  newPointF->setFlag(int(100*probADF));
	  newPointG->setFlag(int(100*probACG));

          nSolved++;
  	}
// 29 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n3n"))// case (3-3)DS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(9));
 	  newPointG->setEnergyLoss(Adc[6], Adc[9]);
          newPointG->setFlag(100);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADG, probBCG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
	  probADG = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCG = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADG));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));

          nSolved++;
  	}
// 30 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p3n"))//case (3-3)DSP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointG->setEnergyLoss(Adc[8], Adc[3]);
          newPointG->setFlag(100);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setD[2], setE[2];
          double probAEG, probBDG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setD[0] = (Adc[4] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[4] - Adc[1])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
	  probAEG = (setA[1]*setE[1])/(setA[1]*setE[1]+setB[1]*setD[1]);
	  probBDG = (setB[1]*setD[1])/(setA[1]*setE[1]+setB[1]*setD[1]);
	  newPointA->setFlag(int(100*probAEG));
	  newPointB->setFlag(int(100*probBDG));
	  newPointD->setFlag(int(100*probBDG));
	  newPointE->setFlag(int(100*probAEG));

          nSolved++;
  	}
// 31 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p3n"))//   case (3-3)E
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointE->setEnergyLoss(Adc[6], Adc[5]);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 32 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n3n"))//   case (3-3)EP
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLoss(Adc[5], Adc[7]);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 33 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p2n3n"))//  case (3-3)F
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLoss(Adc[3], Adc[6]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointG->setEnergyLoss(Adc[7], Adc[6]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2];
          double probADG, probAEF, probBCG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[3] - Adc[6])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[7] - Adc[2])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[7] - Adc[6])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
	  probADG = (setA[1]*setD[1]*setG[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  probAEF = (setA[1]*setE[1]*setF[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  probBCG = (setB[1]*setC[1]*setG[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  newPointA->setFlag(int(100*(probADG+probAEF)));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*(probADG+probBCG)));

          nSolved++;
  	}
// 34 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p2n3n"))//  case (3-3)G
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0], Adc[3]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointG->setEnergyLoss(Adc[8], Adc[2]);
	  mPoint->addNewPoint(newPointG);

	  StScmPoint *newPointH = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointH->setEnergyLoss(Adc[8], Adc[3]);
	  mPoint->addNewPoint(newPointH);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          double probAEH, probAFG, probBDH, probCDG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[0] - Adc[3])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[4] - Adc[1])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[4] - Adc[2])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[4] - Adc[3])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[8] - Adc[2])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
          setH[0] = (Adc[8] - Adc[3])/sqrt(2);
          setH[1] = matchDistr(scm_ctrl, setH[0]);
	  probAEH = (setA[1]*setE[1]*setH[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probAFG = (setA[1]*setF[1]*setG[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probBDH = (setB[1]*setD[1]*setH[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probCDG = (setC[1]*setD[1]*setG[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  newPointA->setFlag(int(100*(probAEH+probAFG)));
	  newPointB->setFlag(int(100*probBDH));
	  newPointC->setFlag(int(100*probCDG));
	  newPointD->setFlag(int(100*(probBDH+probCDG)));
	  newPointE->setFlag(int(100*probAEH));
	  newPointF->setFlag(int(100*probAFG));
	  newPointG->setFlag(int(100*(probAFG+probCDG)));
	  newPointH->setFlag(int(100*(probAEH+probBDH)));

          nSolved++;
  	}
// 35 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p1n2n3n"))//  case (3-3)GS
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0],  Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLoss(Adc[3], Adc[6]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(1));
 	  newPointF->setEnergyLoss(Adc[7], Adc[1]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointG->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointG);

	  StScmPoint *newPointH = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(geom_par, newPointH, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointH->setEnergyLoss(Adc[7], Adc[6]);
	  mPoint->addNewPoint(newPointH);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          double probADH, probAEG, probBCH, probBEF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[3] - Adc[6])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[7] - Adc[1])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[7] - Adc[2])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
          setH[0] = (Adc[7] - Adc[6])/sqrt(2);
          setH[1] = matchDistr(scm_ctrl, setH[0]);
	  probADH = (setA[1]*setD[1]*setH[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probAEG = (setA[1]*setE[1]*setG[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probBCH = (setB[1]*setC[1]*setH[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probBEF = (setB[1]*setE[1]*setF[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  newPointA->setFlag(int(100*(probADH+probAEG)));
	  newPointB->setFlag(int(100*(probBCH+probBEF)));
	  newPointC->setFlag(int(100*probBCH));
	  newPointD->setFlag(int(100*probADH));
	  newPointE->setFlag(int(100*(probAEG+probBEF)));
	  newPointF->setFlag(int(100*probBEF));
	  newPointG->setFlag(int(100*probAEG));
	  newPointH->setFlag(int(100*(probADH+probBCH)));

          nSolved++;
  	}
// 36 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p1n2n3n"))// case (3-3)H
  	{
	  StScmPoint *newPointA = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StScmPoint *newPointB = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StScmPoint *newPointC = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0], Adc[3]);
	  mPoint->addNewPoint(newPointC);

	  StScmPoint *newPointD = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StScmPoint *newPointE = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StScmPoint *newPointF = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointF);

	  StScmPoint *newPointG = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(1));
 	  newPointG->setEnergyLoss(Adc[8], Adc[1]);
	  mPoint->addNewPoint(newPointG);

	  StScmPoint *newPointH = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointH->setEnergyLoss(Adc[8], Adc[2]);
	  mPoint->addNewPoint(newPointH);

	  StScmPoint *newPointI = new StScmPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(geom_par, newPointI, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointI->setEnergyLoss(Adc[8], Adc[3]);
	  mPoint->addNewPoint(newPointI);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2], setI[2];
          double probAEI, probCEG, probAFH, probBDI, probCDH, probBFG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2);
          setA[1] = matchDistr(scm_ctrl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2);
          setB[1] = matchDistr(scm_ctrl, setB[0]);
          setC[0] = (Adc[0] - Adc[3])/sqrt(2);
          setC[1] = matchDistr(scm_ctrl, setC[0]);
          setD[0] = (Adc[4] - Adc[1])/sqrt(2);
          setD[1] = matchDistr(scm_ctrl, setD[0]);
          setE[0] = (Adc[4] - Adc[2])/sqrt(2);
          setE[1] = matchDistr(scm_ctrl, setE[0]);
          setF[0] = (Adc[4] - Adc[3])/sqrt(2);
          setF[1] = matchDistr(scm_ctrl, setF[0]);
          setG[0] = (Adc[8] - Adc[1])/sqrt(2);
          setG[1] = matchDistr(scm_ctrl, setG[0]);
          setH[0] = (Adc[8] - Adc[2])/sqrt(2);
          setH[1] = matchDistr(scm_ctrl, setH[0]);
          setI[0] = (Adc[8] - Adc[3])/sqrt(2);
          setI[1] = matchDistr(scm_ctrl, setI[0]);
	  probAEI = (setA[1]*setE[1]*setI[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probCEG = (setC[1]*setE[1]*setG[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probAFH = (setA[1]*setF[1]*setH[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probBDI = (setB[1]*setD[1]*setI[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probCDH = (setC[1]*setD[1]*setH[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probBFG = (setB[1]*setF[1]*setG[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  newPointA->setFlag(int(100*(probAEI+probAFH)));
	  newPointB->setFlag(int(100*(probBDI+probBFG)));
	  newPointC->setFlag(int(100*(probCEG+probCDH)));
	  newPointD->setFlag(int(100*(probBDI+probCDH)));
	  newPointE->setFlag(int(100*(probAEI+probCEG)));
	  newPointF->setFlag(int(100*(probAFH+probBFG)));
	  newPointG->setFlag(int(100*(probCEG+probBFG)));
	  newPointH->setFlag(int(100*(probAFH+probCDH)));
	  newPointI->setFlag(int(100*(probAEI+probBDI)));

          nSolved++;
  	}
      else cout<<" Warning unsolved case ("<<currentKind<<")\n";// other cases
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nSolved;
}

int StScmWafer::convertDigitToAnalog(double PairCreationEnergy)
{
  StScmPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      currentPoint->setDe(currentPoint->getDe(0)*PairCreationEnergy,0);
      currentPoint->setDe(currentPoint->getDe(1)*PairCreationEnergy,1);
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}


int StScmWafer::convertUFrameToLocal(sdm_geom_par_st *geom_par)
{
  StScmPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      currentPoint->setXl(currentPoint->getPositionU(0)/2.+currentPoint->getPositionU(1)/2.-geom_par[0].L_wafer_act_l+geom_par[0].L_wafer_act_w*tan(geom_par[0].L_stereo_angle),0);
      currentPoint->setXl((currentPoint->getPositionU(1)-currentPoint->getPositionU(0))/(2*tan(geom_par[0].L_stereo_angle)),1);
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}

int StScmWafer::convertLocalToGlobal()
{
  StScmPoint *currentPoint = mPoint->first();
  float B[3],wD[3],wT[3],wN[3],tempXl[3];
  float delta = 0;
  while(currentPoint)
    {
      B[0]=currentPoint->getXl(0)+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
      B[1]=currentPoint->getXl(1)+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
      B[2]=currentPoint->getXl(2)+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

//   Xl[0]=mDv.Xgv-mDv.Xv        ( mD[0] mD[1] mD[2] ) (Xg[0]) = (B[0])
//   Xl[1]=mTv.Xgv-mTv.Xv  i.e.  ( mT[0] mT[1] mT[2] ) (Xg[1])   (B[1])
//   Xl[2]=mNv.Xgv-mNv.Xv        ( mN[0] mN[1] mN[2] ) (Xg[2])   (B[2])

//   ( wD[0] wD[1] wD[2] ) (B[0]) = (tempXl[0])
//   ( wT[0] wT[1] wT[2] ) (B[1]) = (tempXl[1])
//   ( wN[0] wN[1] wN[2] ) (B[2]) = (tempXl[2])

      delta = mD[0]*mT[1]*mN[2]-mD[0]*mT[2]*mN[1]-mD[1]*mT[0]*mN[2]+mD[2]*mT[0]*mN[1]+mD[1]*mT[2]*mN[0]-mD[2]*mT[1]*mN[0]; // change frame =>delta = -1 !..

      wD[0] = mT[1]*mN[2]-mT[2]*mN[1];
      wD[1] = mD[2]*mN[1]-mD[1]*mN[2];
      wD[2] = mD[1]*mT[2]-mD[2]*mT[1];
      wT[0] = mT[2]*mN[0]-mT[0]*mN[2];
      wT[1] = mD[0]*mN[2]-mD[2]*mN[0];
      wT[2] = mD[2]*mT[0]-mD[0]*mT[2];
      wN[0] = mT[0]*mN[1]-mT[1]*mN[0];
      wN[1] = mD[1]*mN[0]-mD[0]*mN[1];
      wN[2] = mD[0]*mT[1]-mD[1]*mT[0];

      tempXl[0] = wD[0]*B[0]+wD[1]*B[1]+wD[2]*B[2];
      tempXl[1] = wT[0]*B[0]+wT[1]*B[1]+wT[2]*B[2];
      tempXl[2] = wN[0]*B[0]+wN[1]*B[1]+wN[2]*B[2];

      currentPoint->setXg(tempXl[0]/delta,0);
      currentPoint->setXg(tempXl[1]/delta,1);
      currentPoint->setXg(tempXl[2]/delta,2);

      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}


int StScmWafer::printborder()
{
  if (mId==7101){
    printf("Wafer = %d \n",mId);
    float activeXs[4],BXs[4],tempXls[4];
    float activeYs[4],BYs[4],tempYls[4];
    float activeZs[4],BZs[4],tempZls[4];
    float activeXe[4],BXe[4],tempXle[4];
    float activeYe[4],BYe[4],tempYle[4];
    float activeZe[4],BZe[4],tempZle[4];
    float wD[3],wT[3],wN[3];
    float delta = 0;

    activeXs[0]=-3.65,activeXs[1]= 3.65,activeXs[2]= 3.65,activeXs[3]=-3.65;
    activeYs[0]= 2.00,activeYs[1]= 2.00,activeYs[2]=-2.00,activeYs[3]=-2.00;
    activeZs[0]= 0.00,activeZs[1]= 0.00,activeZs[2]= 0.00,activeZs[3]= 0.00;

    activeXe[0]= 3.65,activeXe[1]= 3.65,activeXe[2]=-3.65,activeXe[3]=-3.65;
    activeYe[0]= 2.00,activeYe[1]=-2.00,activeYe[2]=-2.00,activeYe[3]= 2.00;
    activeZe[0]= 0.00,activeZe[1]= 0.00,activeZe[2]= 0.00,activeZe[3]= 0.00;

    wD[0] = mT[1]*mN[2]-mT[2]*mN[1];
    wD[1] = mD[2]*mN[1]-mD[1]*mN[2];
    wD[2] = mD[1]*mT[2]-mD[2]*mT[1];
    wT[0] = mT[2]*mN[0]-mT[0]*mN[2];
    wT[1] = mD[0]*mN[2]-mD[2]*mN[0];
    wT[2] = mD[2]*mT[0]-mD[0]*mT[2];
    wN[0] = mT[0]*mN[1]-mT[1]*mN[0];
    wN[1] = mD[1]*mN[0]-mD[0]*mN[1];
    wN[2] = mD[0]*mT[1]-mD[1]*mT[0];
    for(int i=0;i<5;i++)
      {
	BXs[i]=activeXs[i]+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
	BYs[i]=activeYs[i]+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
	BZs[i]=activeZs[i]+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

	BXe[i]=activeXe[i]+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
	BYe[i]=activeYe[i]+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
	BZe[i]=activeZe[i]+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

	tempXls[i] = wD[0]*BXs[i]+wD[1]*BYs[i]+wD[2]*BZs[i];
	tempYls[i] = wT[0]*BXs[i]+wT[1]*BYs[i]+wT[2]*BZs[i];
	tempZls[i] = wN[0]*BXs[i]+wN[1]*BYs[i]+wN[2]*BZs[i];

	tempXle[i] = wD[0]*BXe[i]+wD[1]*BYe[i]+wD[2]*BZe[i];
	tempYle[i] = wT[0]*BXe[i]+wT[1]*BYe[i]+wT[2]*BZe[i];
	tempZle[i] = wN[0]*BXe[i]+wN[1]*BYe[i]+wN[2]*BZe[i];

      }
    delta = mD[0]*mT[1]*mN[2]-mD[0]*mT[2]*mN[1]-mD[1]*mT[0]*mN[2]+mD[2]*mT[0]*mN[1]+mD[1]*mT[2]*mN[0]-mD[2]*mT[1]*mN[0]; // change frame =>delta = -1 !..



    printf("xsSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempXls[0]/delta,tempXls[1]/delta,tempXls[2]/delta,tempXls[3]/delta);
    printf("xeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempXle[0]/delta,tempXle[1]/delta,tempXle[2]/delta,tempXle[3]/delta);
    printf("ysSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempYls[0]/delta,tempYls[1]/delta,tempYls[2]/delta,tempYls[3]/delta);
    printf("yeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempYle[0]/delta,tempYle[1]/delta,tempYle[2]/delta,tempYle[3]/delta);
    printf("zsSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempZls[0]/delta,tempZls[1]/delta,tempZls[2]/delta,tempZls[3]/delta);
    printf("zeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempZle[0]/delta,tempZle[1]/delta,tempZle[2]/delta,tempZle[3]/delta);

  }
  return 1;
}

StScmWafer::StScmWafer(const StScmWafer & originalWafer)
{
  mId         = originalWafer.mId;
  mD          = new float[3];
  mT          = new float[3];
  mN          = new float[3];
  mX          = new float[3];
//   mDeadStripP = originalWafer.mDeadStripP;
//   mDeadStripN = originalWafer.mDeadStripN;

  mClusterP   = new StScmListCluster();
  mClusterN   = new StScmListCluster();
  mPackage    = new StScmListPackage();
  mPoint      = new StScmListPoint();

  for(int i = 0; i < 3; i++)
    {
      mD[i]   = originalWafer.mD[i]; 
      mT[i]   = originalWafer.mT[i]; 
      mN[i]   = originalWafer.mN[i]; 
      mX[i]   = originalWafer.mX[i];
    } 
}

StScmWafer& StScmWafer::operator=(const StScmWafer originalWafer)
{
  mId         = originalWafer.mId;
//   mDeadStripP = originalWafer.mDeadStripP;
//   mDeadStripN = originalWafer.mDeadStripN;

  for(int i = 0; i < 3; i++)
    {
      mD[i]   = originalWafer.mD[i]; 
      mT[i]   = originalWafer.mT[i]; 
      mN[i]   = originalWafer.mN[i]; 
      mX[i]   = originalWafer.mX[i];
    } 
  return *this;
}


int StScmWafer::geoMatched(sdm_geom_par_st *geom_par, StScmCluster *ptr1, StScmCluster *ptr2)
{
  int geomatched = 0;
  int numStrip = int((2*geom_par[0].L_wafer_act_w*tan(geom_par[0].L_stereo_angle)/geom_par[0].L_strip_pitch)+1);
  if ( (!ptr1) || (!ptr2) )
    geomatched = 0;
  else if((ptr2->getStripMean() > ( ptr1->getStripMean() - numStrip))
     && (ptr2->getStripMean() < (ptr1->getStripMean()  + numStrip)))
    geomatched = 1;
  return geomatched;
}

int StScmWafer::setMatcheds(sdm_geom_par_st *geom_par, StScmPoint *Point, StScmCluster *pMatched, StScmCluster *nMatched)
{// strip(1) -> Upos(0)...
  Point->setPositionU((pMatched->getStripMean()-1)*geom_par[0].L_strip_pitch,0);
  Point->setPositionU((nMatched->getStripMean()-1)*geom_par[0].L_strip_pitch,1);

  // for evaluation only !!!
  int pHitIndex   = 0;
  int nHitIndex   = 0;
  int sptHitIndex = 0;
  for (pHitIndex = 0; pHitIndex < 5; pHitIndex++)
    {
      for (nHitIndex = 0; nHitIndex < 5; nHitIndex++)
	{
	  if ((pMatched->getIdMcHit(pHitIndex))
	      &&(nMatched->getIdMcHit(nHitIndex))
              &&(pMatched->getIdMcHit(pHitIndex) == nMatched->getIdMcHit(nHitIndex))
	      &&(sptHitIndex < 5))
	      Point->setNMchit(pMatched->getIdMcHit(pHitIndex),sptHitIndex++);
	}
    }
  return 1;
}

float StScmWafer::matchDistr(scm_ctrl_st *scm_ctrl, float x)
{
  float mean = scm_ctrl[0].matchMean;
  float sigm = scm_ctrl[0].matchSigma;
  return (1/(sigm*sqrt(2*M_PI)))*exp(-0.5*((x-mean)*(x-mean))/(sigm*sigm));
}
