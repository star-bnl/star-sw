#include "StSsdBarrel.hh"

#include "TFile.h"
#include "StEvent.h"
#include "StSsdHit.h"
#include "StSsdHitCollection.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdStripCalib_Table.h"

#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "StSsdClusterControl.h"
#include "StSsdDynamicControl.h"
#include "StSsdStripList.hh"
#include "StSsdStrip.hh"
#include "StSsdClusterList.hh"
#include "StSsdCluster.hh"
#include "StSsdPointList.hh"
#include "StSsdPoint.hh"

/*!
Constructor using the ssdDimensions_st and ssdConfiguration_st tables from the db
 */
StSsdBarrel::StSsdBarrel(ssdDimensions_st  *dimensions, ssdConfiguration_st *configuration)
{
  mSsdLayer        = 7; // all layers : 1->7
  mNLadder         = configuration[0].nMaxLadders;
  mNWaferPerLadder = dimensions[0].wafersPerLadder;
  mNStripPerSide   = dimensions[0].stripPerSide;

  mLadders = new StSsdLadder*[mNLadder];
  for (int iLad=0; iLad < mNLadder; iLad++)
      mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
}

StSsdBarrel::~StSsdBarrel()
{
  for (int iLad = 0 ; iLad < mNLadder; iLad++)
    delete mLadders[iLad];
}

void StSsdBarrel::debugUnPeu (int monladder, int monwafer)
{
  cout<<"Number of ladders : "<<this->getNumberOfLadders()<<endl;
  for (int i=0;i<this->getNumberOfLadders();i++)
    {
      if (this->mLadders[i]->getLadderNumb()==monladder) 
	{
	  cout<<" Ladder "<<monladder<<" found"<<endl;
	  this->mLadders[i]->debugUnPeu(monwafer);
	}
    }
}

void StSsdBarrel::initLadders(St_ssdWafersPosition *wafpos)
{
  for (int iLad = 0; iLad < mNLadder; iLad++)
    mLadders[iLad]->initWafers(wafpos);
}

// int StSsdBarrel::readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db)
// {
//   int idWaf      = 0;
//   int nStrip     = 0;
//   int nDeadStrip = 0;
//   int iSide      = 0;
//   int isActive   = 0;
//   int sizeP      = 0;
//   int sizeN      = 0; 
//   int i          = 0;
//   for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder; iWaf++)
//     {
//       for ( i = 0 ; i < condition_db_h->nok; i++)
//         {  
//           idWaf  = condition_db[i].id_strip-10000*((int)(condition_db[i].id_strip/10000.)); 
//           nStrip = (int)(condition_db[i].id_strip/100000.);
//           iSide  = (condition_db[i].id_strip-idWaf-nStrip*100000)/10000;
//           isActive = condition_db[i].is_active;
//           if ((!isActive) && (idWaferToWaferNumb(idWaf) == iWaf))
//            { nDeadStrip++;
//              if (iSide == 0)
//                sizeP++;           
//              else 
//                sizeN++;
//            }
//         }
     
//        int* TempDeadStripP = new int[sizeP];
//        int* TempDeadStripN = new int[sizeN]; 
//        int currentDeadStripP = 0;
//        int currentDeadStripN = 0;

//         for (i = 0 ; i < condition_db_h->nok; i++)
//           {
//             idWaf  = condition_db[i].id_strip-10000*((int)(condition_db[i].id_strip/10000.));
//             nStrip = (int)(condition_db[i].id_strip/100000.);
//             iSide  = (condition_db[i].id_strip-idWaf-nStrip*100000)/10000;
//             isActive = condition_db[i].is_active;
// 	    if ((!isActive) && (idWaferToWaferNumb(idWaf) == iWaf) )
// 	      {
// 		if (iSide == 0)
// 		  {
// 		    TempDeadStripP[currentDeadStripP]=nStrip;
// 		    currentDeadStripP++;}
// 	        else
// 		  {
// 		    TempDeadStripN[currentDeadStripN]=nStrip;
// 		    currentDeadStripN++;}
// 	      }
//           }
	
//         mDeadStripP[iWaf] = TempDeadStripP;
//         mDeadStripN[iWaf] = TempDeadStripN;
//         delete TempDeadStripP;
//         delete TempDeadStripN; 
//     }
//   return nDeadStrip;
// }

int StSsdBarrel::readStripFromTable(St_spa_strip *spa_strip)
{
  spa_strip_st *strip = spa_strip->GetTable();
  
  int NumberOfStrip = 0;
  int idWaf         = 0;
  int iWaf          = 0;
  int iLad          = 0;
  int nStrip        = 0;
  int iSide         = 0;
  float sigma         = 0;
  int idMcHit[5]    = {0,0,0,0,0};
  int e = 0;
  for (int i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSsdStrip *newStrip = new StSsdStrip(nStrip, strip[i].adc_count, sigma, idMcHit);
      mLadders[iLad]->mWafers[iWaf]->addStrip(newStrip, iSide);
    }
  NumberOfStrip = spa_strip->GetNRows();  
  return NumberOfStrip;
}
int  StSsdBarrel::writeNoiseToFile(St_spa_strip *spa_strip)
{
  spa_strip_st *strip = spa_strip->GetTable();
  St_ssdStripCalib *stripCal = new St_ssdStripCalib("ssdStripCalib",spa_strip->GetNRows());
  ssdStripCalib_st noise_strip;
  for (int i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      noise_strip.id=strip[i].id_strip;
      noise_strip.pedestals=strip[i].id_mchit[0];
      noise_strip.rms=strip[i].adc_count;
      noise_strip.isActive=1;
      stripCal->AddAt(&noise_strip);
    }
  TFile f1("ssdStripCalib.root","NEW");
  stripCal->Write();
  f1.Close();
  return spa_strip->GetNRows();
}
/*!
  Old method reading noise from the spa_noise table
 */
int  StSsdBarrel::readNoiseFromTable(St_sdm_calib_db *spa_noise, StSsdDynamicControl *dynamicControl)
{
  sdm_calib_db_st *noise = spa_noise->GetTable();
  
  int NumberOfNoise = 0;
  int idWaf  = 0;
  int iWaf   = 0;
  int iLad   = 0;
  int nStrip = 0;
  int iSide  = 0;
  for (int i = 0 ; i < spa_noise->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id_strip/100000.);
      idWaf   = noise[i].id_strip-10000*((int)(noise[i].id_strip/10000.));
      iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;
      mLadders[iLad]->mWafers[iWaf]->setSigmaStrip(nStrip, iSide, noise[i].n_sigma, dynamicControl);
    }

  NumberOfNoise = spa_noise->GetNRows();
  return NumberOfNoise;
//   return noise_h->nok;
}
/*!
New method reading from the ssdStripCalib table
 */
int  StSsdBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib, StSsdDynamicControl *dynamicControl)
{
  ssdStripCalib_st *noise = strip_calib->GetTable();
  
  int NumberOfNoise = 0;
  int idWaf  = 0;
  int iWaf   = 0;
  int iLad   = 0;
  int nStrip = 0;
  int iSide  = 0;
  for (int i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id/100000.);
      idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
      iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
      mLadders[iLad]->mWafers[iWaf]->setSigmaStrip(nStrip, iSide, noise[i].rms, dynamicControl);
      //       if (iLad==11 && iWaf==8 && nStrip <10) 
      //	cout<<"iLad,idWaf,nStrip,iSide,rms = "<<iLad
      //	    <<" "<<idWaf
      //	    <<" "<<nStrip
      //	    <<" "<<iSide
      //	    <<" "<<noise[i].rms<<endl;
    }
  NumberOfNoise = strip_calib->GetNRows();
  return NumberOfNoise;
//   return noise_h->nok;
}

int StSsdBarrel::readClusterFromTable(St_scf_cluster *scf_cluster)
{
  scf_cluster_st *cluster = scf_cluster->GetTable();

  int NumberOfCluster = 0;
  int idWaf           = 0;
  int iWaf            = 0;
  int iLad            = 0;
  int nCluster        = 0;
  int nPCluster       = 0;
  int nNCluster       = 0;
  int iSide           = 0;
  int idMcHit[5]      = {0,0,0,0,0};
  int e               = 0;
  int nStrip          = 0;
  int nFirstStrip     = 0;
  int nFirstAdc       = 0;
  int nLastAdc        = 0;
  int nAdcCount       = 0;
  int nNoiseCount     = 0;
  float nStripMean    = 0;
  int nFlag           = 0;

  for (int i = 0 ; i < scf_cluster->GetNRows(); i++)
    {
      nCluster    = (int)(cluster[i].id_cluster/100000.);
      idWaf       = (cluster[i].id_cluster-10000*((int)(cluster[i].id_cluster/10000.)));
      iSide       = (cluster[i].id_cluster-idWaf-nCluster*100000)/10000;
      iWaf        = (int)((idWaf - mSsdLayer*1000)/100 - 1);
      iLad        = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      nFirstStrip = (int)(cluster[i].first_strip/100000.);
      nStrip      = cluster[i].n_strip;
      nFirstAdc   = cluster[i].first_adc_count;
      nLastAdc    = cluster[i].last_adc_count;
      nAdcCount   = cluster[i].adc_count;
      nNoiseCount = cluster[i].noise_count;
      nStripMean  = cluster[i].strip_mean;
      nFlag       = cluster[i].flag;
      for (e = 0 ; e < 5; e++) idMcHit[e] = cluster[i].id_mchit[e];
      StSsdCluster *newCluster = new StSsdCluster(nCluster, nFirstStrip, nStrip, nAdcCount, nFirstAdc, nLastAdc, nNoiseCount, nStripMean, nFlag, idMcHit);
      if (iSide == 0)
	{nPCluster++;}
      else
        {nNCluster++;}
      mLadders[iLad]->mWafers[iWaf]->addCluster(newCluster, iSide);
    }

  NumberOfCluster = scf_cluster->GetNRows();  
  return NumberOfCluster;
}

int  StSsdBarrel::writeClusterToTable(St_scf_cluster *scf_cluster)
{
  scf_cluster_st cluster;
  int currRecord  = 0;
  int i           = 0;

  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
      {
	int idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSsdClusterList *clusterP = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	StSsdClusterList *clusterN = mLadders[iLad]->mWafers[iWaf]->getClusterN();

	StSsdCluster *pClusterP = clusterP->first();
	while (pClusterP)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterP->getNCluster() + 0)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterP->getFirstStrip()+ 0)+idCurrentWaf;
	    cluster.n_strip         = pClusterP->getClusterSize();
	    cluster.adc_count       = pClusterP->getTotAdc();
	    cluster.first_adc_count = pClusterP->getFirstAdc();
	    cluster.last_adc_count  = pClusterP->getLastAdc();
	    cluster.noise_count     = pClusterP->getTotNoise();
	    cluster.flag            = pClusterP->getFlag();
	    cluster.strip_mean      = pClusterP->getStripMean();
	    for (i = 0 ; i < 5 ; i++)
	      cluster.id_mchit[i] = pClusterP->getIdMcHit(i);
	    scf_cluster->AddAt(&cluster);
	    currRecord++;
	    pClusterP    = clusterP->next(pClusterP);
	  }

	StSsdCluster *pClusterN = clusterN->first();
	while (pClusterN)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterN->getNCluster() + 1)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterN->getFirstStrip() + 1)+idCurrentWaf;
	    cluster.n_strip         = pClusterN->getClusterSize();
	    cluster.adc_count       = pClusterN->getTotAdc();
	    cluster.first_adc_count = pClusterN->getFirstAdc();
	    cluster.last_adc_count  = pClusterN->getLastAdc();
	    cluster.noise_count     = pClusterN->getTotNoise();
	    cluster.flag            = pClusterN->getFlag();
	    cluster.strip_mean      = pClusterN->getStripMean();
	    for (i = 0 ; i < 5 ; i++)
	      cluster.id_mchit[i] = pClusterN->getIdMcHit(i);
	    scf_cluster->AddAt(&cluster);
	    currRecord++;
	    pClusterN    = clusterN->next(pClusterN);
	  }
      }
  return currRecord;
}

int StSsdBarrel::writePointToContainer(St_scm_spt *scm_spt, StSsdHitCollection* ssdHitColl)
{
  scm_spt_st spt;
  StSsdHit *currentSsdHit; 
  // table size is 148 bytes
  int i = 0, inContainer = 0, inTable = 0;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  int hw; float q ; unsigned char c; 

  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	int idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSsdPointList *sptList = mLadders[iLad]->mWafers[iWaf]->getPoint();
	StSsdPoint *pSpt = sptList->first();
     
	while (pSpt){// Start of Point Loop
	  if (ssdHitColl){ // If Available, Fill the StEvent Container
	    //
	    //StSsdHit::StSsdHit(const StThreeVectorF& p,
	    //const StThreeVectorF& e,
	    // unsigned int hw, float q, unsigned char c) : StHit(p, e, hw, q, c)
	    for (i = 0 ; i < 3 ; i++){
	      gPos[i]      =  pSpt->getXg(i);
	      gPosError[i] =  0.0; 
	    }
	    hw = idCurrentWaf;
	    q =  pSpt->getDe(0);
	     
	    currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);

// 	      spt.flag          = pSpt->getFlag();
// 	      spt.id            = 10000*(pSpt->getNPoint())+idCurrentWaf;
// 	      spt.id_cluster    = pSpt->getNCluster();
// 	      spt.id_globtrk    = 0;
// 	      spt.id_match      = pSpt->getNMatched();
// 	      for (i = 0 ; i < 5 ; i++)
// 		{	  
// 		  spt.id_mchit[i]   = pSpt->getNMchit(i);
	    currentSsdHit->setIdTruth(pSpt->getNMchit(0));// need to check first = most probable!
// 		  spt.id_mctrack[i] = 0;
// 		  spt.id_track[i]   = 0;
// 		}	  
// 	      spt.id_wafer      = idCurrentWaf;
// 	      for (i = 0 ; i < 3 ; i++)
// 		{	  
// 		  spt.cov[i]        = 0;
// 		  spt.res[i]        = 0;
// 		  spt.x[i]          = pSpt->getXg(i);
// 		  spt.xl[i]         = pSpt->getXl(i);
// 		}
// 	      for (i = 0 ; i < 2 ; i++)
// 		{
// 		  spt.mom2[i]       = 0;
// 		  spt.de[i]         = pSpt->getDe(i);
// 		}
	    currentSsdHit->setHardwarePosition(8+16*idWaferToWaferNumb(idCurrentWaf));
	    inContainer += ssdHitColl->addHit(currentSsdHit);
	  }// Container condition

	  if (1) {//Jerome is Happy, Fill the Table
	    spt.flag          = pSpt->getFlag();
	    spt.id            = 10000*(pSpt->getNPoint())+idCurrentWaf;
	    spt.id_cluster    = pSpt->getNCluster();
	    spt.id_globtrk    = 0;
	    spt.id_match      = pSpt->getNMatched();
	    for (i = 0 ; i < 5 ; i++)
	      {	  
		spt.id_mchit[i]   = pSpt->getNMchit(i);
		spt.id_mctrack[i] = 0;
		spt.id_track[i]   = 0;
	      }	  
	    spt.id_wafer      = idCurrentWaf;
	    for (i = 0 ; i < 3 ; i++)
	      {	  
		spt.cov[i]        = 0;
		spt.res[i]        = 0;
		spt.x[i]          = pSpt->getXg(i);
		spt.xl[i]         = pSpt->getXl(i);
	      }
	    for (i = 0 ; i < 2 ; i++)
	      {
		spt.mom2[i]       = 0;
		spt.de[i]         = pSpt->getDe(i);
	      }
	    scm_spt->AddAt(&spt);
	  }
	  inTable++;
	  pSpt    = sptList->next(pSpt);
	}// End of Point Loop
      }
  return inTable;
}

void StSsdBarrel::doSideClusterisation(int *barrelNumbOfCluster, StSsdClusterControl *clusterControl)
{
  //  int *wafNumbOfCluster = new int[2];
  int wafNumbOfCluster[2];
  wafNumbOfCluster[0] = 0;
  wafNumbOfCluster[1] = 0;

  for (int iLad = 0 ; iLad < mNLadder; iLad++)
    for (int iWaf = 0 ; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->doClusterisation(wafNumbOfCluster, clusterControl);
	barrelNumbOfCluster[0] += wafNumbOfCluster[0];
	barrelNumbOfCluster[1] += wafNumbOfCluster[1];
      }
  //  delete[] wafNumbOfCluster;
}


int StSsdBarrel::doClusterMatching(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl)
{
  int NumberOfPackage = 0;
  int nSolved = 0;
  int nPerfect = 0;
  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      { 
	NumberOfPackage += mLadders[iLad]->mWafers[iWaf]->doFindPackage(dimensions, clusterControl);
	nPerfect  = mLadders[iLad]->mWafers[iWaf]->doSolvePerfect(dimensions, clusterControl);
	nSolved  += nPerfect;
	            mLadders[iLad]->mWafers[iWaf]->doStatPerfect(nPerfect, clusterControl);
	nSolved  += mLadders[iLad]->mWafers[iWaf]->doSolvePackage(dimensions, clusterControl);
      }
  cout<<"****       Remark: "<<nSolved<<"  solved packages     ****\n";
  return NumberOfPackage;
}

void StSsdBarrel::convertDigitToAnalog(StSsdDynamicControl *dynamicControl)
{
  long   NElectronInAMip    = dynamicControl->getNElectronInAMip();
  long   ADCDynamic         = dynamicControl->getADCDynamic();
  double PairCreationEnergy = dynamicControl->getPairCreationEnergy();

  const int NAdcChannel     = (int)pow(2.0,dynamicControl->getNBitEncoding());
  const double convFactor   = (PairCreationEnergy*ADCDynamic*NElectronInAMip)/NAdcChannel;
  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      mLadders[iLad]->mWafers[iWaf]->convertDigitToAnalog(convFactor);
}

void StSsdBarrel::convertUFrameToOther(ssdDimensions_st *dimensions)
{
  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->convertUFrameToLocal(dimensions);
	mLadders[iLad]->mWafers[iWaf]->convertLocalToGlobal();
      }
}

void StSsdBarrel::sortListStrip()
{
  StSsdStripList *currentList = 0;
  int isSorted = 0;
  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	currentList = mLadders[iLad]->mWafers[iWaf]->getStripP();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortStrip();

	currentList = mLadders[iLad]->mWafers[iWaf]->getStripN();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortStrip();
      }
  return;
}

void StSsdBarrel::sortListCluster()
{
  StSsdClusterList *currentList = 0;
  int isSorted = 0;
  for (int iLad = 0; iLad < mNLadder; iLad++)
    for (int iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	currentList = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortCluster();

	currentList = mLadders[iLad]->mWafers[iWaf]->getClusterN();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortCluster();
      }
}

StSsdBarrel::StSsdBarrel(const StSsdBarrel & originalBarrel)
{
  mSsdLayer             = originalBarrel.mSsdLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;
  
  mLadders = new StSsdLadder*[mNLadder];
  for (int iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
}

StSsdBarrel& StSsdBarrel::operator=(const StSsdBarrel  originalBarrel)
{
  mSsdLayer             = originalBarrel.mSsdLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;

  mLadders = new StSsdLadder*[mNLadder];
  for (int iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
  return *this;
}

int StSsdBarrel::idWaferToWaferNumb(int idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

int StSsdBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;
}
