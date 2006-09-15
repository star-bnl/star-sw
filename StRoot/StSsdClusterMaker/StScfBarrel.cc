// $Id: StScfBarrel.cc,v 1.6 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfBarrel.cc,v $
// Revision 1.6  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.5  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.4  2005/06/14 12:20:24  bouchet
// cleaner version
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:32  lmartin
// CVS tags added
//
#include "StScfBarrel.hh"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdStripCalib_Table.h"

StScfBarrel::StScfBarrel(ssdDimensions_st  *geom_par)
{
  this->setSsdParameters(geom_par);

  int nWafer  = mNLadder*mNWaferPerLadder;
  int idWaf   = 0;

  mWafers = new StScfWafer*[nWafer];

  for (int iWaf=0; iWaf < nWafer; iWaf++)
    {
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StScfWafer(idWaf);
    }
}

StScfBarrel::~StScfBarrel()
{
  for (int iWaf = 0 ; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      delete mWafers[iWaf];
    }
}


void StScfBarrel::setSsdParameters(ssdDimensions_st *geom_par)
{
  mSsdLayer        = 7;//7; // all layers : 1->7
  mNLadder         = 20; //20;
  mNWaferPerLadder = geom_par[0].wafersPerLadder;
  mNStripPerSide   = geom_par[0].stripPerSide;//;
}

int StScfBarrel::idWaferToWaferNumb(int idWafer)
{
  // idwafer = layer*1000+waf*100+ladder

  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

int StScfBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;
}

int StScfBarrel::readStripFromTable(St_spa_strip *spa_strip)
{
  spa_strip_st *strip = spa_strip->GetTable();
  
  int NumberOfStrip = 0;
  int idWaf = 0;
  int nStrip = 0;
  int iSide = 0;
  //int sigma = 0;
  float sigma = 0;
//   int *idMcHit = new int[5];
  int idMcHit[5]      = {0,0,0,0,0};
  int e = 0;
  for (int i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StScfStrip *newStrip = new StScfStrip(nStrip, strip[i].adc_count, sigma, idMcHit);
      //if(idWaf==7201)printf("id=%d  stripId=%d  side=%d  waferId=%d  adc_count=%d  sigma=%f\n",strip[i].id_strip,nStrip,iSide,idWaf,strip[i].adc_count,sigma);
      //if(idWaf==7601)printf("id=%d  stripId=%d  side=%d  waferId=%d  adc_count=%d  sigma=%f\n",strip[i].id_strip,nStrip,iSide,idWaf,strip[i].adc_count,sigma);
      mWafers[idWaferToWaferNumb(idWaf)]->addStrip(newStrip, iSide);
    }
  cout << "Fired strips = "<<spa_strip->GetNRows()<< endl;
  NumberOfStrip = spa_strip->GetNRows();  
  //  delete [] idMcHit;
  return NumberOfStrip;
}

void StScfBarrel::sortListStrip()
{
  StScfListStrip *currentList = 0;
  int iWafer = 0;
  int isSorted = 0;
  for (iWafer = 0 ; iWafer < mNWaferPerLadder*mNLadder; iWafer++)
    {
      currentList = mWafers[iWafer]->getStripP();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortStrip();

      currentList = mWafers[iWafer]->getStripN();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortStrip();
    }
  return;
}

void StScfBarrel::sortListCluster()
{
  StScfListCluster *currentList = 0;
  int iWafer = 0;
  int isSorted = 0;
  for (iWafer = 0 ; iWafer < mNWaferPerLadder*mNLadder; iWafer++)
    {
      currentList = mWafers[iWafer]->getClusterP();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortCluster();

      currentList = mWafers[iWafer]->getClusterN();
      isSorted = currentList->isSorted();
      if (!isSorted) currentList->sortCluster();
    }
  return;
}


int  StScfBarrel::readNoiseFromTable(St_sdm_calib_db *spa_noise, slsCtrl_st *slsCtrl)
{
  sdm_calib_db_st *noise = spa_noise->GetTable();
  
  int NumberOfNoise = 0;
  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  for (int i = 0 ; i < spa_noise->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id_strip/100000.);
      idWaf   = noise[i].id_strip-10000*((int)(noise[i].id_strip/10000.));
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;

      mWafers[idWaferToWaferNumb(idWaf)]->setSigmaStrip(nStrip, iSide, noise[i].n_sigma, slsCtrl);
    }

  NumberOfNoise = spa_noise->GetNRows();
  return NumberOfNoise;
//   return noise_h->nok;
}

int  StScfBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib, slsCtrl_st *slsCtrl)
{
  //read and uses only ssdStripCalib for the noise, so the conversion adc-->electrons is only for the noise
  ssdStripCalib_st *noise = strip_calib->GetTable();
  int NAdcChannel          = (int)pow(2.0,10.0*1.0);
  int NElectronInAMip      = 22500;
  int ADCDynamic           = 20;
  //  const float   conversionFactor = (float)(NAdcChannel)/(ADCDynamic*NElectronInAMip);
  const float   AdctoE =  (ADCDynamic*NElectronInAMip)/(float)(NAdcChannel);
    
  int idWaf      = 0;
  int iLad       = 0;
  int nStrip     = 0;
  int iSide      = 0;
  int i          = 0;
  int ent        = 0;
  float my_noise = 0;
  for (i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	iLad    = (int)(idWaf - 7*1000 - (idWaferToWaferNumb(idWaf)+1)*100 - 1);
	my_noise = (noise[i].rms*AdctoE)/16.;
	//if(idWaf==7101)printf("id=%d stripId=%d  side=%d  waferId=%d  pedestal=%d  noise=%f\n",noise[i].id,nStrip,iSide,idWaf,noise[i].pedestals,my_noise);
	mWafers[idWaferToWaferNumb(idWaf)]->setSigmaStrip(nStrip, iSide, my_noise,slsCtrl);
	ent++;
      }
    }
  //printf("Entries = %d\n",ent);
  return ent;
  //   return noise_h->nok;
}

void StScfBarrel::doSideClusterisation(int *barrelNumbOfCluster,St_slsCtrl *my_slsCtrl,St_scf_ctrl *my_scf_ctrl)
{
  int *wafNumbOfCluster = new int[2];
  int iWafer = 0;
  wafNumbOfCluster[0] = 0;
  wafNumbOfCluster[1] = 0;
  for (iWafer = 0 ; iWafer < mNWaferPerLadder*mNLadder; iWafer++)
    {
      mWafers[iWafer]->doClusterisation(wafNumbOfCluster, my_slsCtrl, my_scf_ctrl);
      barrelNumbOfCluster[0] += wafNumbOfCluster[0];
      barrelNumbOfCluster[1] += wafNumbOfCluster[1]; 
    }
  delete[] wafNumbOfCluster;
}


int  StScfBarrel::writeClusterToTable(St_scf_cluster *scf_cluster)
{
  scf_cluster_st cluster;
  int currRecord  = 0;
  int i           = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);
      StScfListCluster *clusterP = mWafers[iWaf]->getClusterP();
      StScfListCluster *clusterN = mWafers[iWaf]->getClusterN();
      StScfCluster *pClusterP = clusterP->first();

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
	    {
	      cluster.id_mchit[i] = pClusterP->getIdMcHit(i);
	    }
	  scf_cluster->AddAt(&cluster);
	  currRecord++;
	  pClusterP    = clusterP->next(pClusterP);
	}

      StScfCluster *pClusterN = clusterN->first();
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
	    {
	      cluster.id_mchit[i] = pClusterN->getIdMcHit(i);
	    }
	  scf_cluster->AddAt(&cluster);
	  currRecord++;
	  pClusterN    = clusterN->next(pClusterN);
	}

    }
  return currRecord;
}
int  StScfBarrel::writeClusterToTable(St_scf_cluster *scf_cluster,St_spa_strip *spa_strip)
{
  scf_cluster_st cluster;
  spa_strip_st *on_strip = spa_strip->GetTable(); 
  int currRecord  = 0;
  int i           = 0;
  for (int iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);
      StScfListCluster *clusterP = mWafers[iWaf]->getClusterP();
      StScfListCluster *clusterN = mWafers[iWaf]->getClusterN();
      StScfCluster *pClusterP = clusterP->first();

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
	    {
	      cluster.id_mchit[i] = pClusterP->getIdMcHit(i);
	      if(cluster.id_mchit[i] == 0){
	      cluster.id_mctrack[i]=0;
	      }
	      else{
		for(int j = 0 ; j < spa_strip->GetNRows(); j++){
		  if(cluster.id_mchit[i] == on_strip[j].id_mchit[i]){
		    cluster.id_mctrack[i] = on_strip[j].id_mctrack[i];
		  }
		}
	      }
	    }
	  scf_cluster->AddAt(&cluster);
	  currRecord++;
	  pClusterP    = clusterP->next(pClusterP);
	}

      StScfCluster *pClusterN = clusterN->first();
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
	    {
	      cluster.id_mchit[i] = pClusterN->getIdMcHit(i);
	      if(cluster.id_mchit[i] == 0)cluster.id_mctrack[i]=0;
	      else{
		for(int j = 0 ; j < spa_strip->GetNRows(); j++){
		  if(cluster.id_mchit[i] == on_strip[j].id_mchit[i]){
		    cluster.id_mctrack[i] = on_strip[j].id_mctrack[i];
		  }
		}
	      }
	    }
	  scf_cluster->AddAt(&cluster);
	  currRecord++;
	  pClusterN    = clusterN->next(pClusterN);
	}
    }
  return currRecord;
}
