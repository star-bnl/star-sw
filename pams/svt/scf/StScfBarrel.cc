#include "StScfBarrel.hh"
StScfBarrel::StScfBarrel(sdm_geom_par_st  *geom_par)
{
  this->setScfParameters(geom_par);

  int nWafer  = mNLadder*mNWaferPerLadder;
  int idWaf   = 0;

  mWafers = new StScfWafer*[nWafer];

  int iWaf = 0;
  for (iWaf=0; iWaf < nWafer; iWaf++)
    {
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StScfWafer(idWaf);
    }
}

StScfBarrel::~StScfBarrel()
{
  int i = 0;
  for (i = 0 ; i < mNLadder*mNWaferPerLadder ; i++)
    {
      delete mWafers[i];
    }
}


void StScfBarrel::setScfParameters(sdm_geom_par_st  *geom_par)
{
  mSsdLayer        = geom_par[0].N_layer; // all layers : 1->7
  mNLadder         = geom_par[0].N_ladder;
  mNWaferPerLadder = geom_par[0].N_waf_per_ladder;
  mNStripPerSide   = geom_par[0].N_strip_per_side;//;
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

int StScfBarrel::readStripFromTable(table_head_st *spa_strip_h, spa_strip_st *spa_strip)
{

  int idWaf = 0;
  int nStrip = 0;
  int iSide = 0;
  int i = 0;
  int sigma = 0;
  int *idMcHit = new int[5];
  int e = 0;
  for (i = 0 ; i < spa_strip_h->nok; i++)
    {
      nStrip  = (int)(spa_strip[i].id_strip/100000.);
      idWaf   = spa_strip[i].id_strip-10000*((int)(spa_strip[i].id_strip/10000.));
      iSide   = (spa_strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = spa_strip[i].id_mchit[e];
      StScfStrip *newStrip = new StScfStrip(nStrip, spa_strip[i].adc_count, sigma, idMcHit);
      mWafers[idWaferToWaferNumb(idWaf)]->addStrip(newStrip, iSide);
    }
  delete [] idMcHit;
  return spa_strip_h->nok;
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


int  StScfBarrel::readNoiseFromTable(table_head_st *noise_h, sdm_calib_db_st *noise, sls_ctrl_st *sls_ctrl)
{
  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  int i = 0;
  for (i = 0 ; i < noise_h->nok; i++)
    {
      nStrip  = (int)(noise[i].id_strip/100000.);
      idWaf   = noise[i].id_strip-10000*((int)(noise[i].id_strip/10000.));
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;

      mWafers[idWaferToWaferNumb(idWaf)]->setSigmaStrip(nStrip, iSide, noise[i].n_sigma, sls_ctrl);
    }
  return noise_h->nok;
}

void StScfBarrel::doSideClusterisation(int *barrelNumbOfCluster, sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl)
{
  int *wafNumbOfCluster = new int[2];
  int iWafer = 0;
  wafNumbOfCluster[0] = 0;
  wafNumbOfCluster[1] = 0;

  for (iWafer = 0 ; iWafer < mNWaferPerLadder*mNLadder; iWafer++)
    {
      mWafers[iWafer]->doClusterisation(wafNumbOfCluster, sls_ctrl, scf_ctrl);
      barrelNumbOfCluster[0] += wafNumbOfCluster[0];
      barrelNumbOfCluster[1] += wafNumbOfCluster[1];
    }
  delete[] wafNumbOfCluster;
}


int  StScfBarrel::writeClusterToTable(table_head_st *cluster_h, scf_cluster_st *cluster)
{
  int currRecord  = 0;
  cluster_h->nok = 0;
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);

      StScfListCluster *clusterP = mWafers[iWaf]->getClusterP();
      StScfListCluster *clusterN = mWafers[iWaf]->getClusterN();

      StScfCluster *pClusterP = clusterP->first();
      while (pClusterP)
	{
	  cluster[currRecord].id              = currRecord + 1;
	  cluster[currRecord].id_cluster      = 10000*(10*pClusterP->getNCluster() + 0)+idCurrentWaf;
	  cluster[currRecord].first_strip     = 10000*(10*pClusterP->getFirstStrip()+ 0)+idCurrentWaf;
	  cluster[currRecord].n_strip         = pClusterP->getClusterSize();
	  cluster[currRecord].adc_count       = pClusterP->getTotAdc();
	  cluster[currRecord].first_adc_count = pClusterP->getFirstAdc();
	  cluster[currRecord].last_adc_count  = pClusterP->getLastAdc();
	  cluster[currRecord].noise_count     = pClusterP->getTotNoise();
	  cluster[currRecord].flag            = pClusterP->getFlag();
	  cluster[currRecord].strip_mean      = pClusterP->getStripMean();
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      cluster[currRecord].id_mchit[i] = pClusterP->getIdMcHit(i);
	    }
	  currRecord++;
	  pClusterP    = clusterP->next(pClusterP);
	  cluster_h->nok++;
	}

      StScfCluster *pClusterN = clusterN->first();
      while (pClusterN)
	{
	  cluster[currRecord].id              = currRecord + 1;
	  cluster[currRecord].id_cluster      = 10000*(10*pClusterN->getNCluster() + 1)+idCurrentWaf;
	  cluster[currRecord].first_strip     = 10000*(10*pClusterN->getFirstStrip() + 1)+idCurrentWaf;
	  cluster[currRecord].n_strip         = pClusterN->getClusterSize();
	  cluster[currRecord].adc_count       = pClusterN->getTotAdc();
	  cluster[currRecord].first_adc_count = pClusterN->getFirstAdc();
	  cluster[currRecord].last_adc_count  = pClusterN->getLastAdc();
	  cluster[currRecord].noise_count     = pClusterN->getTotNoise();
	  cluster[currRecord].flag            = pClusterN->getFlag();
	  cluster[currRecord].strip_mean      = pClusterN->getStripMean();
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      cluster[currRecord].id_mchit[i] = pClusterN->getIdMcHit(i);
	    }
	  currRecord++;
	  pClusterN    = clusterN->next(pClusterN);
	  cluster_h->nok++;
	}

    }
  return currRecord;
}
