#include "StSpaBarrel.hh"
StSpaBarrel::StSpaBarrel(sdm_geom_par_st  *geom_par, sdm_calib_par_st *cal_par)
{
  double       lSigma = 1.;
  double       lMean  = 0.;
  unsigned int lSeed  = (unsigned int)cal_par[0].i_seed;
  this->setSpaParameters(geom_par);
  mGaussDistribution = new RanGauss(lMean, lSigma, lSeed);

  int nWafer  = mNLadder*mNWaferPerLadder; 
  int idWaf   = 0;
  
  mWafers = new StSpaWafer*[nWafer];

  int iWaf = 0;
  for (iWaf=0; iWaf < nWafer; iWaf++)
    { 
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSpaWafer(idWaf);
    }
}

StSpaBarrel::~StSpaBarrel()
{
  int i = 0;
  for (i = 0 ; i < mNLadder*mNWaferPerLadder ; i++)
    { delete mWafers[i]; }
}

void StSpaBarrel::setSpaParameters(sdm_geom_par_st  *geom_par)
{
  mSsdLayer        = geom_par[0].N_layer; // all layers : 1->7
  mNLadder         = geom_par[0].N_ladder;
  mNWaferPerLadder = geom_par[0].N_waf_per_ladder;
  mNStripPerSide   = geom_par[0].N_strip_per_side;
}

int StSpaBarrel::idWaferToWaferNumb(int idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  int iW = (int)((idWafer - mSsdLayer*1000)/100);
  int iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

int StSpaBarrel::waferNumbToIdWafer(int waferNumb)
{
  int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;  
}

int StSpaBarrel::readStripFromTable(table_head_st *sls_strip_h, sls_strip_st *sls_strip)
{
  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  int i = 0;
  int *idMcHit = new int[5];
  int e = 0;
  for (i = 0 ; i < sls_strip_h->nok; i++)
    {
      nStrip  = (int)(sls_strip[i].id_strip/100000.);
      idWaf   = sls_strip[i].id_strip-10000*((int)(sls_strip[i].id_strip/10000.));
      iSide   = (sls_strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = sls_strip[i].id_mchit[e];
      StSpaStrip *newStrip = new StSpaStrip(nStrip, i, sls_strip[i].adc_count, sls_strip[i].de, idMcHit);
      mWafers[idWaferToWaferNumb(idWaf)]->addStrip(newStrip, iSide
);
    }
  delete [] idMcHit;
  return sls_strip_h->nok; 
}

int  StSpaBarrel::readNoiseFromTable(table_head_st *noise_h, sdm_calib_db_st *noise)
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
      
      StSpaNoise *newStrip = 
	new StSpaNoise(nStrip ,noise[i].n_pedestal, noise[i].n_sigma);

      newStrip->setNoiseValue((int)((this->mGaussDistribution)->getValue()*(double)newStrip->getSigma()));
      
      mWafers[idWaferToWaferNumb(idWaf)]->addNoise(newStrip, iSide);
    }
  return noise_h->nok;
}

int  StSpaBarrel::readConditionDbFromTable(table_head_st *condition_h, sdm_condition_db_st *condition)
{
  int idWaf      = 0;
  int nStrip     = 0;
  int iSide      = 0;

  int i = 0;
  for (i = 0 ; i < condition_h->nok; i++)
    {
      if (!(condition[i].is_active))
	{
	  nStrip  = (int)(condition[i].id_strip/100000.);
	  idWaf   = condition[i].id_strip-10000*((int)(condition[i].id_strip/10000.));
	  iSide   = (condition[i].id_strip - nStrip*100000 - idWaf)/10000;
	  mWafers[idWaferToWaferNumb(idWaf)]->setIsActive(condition[i].is_active, iSide, nStrip);
	}
    }
  return condition_h->nok;
}


int  StSpaBarrel::writeStripToTable(table_head_st *out_strip_h, spa_strip_st *out_strip,
					  table_head_st *sls_strip_h, sls_strip_st *sls_strip)
{
  int currRecord   = 0;
  out_strip_h->nok = 0;
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);
      
      StSpaListStrip *stripP = mWafers[iWaf]->getStripP();
      StSpaListStrip *stripN = mWafers[iWaf]->getStripN();
					  
      StSpaStrip *pStripP = stripP->first();
      while (pStripP)
	{
	  
 	  out_strip[currRecord].id          = currRecord + 1;
	  out_strip[currRecord].adc_count   = pStripP->getDigitSig();
	  out_strip[currRecord].id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      out_strip[currRecord].id_mchit[i]   = pStripP->getIdMcHit(i);
	    }
	  currRecord++;
	  pStripP    = stripP->next(pStripP);
	  out_strip_h->nok++;
	}      
      
      StSpaStrip *pStripN = stripN->first();
      while (pStripN)
	{
	  
	  out_strip[currRecord].id          = currRecord + 1;
	  out_strip[currRecord].adc_count   = pStripN->getDigitSig();
	  out_strip[currRecord].id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      out_strip[currRecord].id_mchit[i]   = pStripN->getIdMcHit(i);
	    }
	  currRecord++;
	  pStripN    = stripN->next(pStripN);
	  out_strip_h->nok++;
	}      
      
    }      
  return currRecord;
}

void  StSpaBarrel::addNoiseToStrip(sls_ctrl_st *ctrl)
{
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      mWafers[iWaf]->sortNoise();
      mWafers[iWaf]->sortStrip();
      mWafers[iWaf]->addNoiseToStripSignal(ctrl[0].NElectronInAMip,ctrl[0].A128Dynamic);
    }
}

void  StSpaBarrel::doDaqSimulation(sls_ctrl_st *ctrl)
{
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      mWafers[iWaf]->convertAnalogToDigit(ctrl[0].NElectronInAMip,
					  ctrl[0].ADCDynamic,
					  ctrl[0].NBitEncoding,
					  ctrl[0].DAQCutValue);
      mWafers[iWaf]->pedestalSubstraction();
      mWafers[iWaf]->zeroSubstraction();
      mWafers[iWaf]->updateListStrip();
    }  
}
