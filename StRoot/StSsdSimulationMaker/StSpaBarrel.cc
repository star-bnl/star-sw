#include "StSpaBarrel.hh"

#include "tables/St_sls_strip_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_sdm_condition_db_Table.h"
#include "tables/St_spa_strip_Table.h"

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

int StSpaBarrel::readStripFromTable(St_sls_strip *sls_strip)
{
  sls_strip_st *strip = sls_strip->GetTable();

  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  int i = 0;
  //  int *idMcHit = new int[5];
  int idMcHit[5] = {0,0,0,0,0};
  int e = 0;
  for (i = 0 ; i < sls_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSpaStrip *newStrip = new StSpaStrip(nStrip, i, strip[i].adc_count, strip[i].de, idMcHit);
      mWafers[idWaferToWaferNumb(idWaf)]->addStrip(newStrip, iSide);
    }
  //  delete [] idMcHit;
  return sls_strip->GetNRows(); 
}

int  StSpaBarrel::readNoiseFromTable(St_sdm_calib_db *sdm_noise)
{
  sdm_calib_db_st *noise = sdm_noise->GetTable();

  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  int i = 0;
  for (i = 0 ; i < sdm_noise->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id_strip/100000.);
      idWaf   = noise[i].id_strip-10000*((int)(noise[i].id_strip/10000.));
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;
      
      StSpaNoise *newStrip = new StSpaNoise(nStrip ,noise[i].n_pedestal, noise[i].n_sigma);

      newStrip->setNoiseValue((int)((this->mGaussDistribution)->getValue()*(double)newStrip->getSigma()));
      
      mWafers[idWaferToWaferNumb(idWaf)]->addNoise(newStrip, iSide);
    }
  return sdm_noise->GetNRows();
}

int  StSpaBarrel::readConditionDbFromTable(St_sdm_condition_db *sdm_condition)
{
  sdm_condition_db_st *condition = sdm_condition->GetTable();

  int idWaf      = 0;
  int nStrip     = 0;
  int iSide      = 0;

  int i = 0;
  for (i = 0 ; i < sdm_condition->GetNRows(); i++)
    {
      if (!(condition[i].is_active))
	{
	  nStrip  = (int)(condition[i].id_strip/100000.);
	  idWaf   = condition[i].id_strip-10000*((int)(condition[i].id_strip/10000.));
	  iSide   = (condition[i].id_strip - nStrip*100000 - idWaf)/10000;
	  mWafers[idWaferToWaferNumb(idWaf)]->setIsActive(condition[i].is_active, iSide, nStrip);
	}
    }
  return sdm_condition->GetNRows();
}


int  StSpaBarrel::writeStripToTable(St_spa_strip *spa_strip)
{
  spa_strip_st out_strip;
  
  int currRecord   = 0;
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      int idCurrentWaf = waferNumbToIdWafer(iWaf);
      
      StSpaListStrip *stripP = mWafers[iWaf]->getStripP();
      StSpaListStrip *stripN = mWafers[iWaf]->getStripN();
					  
      StSpaStrip *pStripP = stripP->first();
      while (pStripP)
	{
	  
 	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripP->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      out_strip.id_mchit[i]   = pStripP->getIdMcHit(i);
	    }
	  spa_strip->AddAt(&out_strip);
	  currRecord++;
	  pStripP    = stripP->next(pStripP);
	}      
      
      StSpaStrip *pStripN = stripN->first();
      while (pStripN)
	{
	  
	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripN->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	  for (int i = 0 ; i < 5 ; i++)
	    {
	      out_strip.id_mchit[i]   = pStripN->getIdMcHit(i);
	    }
	  spa_strip->AddAt(&out_strip);
	  currRecord++;
	  pStripN    = stripN->next(pStripN);
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
