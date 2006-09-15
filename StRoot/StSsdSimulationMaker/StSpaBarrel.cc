// $Id: StSpaBarrel.cc,v 1.4 2006/09/15 21:09:52 bouchet Exp $
//
// $Log: StSpaBarrel.cc,v $
// Revision 1.4  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.3  2005/11/22 03:56:44  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#include "StSpaBarrel.hh"

#include "tables/St_sls_strip_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_sdm_condition_db_Table.h"
#include "tables/St_spa_strip_Table.h"

#include "tables/St_ssdStripCalib_Table.h"

StSpaBarrel::StSpaBarrel(ssdDimensions_st  *geom_par, sdm_calib_par_st *cal_par)
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

void StSpaBarrel::setSpaParameters(ssdDimensions_st  *geom_par)
{
  mSsdLayer        = 7; // all layers : 1->7
  mNLadder         = 20;
  mNWaferPerLadder = geom_par[0].wafersPerLadder;
  mNStripPerSide   = geom_par[0].stripPerSide;
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
  int e  = 0;
  int my_counterP =0;
  int my_counterN =0;
  for (i = 0 ; i < sls_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      if(iSide==0) my_counterP++;
      if(iSide==1) my_counterN++;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSpaStrip *newStrip = new StSpaStrip(nStrip, i, strip[i].adc_count, strip[i].de, idMcHit);
      mWafers[idWaferToWaferNumb(idWaf)]->addStrip(newStrip, iSide);
    }
  //  delete [] idMcHit;
  return sls_strip->GetNRows(); 
}

int  StSpaBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib)
{
  ssdStripCalib_st *noise = strip_calib->GetTable();
  int NAdcChannel          = (int)pow(2.0,10.0*1.0);
  int nElectronInAMip      = 22500;
  int adcDynamic           = 20;
  const float   AdctoE     =  (adcDynamic*nElectronInAMip)/(float)(NAdcChannel);
  printf("AdctoE = %f\n",AdctoE);
  
  int idWaf  = 0;
  int nStrip = 0;
  int iSide  = 0;
  int i      = 0;
  int ent    = 0 ;
  for (i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	int my_noise    = (int)((noise[i].rms*AdctoE)/16);
	int my_pedestal = (int)(noise[i].pedestals*AdctoE); 
	//if((idWaf==7101)&&(iSide==0))printf("id=%d stripId=%d  side=%d  waferId=%d  pedestal=%d  noise=%d\n",noise[i].id,nStrip,iSide,idWaf,noise[i].pedestals,noise[i].rms);
	StSpaNoise *newStrip = new StSpaNoise(nStrip ,(int)(noise[i].pedestals*AdctoE),(int)((noise[i].rms*AdctoE)/16.));
	mWafers[idWaferToWaferNumb(idWaf)]->addNoise(newStrip,iSide);
	ent++;
      }
    }
  printf("Entries = %d\n",ent);
  return ent;
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
int  StSpaBarrel::writeStripToTable(St_spa_strip *spa_strip,St_sls_strip *sls_strip )
{
  spa_strip_st out_strip;
  sls_strip_st *strip = sls_strip->GetTable();
  int my_counterP =0;
  int my_counterN =0;
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
	      out_strip.id_mchit[i] = pStripP->getIdMcHit(i);
	      if(out_strip.id_mchit[i]==0) {
		out_strip.id_mctrack[i]=0;}
	      else {
		for(int j = 0 ; j < sls_strip->GetNRows(); j++){
		    if(out_strip.id_mchit[i] == strip[j].id_mchit[i]) 
		      out_strip.id_mctrack[i] = strip[j].id_mctrack[i];
		  }
		}
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
	      if(out_strip.id_mchit[i]==0)  {
		out_strip.id_mctrack[i]=0;}
	      else {
		for(int j = 0 ; j < sls_strip->GetNRows(); j++){
		    if(out_strip.id_mchit[i] == strip[j].id_mchit[i]) 
		      out_strip.id_mctrack[i] = strip[j].id_mctrack[i];
		}
	      }
	    }
	  spa_strip->AddAt(&out_strip);
	  currRecord++;
	  pStripN    = stripN->next(pStripN);
	}
    }
  return currRecord;
}

void  StSpaBarrel::addNoiseToStrip(slsCtrl_st *ctrl)
{
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      mWafers[iWaf]->sortNoise();
      mWafers[iWaf]->sortStrip();
      mWafers[iWaf]->addNoiseToStripSignal(ctrl[0].nElectronInAMip,ctrl[0].a128Dynamic);
    }
}

void  StSpaBarrel::doDaqSimulation(slsCtrl_st *ctrl)
{
  int iWaf = 0;
  for (iWaf = 0; iWaf < mNLadder*mNWaferPerLadder ; iWaf++)
    {
      mWafers[iWaf]->convertAnalogToDigit(ctrl[0].nElectronInAMip,
					  ctrl[0].adcDynamic,
					  ctrl[0].nbitEncoding,
					  ctrl[0].daqCutValue);
      mWafers[iWaf]->pedestalSubstraction();
      mWafers[iWaf]->zeroSubstraction();
      mWafers[iWaf]->updateListStrip();
    }  
}
