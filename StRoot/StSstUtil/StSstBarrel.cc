//$Id: StSstBarrel.cc,v 1.8 2016/06/10 19:26:55 bouchet Exp $
//
//$Log: StSstBarrel.cc,v $
//Revision 1.8  2016/06/10 19:26:55  bouchet
//coverity : FORWARD_NULL
//
//Revision 1.7  2016/06/08 20:52:57  bouchet
//coverity : PASS_BY_VALUE
//
//Revision 1.6  2016/05/29 19:34:51  bouchet
//coverity : CTOR_DTOR_LEAK fixed
//
//Revision 1.5  2016/05/27 15:21:13  bouchet
//cleanup cout
//
//Revision 1.4  2015/11/17 18:09:51  bouchet
//Long Zhou modification, move cout to LOG_DEBUG
//
//Revision 1.3  2015/07/21 14:29:22  bouchet
//removed unused variables
//
//Revision 1.2  2015/06/27 19:48:51  bouchet
//removed obsolete libraries : ssdConfiguration, ssdDimensions, ssdWafersPosition ; fixed static StSstBarrel name
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.6  2015/06/19 14:16:23  bouchet
//readNoiseFromTable() takes sstStripCalib data as input, not the structure
//
//Revision 1.5  2015/05/23 20:51:51  bouchet
//ADC hits set properly in writePointToContainer() ; cosmetics
//
//Revision 1.4  2015/04/21 18:26:08  bouchet
//typos between SSD and SST name fixed
//
//Revision 1.3  2015/04/20 20:09:20  bouchet
//added readNoiseFromTable() method using sstStripCalib table ; added includes for sstStripCalib and sstWafersPosition
//
//Revision 1.2  2015/04/20 19:01:25  bouchet
//removed some hard coded constants ; removed unused writePointToContainer() method
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein
#include "StSstUtil/StSstBarrel.hh"

#include "TFile.h"
#include "StEvent.h"
#include "StSstHit.h"
#include "StSstHitCollection.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_ssdPedStrip_Table.h"
//#include "tables/St_sstPedStrip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
//new
#include "tables/St_sstDimensions_Table.h"
#include "tables/St_sstConfiguration_Table.h"
#include "tables/St_sstWafersPosition_Table.h"
#include "tables/St_sstStripCalib_Table.h"
#include "tables/St_sstSlsCtrl_Table.h"
//
#include "tables/St_sls_strip_Table.h"
#include "tables/St_ssdStripCalib_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_spa_strip_Table.h"
#include "StSstUtil/StSstLadder.hh"
#include "StSstUtil/StSstWafer.hh"
#include "StSstUtil/StSstClusterControl.h"
#include "StSstDynamicControl.h"
#include "StSstStripList.hh"
#include "StSstStrip.hh"
#include "StSstUtil/StSstClusterList.hh"
#include "StSstUtil/StSstCluster.hh"
#include "StSstUtil/StSstPointList.hh"
#include "StSstUtil/StSstPoint.hh"
#include "SystemOfUnits.h"
#include "StarMagField.h"
#include "TMath.h"
#include "StMessMgr.h"

#include "tables/St_ssdNoise_Table.h"
//new
#include "tables/St_sstGainCalibWafer_Table.h"
#include "tables/St_sstNoise_Table.h"
#include "tables/St_sstWaferConfiguration_Table.h"

#include "StMcEvent/StMcSsdHitCollection.hh"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StSstUtil/StSstConsts.h"

#include "TDataSetIter.h"

StSstBarrel* StSstBarrel::fSstBarrel = 0;
//________________________________________________________________________________
/*!
Constructor using the sstDimensions_st and sstConfiguration_st tables from the db
 */
StSstBarrel::StSstBarrel(sstDimensions_st  *dimensions, sstConfiguration_st *config ) : mDebug(0)
{
  memset (first, 0, last-first);
  fSstBarrel = this;
  setSstParameters(dimensions);
  // Set the Lorentz shift for holes and electrons
  setLorentzShift(dimensions);
  if (config) {
    mNLadder         = config[0].nMaxLadders;
    for (Int_t i=0;i<mNLadder;i++) {
      mActiveLadders[i]=1;
      if (config && config[0].ladderIsPresent[i]==0) 
	mActiveLadders[i]=0;
    }
  }
  mLadders = new StSstLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++){
    mLadders[iLad] = new StSstLadder(iLad,mSstLayer,mNWaferPerLadder,mNStripPerSide);
    if (Debug()) mLadders[iLad]->SetDebug(Debug());
  }
}
//________________________________________________________________________________
StSstBarrel::~StSstBarrel(){
  delete [] mLadders;
  fSstBarrel = 0;
}
//________________________________________________________________________________
void StSstBarrel::setSstParameters(sstDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSstLayer            = 7; // historic : SVT was first 6 layers
  // note to self --> change it later
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}
//________________________________________________________________________________
void StSstBarrel::setLorentzShift(sstDimensions_st *geom_par){
  Float_t center[3] = {0,0,0};
  Float_t B[3]      = {0,0,0};
  //we take the BField at the point (0,0,0)
  StarMagField::Instance()->BField(center,B);
  //use the values from CMS
  Float_t scale        = 1.61;
  Float_t tan_theta_h  = scale*TMath::ATan(TMath::Tan(21*2*TMath::Pi()/360)*(B[2]/40.));
  Float_t tan_theta_e  = scale*TMath::ATan(TMath::Tan( 8*2*TMath::Pi()/360)*(B[2]/40.));
  mShift_hole          = geom_par[0].waferHalfThickness*tan_theta_h;
  mShift_elec          = geom_par[0].waferHalfThickness*tan_theta_e;
}
//________________________________________________________________________________

void StSstBarrel::debugUnPeu (Int_t monladder, Int_t monwafer){
  for (Int_t i=0;i<this->getNumberOfLadders();i++)
    {
      if (this->mLadders[i]->getLadderNumb()==monladder) 
	{
	  LOG_INFO<<" Ladder "<<monladder<<" found"<<endm;
	  this->mLadders[i]->debugUnPeu(monwafer);
	}
    }
}
//______________________________________________________________
void StSstBarrel::initLadders(St_sstWafersPosition *wafpos) {for (Int_t iLad = 0; iLad < mNLadder; iLad++) mLadders[iLad]->initWafers(wafpos);}
//________________________________________________________________________________
void StSstBarrel::Reset() {for (Int_t iLad = 0; iLad < mNLadder; iLad++) mLadders[iLad]->Reset();}
//________________________________________________________________________________
Int_t StSstBarrel::readStripFromTable(St_spa_strip *spa_strip){
  spa_strip_st *strip = spa_strip->GetTable();
  
  Int_t NumberOfStrip = 0;
  Int_t idWaf         = 0;
  Int_t iWaf          = 0;
  Int_t iLad          = 0;
  Int_t nStrip        = 0;
  Int_t iSide         = 0;
  Float_t sigma       = 3.0;
  Int_t iPedestal     = 120;
  Int_t idMcHit[5]    = {0,0,0,0,0};
  Int_t idMcTrack[5]  = {0,0,0,0,0};
  Int_t e = 0;
  for (Int_t i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf    = idWaferToWafer(idWaf);
      iLad    = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) {
	idMcHit[e]   = strip[i].id_mchit[e];
	idMcTrack[e] = strip[i].id_mctrack[e];
      }
      StSstStrip *newStrip = new StSstStrip(nStrip, strip[i].adc_count, sigma, iPedestal, idMcHit,idMcTrack);
      mLadders[iLad]->mWafers[iWaf]->addStrip(newStrip, iSide);
    }
  NumberOfStrip = spa_strip->GetNRows();  
  return NumberOfStrip;
}
//________________________________________________________________________________
Int_t  StSstBarrel::writeNoiseToFile(St_spa_strip *spa_strip){
  spa_strip_st *strip = spa_strip->GetTable();
  St_ssdStripCalib *stripCal = new St_ssdStripCalib("ssdStripCalib",spa_strip->GetNRows());
  ssdStripCalib_st noise_strip;
  for (Int_t i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      noise_strip.id=strip[i].id_strip;
      noise_strip.pedestals=(short) strip[i].id_mchit[0];
      noise_strip.rms=(short) strip[i].adc_count;
      stripCal->AddAt(&noise_strip);
    }
  TFile f1("ssdStripCalib.root","NEW");
  stripCal->Write();
  f1.Close();
  return spa_strip->GetNRows();
}
//-----------------------------------------------------------------------------------------
Int_t  StSstBarrel::writeNewNoiseToFile3(St_ssdPedStrip  *spa_ped_strip, char myLabel[]){
  char *name =new char[100] ;
  ssdPedStrip_st *strip = spa_ped_strip->GetTable();
  St_ssdNoise *StripCal = new St_ssdNoise("ssdNoise",spa_ped_strip->GetNRows());
  ssdNoise_st  temp[mNLadder* mNWaferPerLadder];
  memset(temp, 0, (mNLadder* mNWaferPerLadder)*sizeof(ssdNoise_st));
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t wafer  = 0;
  Int_t N = spa_ped_strip->GetNRows();
  for (Int_t i=0; i< N;i++) {
      if (strip[i].id_strip>0 && strip[i].id_strip<=76818620) {
	nStrip  = (int)(strip[i].id_strip/100000.);
	idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
	iWaf    = (int)((idWaf - mSstLayer*1000)/100 - 1);
	iLad    = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
	wafer = iLad*mNWaferPerLadder +iWaf;
	if (iSide == 0) temp[wafer].rmsp[nStrip-1] = (unsigned char)strip[i].noise;
	if (iSide == 1) temp[wafer].rmsn[nStrip-1] = (unsigned char)strip[i].noise;
      }
  }
  for(Int_t i=0;i<mNLadder* mNWaferPerLadder;i++) {
    temp[i].id = i;
    StripCal->AddAt(&temp[i]);
  }
  sprintf(name,"%s%s%s","ssdNoise.",myLabel,".root");
  TFile f1(name,"RECREATE","SSD ped and noise file",9);
  StripCal->Write();
  f1.Close();
  return spa_ped_strip->GetNRows();
}
//________________________________________________________________________________
/*
 Method to read pedestal data and save them into a root file
*/
Int_t  StSstBarrel::writeNoiseToFile(St_ssdPedStrip *spa_ped_strip, char myLabel[]){ 
  char *name =new char[100] ;
  ssdPedStrip_st *strip = spa_ped_strip->GetTable();
  St_ssdStripCalib *stripCal = new St_ssdStripCalib("ssdStripCalib",spa_ped_strip->GetNRows());
  ssdStripCalib_st noise_strip;
  for (Int_t i = 0 ; i < spa_ped_strip->GetNRows(); i++)
    {
      noise_strip.id=strip[i].id_strip;
      noise_strip.pedestals= (unsigned char) strip[i].pedestal;
      noise_strip.rms=(unsigned char) strip[i].noise;
      stripCal->AddAt(&noise_strip);
    }
  sprintf(name,"%s%s%s","ssdStripCalib.",myLabel,".root");
  TFile f1(name,"RECREATE","SSD ped and noise file",9);
  stripCal->Write();
  f1.Close();
  return spa_ped_strip->GetNRows();
}
//________________________________________________________________________________
/*!
  Old method reading noise from the spa_noise table
 */
Int_t  StSstBarrel::readNoiseFromTable(St_sdm_calib_db *spa_noise, StSstDynamicControl *dynamicControl){
  sdm_calib_db_st *noise = spa_noise->GetTable();
  
  Int_t NumberOfNoise = 0;
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  for (Int_t i = 0 ; i < spa_noise->GetNRows(); i++)
    {
      nStrip  = (int)(noise[i].id_strip/100000.);
      idWaf   = noise[i].id_strip-10000*((int)(noise[i].id_strip/10000.));
      iWaf    = idWaferToWafer(idWaf);
      iLad    = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;
      mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip, iSide,0, noise[i].n_sigma, dynamicControl);
    }

  NumberOfNoise = spa_noise->GetNRows();
  return NumberOfNoise;
}
//________________________________________________________________________________
/*!
New method reading from the ssdStripCalib table
 */
Int_t  StSstBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib, StSstDynamicControl *dynamicControl){
  ssdStripCalib_st *noise = strip_calib->GetTable();
  
  Int_t NumberOfNoise = 0;
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  for (Int_t i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iWaf    = idWaferToWafer(idWaf);
	iLad    = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip, iSide, noise[i].pedestals, noise[i].rms, dynamicControl);
	NumberOfNoise++;
      }
    }
  return NumberOfNoise;
}
//---------------------------------------------------------------------------------------------
Int_t StSstBarrel::readNoiseDefault(StSstDynamicControl *dynamicControl){
  Int_t rms           = 48  ; 
  // the noise is coded as 16*rms then for each strip, noise = 60/16 = 3.75 adc
  Int_t ped           = 120 ;
  Int_t NumberOfNoise = 0;
  for(Int_t i=0;i<mNLadder;i++)
    {
      for(Int_t j=0;j<mNWaferPerLadder;j++)
	{
	  for(Int_t k=0;k<mNStripPerSide;k++)
	    {
	      for(Int_t iSide=0;iSide<2;iSide++)
		{
		  mLadders[i]->mWafers[j]->setPedestalSigmaStrip(k+1,iSide,ped,rms,dynamicControl);
		  NumberOfNoise++;
		}
	    }
	}
    }
  return NumberOfNoise;
}
//________________________________________________________________________________
/*!
first method reading from the ssdNoise table
 */
Int_t  StSstBarrel::readNoiseFromTable(St_ssdNoise *strip_noise, StSstDynamicControl *dynamicControl){
  ssdNoise_st *noise = strip_noise->GetTable();
  
  Int_t NumberOfNoise = 0;
  Int_t iWaf          = 0;
  Int_t iLad          = 0;
  Int_t nStrip        = 0;
  Int_t iSide         = 0;
  Int_t pedestal      = 120;//constant, not used later
  LOG_INFO << "size of m_noise3 table = " << (int)strip_noise->GetNRows() << endm;
  for (Int_t i = 0 ; i < strip_noise->GetNRows(); i++)
    {
      iWaf      = noise[i].id-(noise[i].id/mNWaferPerLadder)*mNWaferPerLadder;
      iLad      = noise[i].id/16;
      for(nStrip=0;nStrip<mNStripPerSide;nStrip++){
	iSide=0;
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip+1, iSide, pedestal, noise[i].rmsp[nStrip], dynamicControl);
	NumberOfNoise++;
	iSide=1;
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip+1, iSide, pedestal, noise[i].rmsn[nStrip], dynamicControl);
	NumberOfNoise++;
      }
    }
  return NumberOfNoise;
}
//________________________________________________________________________________
Int_t StSstBarrel::readNoiseFromTable(sstStripCalib_st *noise, StSstDynamicControl *dynamicControl){
  Int_t NumberOfNoise     = 0;
  Int_t iWaf              = 0;
  Int_t iLad              = 0;
  Int_t iStrip            = 0;
  Int_t iSide             = 0;
  Int_t TotStripSide      = kSstNumLadders * kSstNumSensorsPerLadder * kSstNumStripsOnSensor;
  Int_t TotStripPerLadder = kSstNumSensorsPerLadder * kSstNumStripsOnSensor;
  
  /*
    coding is : id = (491520)*side[0-1]/2 + ladder[0-19]*12288 + wafer[0-15]*768 + strip[0-767]
  */
  
  for (Int_t i = 0 ; i < (2*TotStripSide); i++){
    iSide  = i/TotStripSide;
    iLad   = (i - (iSide*TotStripSide))/TotStripPerLadder;
    iWaf   = (i - iSide*TotStripSide - iLad*TotStripPerLadder)/kSstNumStripsOnSensor;
    iStrip = i - iSide*TotStripSide - iLad*TotStripPerLadder - iWaf*kSstNumStripsOnSensor;
    LOG_DEBUG <<" i/side/lad/waf/strip/ped/rms : " << i << " " << iSide << " " << iLad <<" " << iWaf <<" " << iStrip <<" " << (int)noise[0].pedestals[i] <<" "<< (int)noise[0].rms[i] << endm;
    mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(iStrip+1, iSide, (int)noise[0].pedestals[i], (int)noise[0].rms[i], dynamicControl);
    NumberOfNoise++;
  }
  return NumberOfNoise;
}
//________________________________________________________________________________
      
Int_t StSstBarrel::readClusterFromTable(St_scf_cluster *scf_cluster){
  scf_cluster_st *cluster = scf_cluster->GetTable();

  Int_t NumberOfCluster = 0;
  Int_t idWaf           = 0;
  Int_t iWaf            = 0;
  Int_t iLad            = 0;
  Int_t nCluster        = 0;
  Int_t nPCluster       = 0;
  Int_t nNCluster       = 0;
  Int_t iSide           = 0;
  Int_t idMcHit[5]      = {0,0,0,0,0};
  Int_t e               = 0;
  Int_t nStrip          = 0;
  Int_t nFirstStrip     = 0;
  Int_t nFirstAdc       = 0;
  Int_t nLastAdc        = 0;
  Int_t nAdcCount       = 0;
  Float_t nNoiseCount   = 0;
  Float_t nStripMean    = 0;
  Int_t nFlag           = 0;

  for (Int_t i = 0 ; i < scf_cluster->GetNRows(); i++)
    {
      nCluster    = (int)(cluster[i].id_cluster/100000.);
      idWaf       = (cluster[i].id_cluster-10000*((int)(cluster[i].id_cluster/10000.)));
      iSide       = (cluster[i].id_cluster-idWaf-nCluster*100000)/10000;
      iWaf        = idWaferToWafer(idWaf);
      iLad        = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
      nFirstStrip = (int)(cluster[i].first_strip/100000.);
      nStrip      = cluster[i].n_strip;
      nFirstAdc   = cluster[i].first_adc_count;
      nLastAdc    = cluster[i].last_adc_count;
      nAdcCount   = cluster[i].adc_count;
      nNoiseCount = cluster[i].noise_count;
      nStripMean  = cluster[i].strip_mean;
      nFlag       = cluster[i].flag;
      for (e = 0 ; e < 5; e++) idMcHit[e] = cluster[i].id_mchit[e];
      StSstCluster *newCluster = new StSstCluster(nCluster, nFirstStrip, nStrip, nAdcCount, nFirstAdc, nLastAdc, nNoiseCount, nStripMean, nFlag, idMcHit);
      if (iSide == 0)
	{nPCluster++;}
      else
        {nNCluster++;}
      mLadders[iLad]->mWafers[iWaf]->addCluster(newCluster, iSide);
    }

  NumberOfCluster = scf_cluster->GetNRows();  
  return NumberOfCluster;
}
//________________________________________________________________________________
Int_t  StSstBarrel::writeClusterToTable(St_scf_cluster *scf_cluster){
  scf_cluster_st cluster;
  Int_t currRecord  = 0;
  Int_t i           = 0;

  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
      {
	Int_t idCurrentWaf = mSstLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSstClusterList *clusterP = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	StSstClusterList *clusterN = mLadders[iLad]->mWafers[iWaf]->getClusterN();

	StSstCluster *pClusterP = clusterP->first();
	while (pClusterP)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterP->getNCluster() + 0)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterP->getFirstStrip()+ 0)+idCurrentWaf;
	    cluster.n_strip         = pClusterP->getClusterSize();
	    cluster.adc_count       = pClusterP->getTotAdc();
	    cluster.first_adc_count = pClusterP->getFirstAdc();
	    cluster.last_adc_count  = pClusterP->getLastAdc();
	    cluster.noise_count     = (int)pClusterP->getTotNoise();
	    cluster.flag            = pClusterP->getFlag();
	    cluster.strip_mean      = pClusterP->getStripMean();
	    for (i = 0 ; i < 5 ; i++)
	      cluster.id_mchit[i] = pClusterP->getIdMcHit(i);
	    scf_cluster->AddAt(&cluster);
	    currRecord++;
	    pClusterP    = clusterP->next(pClusterP);
	  }

	StSstCluster *pClusterN = clusterN->first();
	while (pClusterN)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterN->getNCluster() + 1)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterN->getFirstStrip() + 1)+idCurrentWaf;
	    cluster.n_strip         = pClusterN->getClusterSize();
	    cluster.adc_count       = pClusterN->getTotAdc();
	    cluster.first_adc_count = pClusterN->getFirstAdc();
	    cluster.last_adc_count  = pClusterN->getLastAdc();
	    cluster.noise_count     = (int)pClusterN->getTotNoise();
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
//________________________________________________________________________________
Int_t  StSstBarrel::writeClusterToTable(St_scf_cluster *scf_cluster,St_spa_strip *spa_strip){
  scf_cluster_st cluster;
  spa_strip_st *on_strip = spa_strip->GetTable(); 
  Int_t currRecord  = 0;
  Int_t i           = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
      {
	Int_t idCurrentWaf = mSstLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSstClusterList *clusterP = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	StSstClusterList *clusterN = mLadders[iLad]->mWafers[iWaf]->getClusterN();
	
	StSstCluster *pClusterP = clusterP->first();
	while (pClusterP)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterP->getNCluster() + 0)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterP->getFirstStrip()+ 0)+idCurrentWaf;
	    cluster.n_strip         = pClusterP->getClusterSize();
	    cluster.adc_count       = pClusterP->getTotAdc();
	    cluster.first_adc_count = pClusterP->getFirstAdc();
	    cluster.last_adc_count  = pClusterP->getLastAdc();
	    cluster.noise_count     = (Int_t) pClusterP->getTotNoise();
	    cluster.flag            = pClusterP->getFlag();
	    cluster.strip_mean      = pClusterP->getStripMean();
	    for (i = 0 ; i < 5 ; i++)
	      {
		cluster.id_mchit[i] = pClusterP->getIdMcHit(i);
		if(cluster.id_mchit[i] == 0){
		  cluster.id_mctrack[i]=0;
		}
		else{
		  for(Int_t j = 0 ; j < spa_strip->GetNRows(); j++){
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
	
	StSstCluster *pClusterN = clusterN->first();
	while (pClusterN)
	  {
	    cluster.id              = currRecord + 1;
	    cluster.id_cluster      = 10000*(10*pClusterN->getNCluster() + 1)+idCurrentWaf;
	    cluster.first_strip     = 10000*(10*pClusterN->getFirstStrip() + 1)+idCurrentWaf;
	    cluster.n_strip         = pClusterN->getClusterSize();
	    cluster.adc_count       = pClusterN->getTotAdc();
	    cluster.first_adc_count = pClusterN->getFirstAdc();
	    cluster.last_adc_count  = pClusterN->getLastAdc();
	    cluster.noise_count     = (Int_t) pClusterN->getTotNoise();
	    cluster.flag            = pClusterN->getFlag();
	    cluster.strip_mean      = pClusterN->getStripMean();
	    for (i = 0 ; i < 5 ; i++)
	      {
		cluster.id_mchit[i] = pClusterN->getIdMcHit(i);
		if(cluster.id_mchit[i] == 0)cluster.id_mctrack[i]=0;
		else{
		  for(Int_t j = 0 ; j < spa_strip->GetNRows(); j++){
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
/***********************************************************/
Int_t StSstBarrel::writePointToContainer(St_scm_spt *scm_spt, StSstHitCollection* ssdHitColl,St_scf_cluster *scf_cluster,StSstDynamicControl *mDynamicControl){
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  scm_spt_st spt;
  StSstHit *currentSstHit;
  scf_cluster_st *on_cluster = scf_cluster->GetTable(); 
  // table size is 148 bytes
  Int_t i = 0, inContainer = 0, inTable = 0 ;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  Int_t hw; Float_t q ; unsigned char c; 
  c =0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	Int_t idCurrentWaf = mSstLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSstPointList *sptList = mLadders[iLad]->mWafers[iWaf]->getPoint();
	StSstPoint *pSpt = sptList->first();
     
	while (pSpt){
	  for (i = 0 ; i < 5 ; i++)
	    {	  
	      spt.id_mchit[i]   = pSpt->getNMchit(i);
	      spt.id_mctrack[i] = 0;
	      spt.id_track[i]   = 0;
	      //we look on the clusters table to get the IdMctrack info
	      if (spt.id_mchit[i] == 0) spt.id_mctrack[i]=0;
	      else {
		for(Int_t j = 0 ; j < scf_cluster->GetNRows(); j++){
		  if(spt.id_mchit[i] == on_cluster[j].id_mchit[i]){
		    spt.id_mctrack[i] = on_cluster[j].id_mctrack[i];
		  }
		}
	      }
	    }
	  
	  //now we fill StEvent and get the correct IdTruth 
	  if (ssdHitColl){ // If Available, Fill the StEvent Container
	    for (i = 0 ; i < 3 ; i++){
	      gPos[i]      =  pSpt->getXg(i);
	      gPosError[i] =  0.0; 
	    }
	    hw = idCurrentWaf;
	    q =  pSpt->getDe(0);
	    currentSstHit = new StSstHit(gPos,gPosError,hw,q,c);
	    currentSstHit->setIdTruth(spt.id_mctrack[0],100);// need to check first = most probable!
	  	    
	    //looking for the correct clusters...
	    Int_t Id_P_Side = pSpt->getIdClusterP();
	    Int_t Id_N_Side = pSpt->getIdClusterN();
	    
            StSstClusterList *currentListP_j = mLadders[iLad]->mWafers[iWaf]->getClusterP();
            StSstCluster     *cluster_P_j   = currentListP_j->first();
	    do{
	      if(cluster_P_j->getNCluster()==Id_P_Side) 
                break;
              cluster_P_j = currentListP_j->next(cluster_P_j);
	    }
            while(cluster_P_j!=currentListP_j->last());

            StSstClusterList *currentListN_j = mLadders[iLad]->mWafers[iWaf]->getClusterN();
            StSstCluster *cluster_N_j       = currentListN_j->first();
	    do{
	      if(cluster_N_j->getNCluster()==Id_N_Side) 
		break;
	      cluster_N_j = currentListN_j->next(cluster_N_j);
	    }
            while(cluster_N_j!=currentListN_j->last());
	      
	    // encode the hardware position
	    // 2^3  detector ID number (8) 
	    // 2^4  4-12 num_wafer (0-319)
	    // 2^13 13-22 cebtral strip of the n-side cluster
	    // 2^23 23-27 strip of the p-side cluster relat. to n-side (-15,+16)
	    // 2^28 28-29 n-side cluster size(1-4) 
	    // 2^30 30-31 p-side cluster size(1-4)
	    hw  =         
	                 8                                                                             
  	      +         16 * idWaferToWaferNumb(idCurrentWaf)                                          
 	      +       8192 * (int)cluster_N_j->getStripMean()                                          
  	      +    8388608 * ((int)cluster_P_j->getStripMean() - (int)cluster_N_j->getStripMean() +15)
 	      +  268435456 * (int)((cluster_N_j->getClusterSize() > 3) ? 3 : cluster_N_j->getClusterSize()-1)
 	      + 1073741824 * (int)((cluster_P_j->getClusterSize() > 3) ? 3 : cluster_P_j->getClusterSize()-1);
  	    currentSstHit->setHardwarePosition(hw);
	    currentSstHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1),pSpt->getXl(2));
	    currentSstHit->setADC((int)convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1)),(int)convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1)));
  	    inContainer += ssdHitColl->addHit(currentSstHit);
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

/***********************************************************/
Int_t StSstBarrel::writePointToContainer(St_scm_spt *scm_spt, StSstHitCollection* sstHitColl,St_scf_cluster *scf_cluster,StSstDynamicControl *mDynamicControl,StMcEvent *mcEvent){
  vector<const StMcSsdHit*> hitCol;
  StMcSsdHitCollection *myCol;
  int totRatio, idTruth;
  Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  if(mcEvent)
    {
      LOG_DEBUG << "McEvent exists" << endm;
      myCol  = mcEvent->ssdHitCollection();
      if ((myCol))  
	{ 
	    for (unsigned int iLadder=0; iLadder<myCol->numberOfLadders(); iLadder++) {
	      for (unsigned int iWafer = 0; iWafer < myCol->ladder(iLadder)->numberOfWafers(); iWafer++) {
		for (StMcSsdHitIterator iter = myCol->ladder(iLadder)->wafer(iWafer)->hits().begin();
		     iter != myCol->ladder(iLadder)->wafer(iWafer)->hits().end();
		     iter++) {
		  const StMcSsdHit   *hit = dynamic_cast<const StMcSsdHit   *> (*iter);
		  assert(hit);
		  hitCol.push_back(hit);
		}
	      }
	    }
	}
    }
  else{LOG_INFO<< "No sstMcHitCollection" <<endm;
  }
  scm_spt_st spt;
  StSstHit *currentSstHit;
  scf_cluster_st *on_cluster = scf_cluster->GetTable(); 
  // table size is 148 bytes
  Int_t i = 0, inContainer = 0, inTable = 0;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  Int_t hw; Float_t q ; unsigned char c; 
  c =0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	Int_t idCurrentWaf = mSstLayer*1000 + (iWaf+1)*100 + (iLad+1);//decode the wafer id
	StSstPointList *sptList = mLadders[iLad]->mWafers[iWaf]->getPoint();//loop over StSstPoint list
	StSstPoint *pSpt = sptList->first();
	totRatio = 0;
	while (pSpt){
	  if(mcEvent){
	    
	    //we look only idMcTrack for simulation
	    //jb : we fill StEvent after getting the IdMctrack
	    //jb : as it was done too for the strip and clusters --> see StSpaBarrel.cc and StScfBarrel.cc
	    //printf("Now we find the idMcTrack from the cluster\n");	
	    int idClusP     = 0;
	    int idClusN     = 0;
	    for (i = 0 ; i < 5 ; i++)
	      {
		spt.id_mchit[i]   = pSpt->getNMchit(i);
		spt.id_mctrack[i] = 0;
		spt.id_track[i]   = 0;
		//we look on the clusters table to get the IdMctrack info
		if (spt.id_mchit[i] == 0) spt.id_mctrack[i]=0;
		else {
		  int j =0;
		  for(j = 0 ; j < scf_cluster->GetNRows(); j++){
		    if(spt.id_mchit[i] == on_cluster[j].id_mchit[i]){
		      spt.id_mctrack[i] = on_cluster[j].id_mctrack[i];
		      idClusP = 	    10000*(10*pSpt->getIdClusterP() + 0)+idCurrentWaf;
		      idClusN = 	    10000*(10*pSpt->getIdClusterN() + 1)+idCurrentWaf;
		      break;
		    }
		  }
		  Calculation_Ratio(idCurrentWaf,idClusP,idClusN,hitCol,&totRatio,&idTruth);
		}
	      }
	  }
	  //now we fill StEvent and get the correct IdTruth 
	  if (sstHitColl){ // If Available, Fill the StEvent Container
	    for (i = 0 ; i < 3 ; i++){
	      gPos[i]      =  pSpt->getXg(i);
	      gPosError[i] =  0.0; 
	    }
	    hw = idCurrentWaf;
	    q =  pSpt->getDe(0);
	    currentSstHit = new StSstHit(gPos,gPosError,hw,q,c);
	    //qATruth is set with the ratio chargeCLUSTER/chargeGEANT
	    currentSstHit->setIdTruth(idTruth,(int)totRatio);
	    // Start of Point Loop
	    
	    //looking for the correct clusters...
	    Int_t Id_P_Side = pSpt->getIdClusterP();
	    Int_t Id_N_Side = pSpt->getIdClusterN();
	    
	    StSstClusterList *currentListP_j = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	    StSstCluster     *cluster_P_j   = currentListP_j->first();

	    do{
	      if(cluster_P_j->getNCluster()==Id_P_Side) 
		break;
	      cluster_P_j = currentListP_j->next(cluster_P_j);
	    }
            while(cluster_P_j!=currentListP_j->last());

	    StSstClusterList *currentListN_j = mLadders[iLad]->mWafers[iWaf]->getClusterN();
	    StSstCluster *cluster_N_j       = currentListN_j->first();
	    do{
	      if(cluster_N_j->getNCluster()==Id_N_Side) 
		break;
	      cluster_N_j = currentListN_j->next(cluster_N_j);
	    }	
            while(cluster_N_j!=currentListN_j->last());
    
	    // encode the hardware position
	    // 2^3  detector ID number (8) 
	    // 2^4  4-12 num_wafer (0-319)
	    // 2^13 13-22 cebtral strip of the n-side cluster
	    // 2^23 23-27 strip of the p-side cluster relat. to n-side (-15,+16)
	    // 2^28 28-29 n-side cluster size(1-4) 
	    // 2^30 30-31 p-side cluster size(1-4)
	    hw  =         
	      8                                                                             
	      +         16 * idWaferToWaferNumb(idCurrentWaf)                                          
	      +       8192 * (int)cluster_N_j->getStripMean()                                          
	      +    8388608 * ((int)cluster_P_j->getStripMean() - (int)cluster_N_j->getStripMean() +15)
	      +  268435456 * (int)((cluster_N_j->getClusterSize() > 3) ? 3 : cluster_N_j->getClusterSize()-1)
	      + 1073741824 * (int)((cluster_P_j->getClusterSize() > 3) ? 3 : cluster_P_j->getClusterSize()-1);
	    currentSstHit->setHardwarePosition(hw);
	    //local position in 3D 
	    currentSstHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1),pSpt->getXl(2));
	    //save TotAdc of each cluster
	    currentSstHit->setADC((int)convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1)),(int)convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1)));
	    inContainer += sstHitColl->addHit(currentSstHit);
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
  if (hitCol.size()>0 )hitCol.clear();
  return inTable;
}
//________________________________________________________________________________
void StSstBarrel::Calculation_Ratio(int idWafer,int idClusterP,int idClusterN,vector<const StMcSsdHit*> hitCol, int *ratio, int *idTruth)
{
  Float_t charge[2]      = {0,0};
  int DominatorP         = 0;
  int DominatorN         = 0;
  Float_t adc_DominatorP = 0;
  Float_t adc_DominatorN = 0;
  vector<StSstStrip*> ListstripP;
  vector<StSstStrip*> ListstripN;
  vector<int> GTrackP(5,0),GTrackN(5,0);
  int isSplittedP=0,isSplittedN =0; 
  Int_t lad = idWaferToLadderNumb(idWafer);
  Int_t waf = (int)((idWafer - mSstLayer*1000)/100)-1;
  // now p-side
  StSstCluster *pClusterP = mLadders[lad]->mWafers[waf]->getClusterP()->first();
  while (pClusterP){
    int idP = 10000*(10*pClusterP->getNCluster() + 0)+idWafer;
    if(idP==idClusterP){
      charge[0] = pClusterP->getTotAdc();
      if(pClusterP->getFlag()==1){
	//splitted cluster
	isSplittedP = isSplit(pClusterP,0,lad,waf);
      }
      for(Int_t e =0 ;e<5;e++)GTrackP.push_back(pClusterP->getIdMcHit(e));
      //now we loop over the strips of that cluster
      StSstStripList *stripP = StSstBarrel::Instance()->mLadders[lad]->mWafers[waf]->getStripP();
      int lastScan =0;//lastScan : a same strips can be used by 2 adjacents clusters : it must be count twice
      for(int kk=pClusterP->getFirstStrip();kk<pClusterP->getFirstStrip()+pClusterP->getClusterSize();kk++)
	{
	  StSstStrip *pStripP = stripP->first();
	  while (pStripP)
	    {
	      if((pStripP->getNStrip()==kk)&&(pStripP->getNStrip()!=lastScan))
		{
		  lastScan = pStripP->getNStrip();
		  ListstripP.push_back(pStripP);
		}
	      pStripP = stripP->next(pStripP);
	    }
	}
      break;
    }
    pClusterP = mLadders[lad]->mWafers[waf]->getClusterP()->next(pClusterP);
  }
  // now n-side
  StSstCluster *pClusterN = mLadders[lad]->mWafers[waf]->getClusterN()->first();
  while (pClusterN){
    int idN  =  10000*(10*pClusterN->getNCluster() + 1)+idWafer;
    if(idN==idClusterN){
      charge[1] = pClusterN->getTotAdc();
      if(pClusterN->getFlag()==1){
	//splitted cluster
	isSplittedN = isSplit(pClusterN,1,lad,waf);
      }
      for(Int_t e =0 ;e<5;e++)GTrackN.push_back(pClusterN->getIdMcHit(e));
      //now we loop over the strips of that cluster 
      StSstStripList *stripN = mLadders[lad]->mWafers[waf]->getStripN();
      int lastScan =0;//lastScan : a same strip can be used by 2 adjacents clusters : it must be count twice
      for(int kk=pClusterN->getFirstStrip();kk<pClusterN->getFirstStrip()+pClusterN->getClusterSize();kk++)
	{
	  StSstStrip *pStripN = stripN->first();
	  while (pStripN)
	    {
	      if((pStripN->getNStrip()==kk)&&(pStripN->getNStrip()!=lastScan)) 
		{
		  lastScan = pStripN->getNStrip();
		  ListstripN.push_back(pStripN);
		}
	      pStripN = stripN->next(pStripN);
	    }
	}
      break;
    }
    pClusterN = mLadders[lad]->mWafers[waf]->getClusterN()->next(pClusterN);
  }

  // find dominator from cluster
  DominatorP = FindMcHit(GTrackP,hitCol);
  DominatorN = FindMcHit(GTrackN,hitCol);
  if(DominatorP!=DominatorN) {
    *ratio   = 0;
    *idTruth = 0;
    return;
  }
  if((ListstripN.size()>0)&&(ListstripP.size()>0)) 
    {
      unsigned int min ,max; 
      // side P
      min=0; 
      max=0;
      if(isSplittedP==1){
	min = 0;
	max = ListstripP.size()-1;}
      else 
	if(isSplittedP==2){
	  min = 1;
	  max = ListstripP.size();}
	else{
	  min = 0;
	  max = ListstripP.size();}
      for(unsigned int ee = min;ee<max;ee++){
	StSstStrip* tempoStripP = ListstripP[ee];
	if(DominatorP == tempoStripP->getIdMcTrack(0))
	  {
	    adc_DominatorP += tempoStripP->getDigitSig();
	  }
	else {
	}
      }
      
      //side N
      min =0;
      max =0;
      if(isSplittedN==1){
	min = 0;
	max = ListstripN.size()-1;
      }
      else 
	if(isSplittedN==2){
	  min = 1;
	  max = ListstripN.size();
	}
	else{
	  min = 0;
	  max = ListstripN.size();
	}
      for(unsigned int ee = min;ee<max;ee++){
	StSstStrip *tempoStripN = ListstripN[ee];
	if(DominatorN == tempoStripN->getIdMcTrack(0))
	  {
	    adc_DominatorN += tempoStripN->getDigitSig();
	  }
	else {
	}
      }
    }
  //find the Dominant contributor
  float ratioP   = 0 ;
  float ratioN   = 0 ;
  if(charge[0]!=0) ratioP = adc_DominatorP/charge[0];
  if(charge[1]!=0) ratioN = adc_DominatorN/charge[1];
  if(DominatorP == DominatorN){
    *ratio = (int)(100*(ratioP*ratioN));
    *idTruth = DominatorP;
  }
  else{
    *ratio   = 0;
    *idTruth = 0; 
  }
  if(ListstripP.size()>0) ListstripP.clear();
  if(ListstripN.size()>0) ListstripN.clear();
  if(GTrackP.size()>0) GTrackP.clear();
  if(GTrackN.size()>0) GTrackN.clear();
}
//__________________________________________________________________
Int_t StSstBarrel::FindMcHit(const vector<int> &tempo,const vector<const StMcSsdHit*> &hitCol)
{
  float ChargeTrack = 0.0;
  int idTrack =0;
  for(int e=0;e<5;e++){
    for (unsigned int hit = 0 ; hit<hitCol.size();hit++){
      const StMcSsdHit* currHit = hitCol[hit];
      if((tempo[e]>0)&&(currHit->key() == tempo[e])&&(currHit->dE()>ChargeTrack)){
	ChargeTrack = currHit->dE();
	idTrack = currHit->parentTrack()->key();
      }
    }
  }
  return idTrack;
}
//________________________________________________________________________________________
Int_t StSstBarrel::isSplit(StSstCluster *currentCluster,int iSide,int lad,int waf){
  StSstCluster *firstClus;
  StSstCluster *lastClus;
  StSstCluster *nextClus;
  StSstCluster *prevClus;
  int isSplitted =0 ;
  if(iSide ==0){
    firstClus = mLadders[lad]->mWafers[waf]->getClusterP()->first();
    lastClus  = mLadders[lad]->mWafers[waf]->getClusterP()->last();
  }
  else {
    firstClus = mLadders[lad]->mWafers[waf]->getClusterN()->first();
    lastClus  = mLadders[lad]->mWafers[waf]->getClusterN()->last();
  }
  if(currentCluster->getNCluster() == firstClus->getNCluster()){
    isSplitted=1;
  }
  else if(currentCluster->getNCluster() == lastClus->getNCluster()){
    isSplitted=2;
  }
  else {
    nextClus = currentCluster->getNextCluster();
    if(nextClus->getFirstStrip() == ((currentCluster->getFirstStrip()+currentCluster->getClusterSize()-1))) {
      isSplitted =1;
      prevClus = currentCluster->getPrevCluster();
      if(currentCluster->getFirstStrip() == ((prevClus->getFirstStrip()+prevClus->getClusterSize()-1))) {
	isSplitted=2;
      }
    }
  }
  return isSplitted;
}
//___________________________________________________________________
void StSstBarrel::doSideClusterisation(Int_t *barrelNumbOfCluster){
  Int_t wafNumbOfCluster[2];
  wafNumbOfCluster[0] = 0;
  wafNumbOfCluster[1] = 0;

  for (Int_t iLad = 0 ; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0 ; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->doClusterisation(wafNumbOfCluster, mClusterControl);
	barrelNumbOfCluster[0] += wafNumbOfCluster[0];
	barrelNumbOfCluster[1] += wafNumbOfCluster[1];
      }
}
//________________________________________________________________________________
void StSstBarrel::doSideClusterisation(Int_t *barrelNumbOfCluster,Int_t WafStatus[20][16]){
  Int_t wafNumbOfCluster[2];
  wafNumbOfCluster[0] = 0;
  wafNumbOfCluster[1] = 0;
  for (Int_t iLad = 0 ; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0 ; iWaf < mNWaferPerLadder; iWaf++)
      {
	if(WafStatus[iLad][iWaf]!=0){
	mLadders[iLad]->mWafers[iWaf]->doClusterisation(wafNumbOfCluster, mClusterControl);
	barrelNumbOfCluster[0] += wafNumbOfCluster[0];
	barrelNumbOfCluster[1] += wafNumbOfCluster[1];
	}
      }
}
//_______________________________________________________________________________________
Int_t StSstBarrel::doClusterMatching(Float_t *CalibArray){
  Int_t NumberOfPackage = 0;
  Int_t nSolved         = 0;
  Int_t nPerfect        = 0;
  
  for (Int_t iLad = 0; iLad < mNLadder; iLad++){
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      { 
	//mLadders[iLad]->mWafers[iWaf]->doLorentzShift(mDimensions,mShift_hole,mShift_elec);
	NumberOfPackage += mLadders[iLad]->mWafers[iWaf]->doFindPackage(mDimensions, mClusterControl);
	nPerfect  =  mLadders[iLad]->mWafers[iWaf]->doSolvePerfect(mDimensions, mClusterControl,CalibArray[(iLad*mNWaferPerLadder)+iWaf]);
	if (!nPerfect) continue;
	mLadders[iLad]->mWafers[iWaf]->doStatPerfect(nPerfect, mClusterControl);
	nSolved  += mLadders[iLad]->mWafers[iWaf]->doSolvePackage(mDimensions, mClusterControl,CalibArray[(iLad*mNWaferPerLadder)+iWaf]);
      }
  }
  LOG_INFO<<"****       Remark: "<<nSolved<<"  solved packages     ****" << endm;
  return NumberOfPackage;
}
//________________________________________________________________________________
void StSstBarrel::convertDigitToAnalog(StSstDynamicControl *dynamicControl){
  long nElectronInAMip        = dynamicControl->getnElectronInAMip();
  long adcDynamic             = dynamicControl->getadcDynamic();
  Double_t pairCreationEnergy = dynamicControl->getpairCreationEnergy();

  const Int_t NAdcChannel     = (int)pow(2.0,dynamicControl->getnbitEncoding());
  const Double_t convFactor   = (pairCreationEnergy*adcDynamic*nElectronInAMip)/NAdcChannel;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      mLadders[iLad]->mWafers[iWaf]->convertDigitToAnalog(convFactor);
}
//________________________________________________________________________________
void StSstBarrel::convertUFrameToOther(){
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->convertUFrameToLocal(mDimensions);
	mLadders[iLad]->mWafers[iWaf]->convertLocalToGlobal();
      }
}
//________________________________________________________________________________
void StSstBarrel::convertGlobalFrameToOther(){
  for (Int_t iLad = 0; iLad < mNLadder; iLad++){
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->convertGlobalToLocal();
	mLadders[iLad]->mWafers[iWaf]->convertLocalToUFrame(mDetectorLargeEdge, mDetectorSmallEdge, mTheta);
      }
  }
}
//________________________________________________________________________________
void StSstBarrel::sortListStrip(){
  StSstStripList *currentList = 0;
  Int_t isSorted = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
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
//________________________________________________________________________________
void StSstBarrel::sortListCluster(){
  StSstClusterList *currentList = 0;
  Int_t isSorted = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	currentList = mLadders[iLad]->mWafers[iWaf]->getClusterP();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortCluster();

	currentList = mLadders[iLad]->mWafers[iWaf]->getClusterN();
	isSorted = currentList->isSorted();
	if (!isSorted) currentList->sortCluster();
      }
}

StSstBarrel::StSstBarrel(const StSstBarrel & originalBarrel)
{
  mSstLayer             = originalBarrel.mSstLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;
  
  mLadders = new StSstLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSstLadder(iLad,mSstLayer,mNWaferPerLadder,mNStripPerSide);
}

StSstBarrel& StSstBarrel::operator=(const StSstBarrel & originalBarrel)
{
  mSstLayer             = originalBarrel.mSstLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;

  mLadders = new StSstLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSstLadder(iLad,mSstLayer,mNWaferPerLadder,mNStripPerSide);
  return *this;
}

Int_t StSstBarrel::idWaferToWaferNumb(Int_t idWafer)
{
  Int_t iW = (int)((idWafer - mSstLayer*1000)/100);
  Int_t iL = idWafer - mSstLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

Int_t StSstBarrel::idWaferToLadderNumb(Int_t idWafer)
{
  Int_t iW = (int)((idWafer - mSstLayer*1000)/100);
  Int_t iL = idWafer - mSstLayer*1000 - iW*100;
  return iL-1;
}

Int_t StSstBarrel::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  Int_t iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSstLayer*1000 + iW*100 + iL;
}

Int_t StSstBarrel::isActiveLadder(Int_t iLadder)
{
  return mActiveLadders[iLadder];
}
void StSstBarrel::renumHitAfterRemove() {
  Int_t iLast    = 0;
  Int_t iNewLast = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      iNewLast = mLadders[iLad]->mWafers[iWaf]->getPoint()->renumHits(iLast);
      iLast = iNewLast;
    }
}
//________________________________________________________________________________
StSstPointList *StSstBarrel::getInactiveHitList() {
  StSstPointList *inactiveHits = new StSstPointList();
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      //fill the list of  hits in inactive wafer areas
      StSstPointList *currDeadList = mLadders[iLad]->mWafers[iWaf]->getDeadHits(mDetectorLargeEdge, mDetectorSmallEdge, mStripPitch);
      if(currDeadList->getSize()>0){
      }
      inactiveHits = inactiveHits->addPointList(currDeadList);
      delete currDeadList;
    }
  inactiveHits->sortPoint();
  return inactiveHits;
}
//________________________________________________________________________________
void StSstBarrel::convertToStrip(Double_t pairCreationEnergy,
				 Int_t    nstripInACluster,
				 Double_t parDiffP,
				 Double_t parDiffN,
				 Double_t parIndRightP,
				 Double_t parIndRightN,
				 Double_t parIndLeftP,
				 Double_t parIndLeftN) {

  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      mLadders[iLad]->mWafers[iWaf]->convertToStrip(mStripPitch,
						    mNStripPerSide,
						    pairCreationEnergy,
						    nstripInACluster,
						    parDiffP,
						    parDiffN,
						    parIndRightP,
						    parIndRightN,
						    parIndLeftP,
						    parIndLeftN,
						    mShift_hole,
						    mShift_elec);
    }
}
//________________________________________________________________________________
void  StSstBarrel::addNoiseToStrip(slsCtrl_st *ctrl)
{
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      StSstWafer *wafer = mLadders[iLad]->mWafers[iWaf];
      wafer->sortNoise();
      wafer->sortStrip();
      wafer->addNoiseToStripSignal(ctrl[0].nElectronInAMip,ctrl[0].adcDynamic);
    }
}
//________________________________________________________________________________
Int_t StSstBarrel::readStripFromTable(St_sls_strip *sls_strip)
{
  sls_strip_st *strip = sls_strip->GetTable();

  Int_t idWaf  = 0, iWaf = 0, iLad = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t i = 0;
  Int_t idMcHit[5] = {0,0,0,0,0};
  Int_t e  = 0;
  Int_t my_counterP =0;
  Int_t my_counterN =0;
  for (i = 0 ; i < sls_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf = (int)((idWaf - mSstLayer*1000)/100)-1;
      iLad = idWaf - mSstLayer*1000 - (iWaf+1)*100-1;
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      if(iSide==0) my_counterP++;
      if(iSide==1) my_counterN++;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSstStrip *newStrip = new StSstStrip(nStrip, i, strip[i].adc_count, strip[i].de, idMcHit);
      mLadders[iLad]->mWafers[iWaf]->addStrip(newStrip, iSide);
    }
  return sls_strip->GetNRows(); 
}
//________________________________________________________________________________
Int_t  StSstBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib)
{
  ssdStripCalib_st *noise = strip_calib->GetTable();
  Int_t NAdcChannel          = (int)pow(2.0,10.0*1.0);
  Int_t nElectronInAMip      = 22500;
  Int_t adcDynamic           = 20;
  const Float_t   AdctoE     =  (adcDynamic*nElectronInAMip)/(float)(NAdcChannel);
  LOG_DEBUG << "AdctoE = " << AdctoE << endm;
  
  Int_t idWaf  = 0, iWaf = 0, iLad = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t i      = 0;
  Int_t ent    = 0 ;
  for (i = 0 ; i < strip_calib->GetNRows(); i++)
    {
      if (noise[i].id>0 && noise[i].id<=76818620) {
	nStrip  = (int)(noise[i].id/100000.);
	idWaf   = noise[i].id-10000*((int)(noise[i].id/10000.));
	iWaf = (int)((idWaf - mSstLayer*1000)/100)-1;
	iLad = idWaf - mSstLayer*1000 - (iWaf+1)*100-1;
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	StSpaNoise *newStrip = new StSpaNoise(nStrip ,(int)(noise[i].pedestals*AdctoE),(int)((noise[i].rms*AdctoE)/16.));
	mLadders[iLad]->mWafers[iWaf]->addNoiseToStripSignal(newStrip,iSide);
	ent++;
      }
    }
  return ent;
}
//________________________________________________________________________________
Int_t  StSstBarrel::readConditionDbFromTable(St_sdm_condition_db *sdm_condition)
{
  sdm_condition_db_st *condition = sdm_condition->GetTable();

  Int_t idWaf      = 0, iWaf = 0, iLad = 0;
  Int_t nStrip     = 0;
  Int_t iSide      = 0;

  Int_t i = 0;
  for (i = 0 ; i < sdm_condition->GetNRows(); i++)
    {
      if (!(condition[i].is_active))
	{
	  nStrip  = (int)(condition[i].id_strip/100000.);
	  idWaf   = condition[i].id_strip-10000*((int)(condition[i].id_strip/10000.));
	  iWaf    = idWaferToWaferNumb(idWaf);
	  iLad    = (int)(idWaf - mSstLayer*1000 - (iWaf+1)*100 - 1);
	  iSide   = (condition[i].id_strip - nStrip*100000 - idWaf)/10000;
	  mLadders[iLad]->mWafers[iWaf]->setIsActive(condition[i].is_active, iSide, nStrip);
	}
    }
  return sdm_condition->GetNRows();
}
//________________________________________________________________________________
Int_t  StSstBarrel::writeStripToTable(St_spa_strip *spa_strip)
{
  spa_strip_st out_strip;
  
  Int_t currRecord   = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
    {
      Int_t idCurrentWaf = mSstLayer*1000 +((iWaf+1)*100)+(iLad+1);
      StSstStripList *stripP = mLadders[iLad]->mWafers[iWaf]->getStripP();
      StSstStripList *stripN = mLadders[iLad]->mWafers[iWaf]->getStripN();
					  
      StSstStrip *pStripP = stripP->first();
      while (pStripP)
	{	  
 	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripP->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	  for (Int_t i = 0 ; i < 5 ; i++)
	    {
	      out_strip.id_mchit[i]   = pStripP->getIdMcHit(i);
	    }
	  spa_strip->AddAt(&out_strip);
	  currRecord++;
	  pStripP    = stripP->next(pStripP);
	}      
      
      StSstStrip *pStripN = stripN->first();
      while (pStripN)
	{
	  
	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripN->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	  for (Int_t i = 0 ; i < 5 ; i++)
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
//________________________________________________________________________________
Int_t  StSstBarrel::writeStripToTable(St_spa_strip *spa_strip,St_sls_strip *sls_strip )
{
  spa_strip_st out_strip;
  sls_strip_st *strip = sls_strip->GetTable();
  Int_t currRecord   = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
    {
      Int_t idCurrentWaf = mSstLayer*1000 +((iWaf+1)*100)+(iLad+1);
      StSstStripList *stripP = mLadders[iLad]->mWafers[iWaf]->getStripP();
      StSstStripList *stripN = mLadders[iLad]->mWafers[iWaf]->getStripN();
      StSstStrip *pStripP = stripP->first();
      while (pStripP)
	{ 
 	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripP->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripP->getNStrip() + 0)+idCurrentWaf;
	  for (Int_t i = 0 ; i < 5 ; i++)
	    {
	      out_strip.id_mchit[i] = pStripP->getIdMcHit(i);
	      if(out_strip.id_mchit[i]==0) {
		out_strip.id_mctrack[i]=0;}
	      else {
		for(Int_t j = 0 ; j < sls_strip->GetNRows(); j++){
		    if(out_strip.id_mchit[i] == strip[j].id_mchit[i]) 
		      out_strip.id_mctrack[i] = strip[j].id_mctrack[i];
		  }
		}
	      }
	  spa_strip->AddAt(&out_strip);
	  currRecord++;
	  pStripP    = stripP->next(pStripP);
	}
      
      StSstStrip *pStripN = stripN->first();
      while (pStripN)
	{ 
 	  out_strip.id          = currRecord + 1;
	  out_strip.adc_count   = pStripN->getDigitSig();
	  out_strip.id_strip    = 10000*(10*pStripN->getNStrip() + 1)+idCurrentWaf;
	  for (Int_t i = 0 ; i < 5 ; i++)
	    {
	      out_strip.id_mchit[i]   = pStripN->getIdMcHit(i);
	      if(out_strip.id_mchit[i]==0)  {
		out_strip.id_mctrack[i]=0;}
	      else {
		for(Int_t j = 0 ; j < sls_strip->GetNRows(); j++){
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
//________________________________________________________________________________
void  StSstBarrel::doDaqSimulation(slsCtrl_st *ctrl){
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)    {
      mLadders[iLad]->mWafers[iWaf]->convertAnalogToDigit(ctrl[0].nElectronInAMip,
							  ctrl[0].adcDynamic,
							  ctrl[0].nbitEncoding,
							  ctrl[0].daqCutValue);
      mLadders[iLad]->mWafers[iWaf]->pedestalSubstraction();
      mLadders[iLad]->mWafers[iWaf]->zeroSubstraction();
      mLadders[iLad]->mWafers[iWaf]->updateStripList();
    }  
}
//____________________________________________________________________
Int_t StSstBarrel::readNoiseDefaultForSimu(){
  Int_t rms              = 48;
  // the noise is coded as 16*rms then for each strip, noise = 60/16 = 3.75 adc
  Int_t ped              = 120 ;
  Int_t NAdcChannel      = (int)pow(2.0,10.0*1.0);
  Int_t nElectronInAMip  = 22500;
  Int_t adcDynamic       = 20;
  const Float_t   AdctoE = (adcDynamic*nElectronInAMip)/(float)(NAdcChannel);
  Int_t NumberOfNoise = 0;
  for(Int_t iLad=0;iLad<mNLadder;iLad++)
    {
      for(Int_t iWaf=0;iWaf<mNWaferPerLadder;iWaf++)
	{
	  for(Int_t NStrip=0;NStrip<mNStripPerSide;NStrip++)
	    {
	      for(Int_t iSide=0;iSide<2;iSide++)
		{
		  StSpaNoise *newStrip = new StSpaNoise(NStrip+1 ,(int)(ped*AdctoE),(int)((rms*AdctoE)/16.));
		  mLadders[iLad]->mWafers[iWaf]->addNoiseToStripSignal(newStrip,iSide);
		  NumberOfNoise++;
		}
	    }
	}
    }
  return NumberOfNoise;
}
