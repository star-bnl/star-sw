// $Id: StSsdBarrel.cc,v 1.4 2007/07/12 17:08:08 bouchet Exp $
//
// $Log: StSsdBarrel.cc,v $
// Revision 1.4  2007/07/12 17:08:08  bouchet
// add method to decode new ssdNoise Table
//
// Revision 1.3  2007/03/27 23:11:48  bouchet
// Add a method to use the gain calibration for the Charge Matching between pulse of p and n sides
//
// Revision 1.2  2007/03/21 17:20:40  fisyak
// use TGeoHMatrix for coordinate transformation
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.22  2006/09/15 21:03:14  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.21  2006/05/06 00:53:06  fisyak
// Add local coordinate to StEvent
//
// Revision 1.20  2006/01/18 22:49:22  jeromel
// Removed latest change (no time to check new method)
//
// Revision 1.18  2005/12/23 21:33:17  perev
// Some defence for 1/0 added
//
// Revision 1.17  2005/12/20 13:47:14  lmartin
// better hw position encoding in writePointToContainer (matching the new decoding in StEvent)
//
// Revision 1.16  2005/12/20 10:53:17  lmartin
// ReadNoiseFromTable method modified to ignore rows with id=0
//
// Revision 1.15  2005/12/07 20:41:54  perev
// (int) added. WarnOff
//
// Revision 1.14  2005/08/11 13:51:38  lmartin
// PrintStripDetails, PrintPackageDetails and PrintPointDetails methods added
//
// Revision 1.13  2005/08/11 08:13:08  lmartin
// ssdStripCalib table new format included
//
// Revision 1.12  2005/06/24 10:19:38  lmartin
// preventing crashes if ssdStripCalib is missing
//
// Revision 1.11  2005/04/25 14:13:23  bouchet
// new method makeScfCtrlHistograms and makeScmCtrlHistograms and Clusternoise is coded as a float
//
// Revision 1.10  2005/04/23 08:56:20  lmartin
// physics and pedestal data processing separated
//
// Revision 1.9  2005/03/22 13:45:02  lmartin
// new member mActiveLadders added
//
// Revision 1.8  2005/03/22 10:57:18  lmartin
// hardware position information fully implemented
//
// Revision 1.7  2005/03/18 14:57:49  lmartin
// readNoiseFromTable methods modified to transmit the pedestal
//
// Revision 1.6  2005/03/18 14:04:06  lmartin
// missing CVS header added
//

#include "StSsdUtil/StSsdBarrel.hh"

#include "TFile.h"
#include "StEvent.h"
#include "StSsdHit.h"
#include "StSsdHitCollection.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "tables/St_spa_strip_Table.h"
#include "tables/St_ssdPedStrip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdStripCalib_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_sls_strip_Table.h"
#include "tables/St_spa_strip_Table.h"
#include "StSsdUtil/StSsdLadder.hh"
#include "StSsdUtil/StSsdWafer.hh"
#include "StSsdUtil/StSsdClusterControl.h"
#include "StSsdDynamicControl.h"
#include "StSsdStripList.hh"
#include "StSsdStrip.hh"
#include "StSsdUtil/StSsdClusterList.hh"
#include "StSsdUtil/StSsdCluster.hh"
#include "StSsdUtil/StSsdPointList.hh"
#include "StSsdUtil/StSsdPoint.hh"
#include "SystemOfUnits.h"
#include "StarMagField.h"
#include "TMath.h"
#include "StMessMgr.h"

#include "tables/St_ssdGainCalibWafer_Table.h"
#include "tables/St_ssdNoise_Table.h"
StSsdBarrel* StSsdBarrel::fSsdBarrel = 0;
//________________________________________________________________________________
/*!
Constructor using the ssdDimensions_st and ssdConfiguration_st tables from the db
 */
StSsdBarrel::StSsdBarrel(ssdDimensions_st  *dimensions, ssdConfiguration_st *config) : mDebug(0)
{
  memset (first, 0, last-first);
  fSsdBarrel = this;
  setSsdParameters(dimensions);
  LOG_INFO << "Set the Lorentz shift for holes and electrons" << endm;
  setLorentzShift(dimensions);
  if (config) {
    mNLadder         = config[0].nMaxLadders;
    for (Int_t i=0;i<mNLadder;i++) {
      mActiveLadders[i]=1;
      if (config && config[0].ladderIsPresent[i]==0) 
	mActiveLadders[i]=0;
    }
  }
  mLadders = new StSsdLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++){
    mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
    if (Debug()) mLadders[iLad]->SetDebug(Debug());
  }
}
//________________________________________________________________________________
StSsdBarrel::~StSsdBarrel(){for (Int_t iLad = 0 ; iLad < mNLadder; iLad++) delete mLadders[iLad]; fSsdBarrel = 0;}
//________________________________________________________________________________
void StSsdBarrel::setSsdParameters(ssdDimensions_st *geom_par){
  mDimensions          = geom_par;
  mSsdLayer            = 7; // all layers : 1->7
  mDetectorLargeEdge   = 2.*geom_par[0].waferHalfActLength;
  mDetectorSmallEdge   = 2.*geom_par[0].waferHalfActWidth;
  mNLadder             = 20;
  mNWaferPerLadder     = geom_par[0].wafersPerLadder;
  mNStripPerSide       = geom_par[0].stripPerSide;
  mStripPitch          = geom_par[0].stripPitch;
  mTheta               = geom_par[0].stereoAngle;
}
//________________________________________________________________________________
void StSsdBarrel::setLorentzShift(ssdDimensions_st *geom_par){
  Float_t center[3];
  Float_t B[3];
  center[0] = 0.0;
  center[1] = 0.0;
  center[2] = 0.0;
  B[0]      = 0.0;
  B[1]      = 0.0;
  B[2]      = 0.0;
  //we take the BField at the point (0,0,0)
  StarMagField::Instance()->BField(center,B);
#if 0
  Float_t drift_hole   = centimeter2*470;
  Float_t drift_elec   = centimeter2*1417;
  Float_t tan_theta_h  = 0.7*drift_hole*B[2]*0.1;
  Float_t tan_theta_e  = 1.15*drift_elec*B[2]*0.1;
#endif
  // 03/06/2007 : test : use the values from CMS
  Float_t scale                = 1.61;
  Float_t tan_theta_h          = scale*TMath::ATan(TMath::Tan(21*2*TMath::Pi()/360)*(B[2]/40.));
  Float_t tan_theta_e          = scale*TMath::ATan(TMath::Tan( 8*2*TMath::Pi()/360)*(B[2]/40.));
  mShift_hole          = geom_par[0].waferHalfThickness*tan_theta_h;
  mShift_elec          = geom_par[0].waferHalfThickness*tan_theta_e;
  LOG_INFO <<Form("mShift_hole=%f mShift_elec=%f",mShift_hole,mShift_elec)<<endm;
}
//________________________________________________________________________________

void StSsdBarrel::debugUnPeu (Int_t monladder, Int_t monwafer){
  for (Int_t i=0;i<this->getNumberOfLadders();i++)
    {
      if (this->mLadders[i]->getLadderNumb()==monladder) 
	{
	  cout<<" Ladder "<<monladder<<" found"<<endl;
	  this->mLadders[i]->debugUnPeu(monwafer);
	}
    }
}
//________________________________________________________________________________
void StSsdBarrel::initLadders(St_ssdWafersPosition *wafpos) {for (Int_t iLad = 0; iLad < mNLadder; iLad++) mLadders[iLad]->initWafers(wafpos);}
//________________________________________________________________________________
void StSsdBarrel::Reset() {for (Int_t iLad = 0; iLad < mNLadder; iLad++) mLadders[iLad]->Reset();}
//________________________________________________________________________________
Int_t StSsdBarrel::readStripFromTable(St_spa_strip *spa_strip){
  spa_strip_st *strip = spa_strip->GetTable();
  
  Int_t NumberOfStrip = 0;
  Int_t idWaf         = 0;
  Int_t iWaf          = 0;
  Int_t iLad          = 0;
  Int_t nStrip        = 0;
  Int_t iSide         = 0;
  Float_t sigma       = 3.;
  Int_t iPedestal     = 100;
  Int_t idMcHit[5]    = {0,0,0,0,0};
  Int_t e = 0;
  for (Int_t i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf    = idWaferToWafer(idWaf);
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSsdStrip *newStrip = new StSsdStrip(nStrip, strip[i].adc_count, sigma, iPedestal, idMcHit);
      mLadders[iLad]->mWafers[iWaf]->addStrip(newStrip, iSide);
    }
  NumberOfStrip = spa_strip->GetNRows();  
  return NumberOfStrip;
}
//________________________________________________________________________________
Int_t  StSsdBarrel::writeNoiseToFile(St_spa_strip *spa_strip){
  spa_strip_st *strip = spa_strip->GetTable();
  St_ssdStripCalib *stripCal = new St_ssdStripCalib("ssdStripCalib",spa_strip->GetNRows());
  ssdStripCalib_st noise_strip;
  for (Int_t i = 0 ; i < spa_strip->GetNRows(); i++)
    {
      noise_strip.id=strip[i].id_strip;
      noise_strip.pedestals=(unsigned char) strip[i].id_mchit[0];
      noise_strip.rms=(unsigned char) strip[i].adc_count;
      stripCal->AddAt(&noise_strip);
    }
  TFile f1("ssdStripCalib.root","NEW");
  stripCal->Write();
  f1.Close();
  return spa_strip->GetNRows();
}
//-----------------------------------------------------------------------------------------
Int_t  StSsdBarrel::writeNewNoiseToFile3(St_ssdPedStrip  *spa_ped_strip, char myLabel[]){
  char *name =new char[100] ;
  ssdPedStrip_st *strip = spa_ped_strip->GetTable();
  //print("size is %ld\n",spa_ped_strip->GetNRows());
  St_ssdNoise *StripCal = new St_ssdNoise("ssdNoise",spa_ped_strip->GetNRows());
  ssdNoise_st  temp[320];
  memset(temp, 0, 320*sizeof(ssdNoise_st));
  Int_t idWaf  = 0;
  Int_t iWaf   = 0;
  Int_t iLad   = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t wafer  = 0;
  Int_t N = spa_ped_strip->GetNRows();
  for (Int_t i=0; i< N;i++) {
      if (strip[i].id_strip>0 && strip[i].id_strip<=76818620) {
	//cout << "id = "<<strip[i].id_strip << endl;
	nStrip  = (int)(strip[i].id_strip/100000.);
	idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
	iWaf    = (int)((idWaf - mSsdLayer*1000)/100 - 1);
	iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
	wafer = iLad*mNWaferPerLadder +iWaf;
	//if((idWaf==8009)&&(iSide==0)) cout << "ladder= " << iLad <<" wafer =" << iWaf << " Strip= " << nStrip <<" id =" << strip[i].id_strip  << " noise = " << strip[i].noise << endl; 
	if (iSide == 0) temp[wafer].rmsp[nStrip-1] = (unsigned char)strip[i].noise;
	if (iSide == 1) temp[wafer].rmsn[nStrip-1] = (unsigned char)strip[i].noise;
      }
  }
  for(Int_t i=0;i<320;i++) {
    temp[i].id = i;
    StripCal->AddAt(&temp[i]);
  }
  //printf("Number of entries seen p=%d and n=%d \n",NumberofStripsp,NumberofStripsn);
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
Int_t  StSsdBarrel::writeNoiseToFile(St_ssdPedStrip *spa_ped_strip, char myLabel[]){ 
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
Int_t  StSsdBarrel::readNoiseFromTable(St_sdm_calib_db *spa_noise, StSsdDynamicControl *dynamicControl){
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
      iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (noise[i].id_strip - nStrip*100000 - idWaf)/10000;
      mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip, iSide,0, noise[i].n_sigma, dynamicControl);
    }

  NumberOfNoise = spa_noise->GetNRows();
  return NumberOfNoise;
//   return noise_h->nok;
}
//________________________________________________________________________________
/*!
New method reading from the ssdStripCalib table
 */
Int_t  StSsdBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib, StSsdDynamicControl *dynamicControl){
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
	iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	//if(idWaf==7101)printf("id=%d stripId=%d  side=%d  waferId=%d  pedestal=%d  noise=%d\n",noise[i].id,nStrip,iSide,idWaf,noise[i].pedestals,noise[i].rms);
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip, iSide, noise[i].pedestals, noise[i].rms, dynamicControl);
	NumberOfNoise++;
      }
    }
  return NumberOfNoise;
}

//________________________________________________________________________________
/*!
first method reading from the ssdNoise table
 */
Int_t  StSsdBarrel::readNoiseFromTable(St_ssdNoise *strip_noise, StSsdDynamicControl *dynamicControl){
  ssdNoise_st *noise = strip_noise->GetTable();
  
  Int_t NumberOfNoise = 0;
  Int_t iWaf          = 0;
  Int_t iLad          = 0;
  Int_t nStrip        = 0;
  Int_t iSide         = 0;
  Int_t pedestal      = 150;//constant, not used later
  printf("size of m_noise3 table = %d\n",(int)strip_noise->GetNRows());
  for (Int_t i = 0 ; i < strip_noise->GetNRows(); i++)
    {
      iWaf      = noise[i].id-(noise[i].id/mNWaferPerLadder)*mNWaferPerLadder;
      iLad      = noise[i].id/16;
      for(nStrip=0;nStrip<mNStripPerSide;nStrip++){
	iSide=0;
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip+1, iSide, pedestal, noise[i].rmsp[nStrip], dynamicControl);
	//printf("iSide=%d i=%d wafer=%d ladder=%d nStrip=%d\n",iSide,i,iWaf,iLad,nStrip);
	NumberOfNoise++;
	iSide=1;
	mLadders[iLad]->mWafers[iWaf]->setPedestalSigmaStrip(nStrip+1, iSide, pedestal, noise[i].rmsn[nStrip], dynamicControl);
	//printf("iSide=%d i=%d wafer=%d ladder=%d nStrip=%d\n",iSide,i,iWaf,iLad,nStrip);
	NumberOfNoise++;
      }
    }
  return NumberOfNoise;
}
//________________________________________________________________________________
Int_t StSsdBarrel::readClusterFromTable(St_scf_cluster *scf_cluster){
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
//Int_t nNoiseCount     = 0;
  Float_t nStripMean    = 0;
  Int_t nFlag           = 0;

  for (Int_t i = 0 ; i < scf_cluster->GetNRows(); i++)
    {
      nCluster    = (int)(cluster[i].id_cluster/100000.);
      idWaf       = (cluster[i].id_cluster-10000*((int)(cluster[i].id_cluster/10000.)));
      iSide       = (cluster[i].id_cluster-idWaf-nCluster*100000)/10000;
      iWaf        = idWaferToWafer(idWaf);
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
//________________________________________________________________________________
Int_t  StSsdBarrel::writeClusterToTable(St_scf_cluster *scf_cluster){
  scf_cluster_st cluster;
  Int_t currRecord  = 0;
  Int_t i           = 0;

  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
      {
	Int_t idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
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
	    cluster.noise_count     = (int)pClusterP->getTotNoise();
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
Int_t  StSsdBarrel::writeClusterToTable(St_scf_cluster *scf_cluster,St_spa_strip *spa_strip){
  scf_cluster_st cluster;
  spa_strip_st *on_strip = spa_strip->GetTable(); 
  Int_t currRecord  = 0;
  Int_t i           = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
      {
	//Int_t idCurrentWaf = waferNumbToIdWafer(iWaf);
	Int_t idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
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
		      //printf("ok, found idMcTrack=%d  i=%d   j=%d\n",on_strip[j].id_mctrack[i],i,j);
		    }
		  }
		}
	      }
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
		      //printf("ok, found idMcTrack=%d  i=%d   j=%d\n",on_strip[j].id_mctrack[i],i,j);
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
//________________________________________________________________________________
Int_t StSsdBarrel::writePointToContainer(St_scm_spt *scm_spt, StSsdHitCollection* ssdHitColl){
  scm_spt_st spt;
  StSsdHit *currentSsdHit; 
  // table size is 148 bytes
  Int_t i = 0, inContainer = 0, inTable = 0;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  Int_t hw; Float_t q ; unsigned char c; 

  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	Int_t idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSsdPointList *sptList = mLadders[iLad]->mWafers[iWaf]->getPoint();
	StSsdPoint *pSpt = sptList->first();
     
	while (pSpt){// Start of Point Loop
	  if (ssdHitColl){ // If Available, Fill the StEvent Container
	    for (i = 0 ; i < 3 ; i++){
	      gPos[i]      =  pSpt->getXg(i);
	      gPosError[i] =  0.0; 
	    }
	    hw = idCurrentWaf;
	    q =  pSpt->getDe(0);
	     
	    currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);
	    currentSsdHit->setIdTruth(pSpt->getNMchit(0));// need to check first = most probable!

	    currentSsdHit->setHardwarePosition(8+16*idWaferToWaferNumb(idCurrentWaf));
	    currentSsdHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1));
	    //looking for the correct clusters...
	    Int_t Id_P_Side = pSpt->getIdClusterP();
	    Int_t Id_N_Side = pSpt->getIdClusterN();

            StSsdClusterList *currentListP_j = mLadders[iLad]->mWafers[iWaf]->getClusterP();
            StSsdCluster     *cluster_P_j   = currentListP_j->first();
            while(cluster_P_j)
	    {
	      if(cluster_P_j->getNCluster()==Id_P_Side) 
                break;
              cluster_P_j = currentListP_j->next(cluster_P_j);
	    }


            StSsdClusterList *currentListN_j = mLadders[iLad]->mWafers[iWaf]->getClusterN();
            StSsdCluster *cluster_N_j       = currentListN_j->first();
            while(cluster_N_j)
	    {
	      if(cluster_N_j->getNCluster()==Id_N_Side) 
		break;
	      cluster_N_j = currentListN_j->next(cluster_N_j);
	    }

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
  	    currentSsdHit->setHardwarePosition(hw);

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

/***********************************************************/
Int_t StSsdBarrel::writePointToContainer(St_scm_spt *scm_spt, StSsdHitCollection* ssdHitColl,St_scf_cluster *scf_cluster){
  scm_spt_st spt;
  StSsdHit *currentSsdHit;
  
  //scf_cluster_st *on_cluster = scf_cluster->GetTable(); 
  scf_cluster_st *on_cluster = scf_cluster->GetTable(); 
  // table size is 148 bytes
  Int_t i = 0, inContainer = 0, inTable = 0 ;
  StThreeVectorF gPos; StThreeVectorF gPosError; 
  Int_t hw; Float_t q ; unsigned char c; 

  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	Int_t idCurrentWaf = mSsdLayer*1000 + (iWaf+1)*100 + (iLad+1);
	StSsdPointList *sptList = mLadders[iLad]->mWafers[iWaf]->getPoint();
	StSsdPoint *pSpt = sptList->first();
     
	while (pSpt){
	  //jb : we fill StEvent after getting the IdMctrack
	  //jb : as it was done too for the strip and clusters --> see StSpaBarrel.cc and StScfBarrel.cc
	  //printf("Now we find the idMcTrack from the cluster\n");
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
		    //printf("ok, found idMcTrack=%d  i=%d   j=%d\n",on_cluster[j].id_mctrack[i],i,j);
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
	    currentSsdHit = new StSsdHit(gPos,gPosError,hw,q,c);
	    currentSsdHit->setIdTruth(spt.id_mctrack[0],100);// need to check first = most probable!
	    // Start of Point Loop
	    
	    //currentSsdHit->setHardwarePosition(8+16*idWaferToWaferNumb(idCurrentWaf));
	    //currentSsdHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1));
	    
	    //looking for the correct clusters...
	    Int_t Id_P_Side = pSpt->getIdClusterP();
	    Int_t Id_N_Side = pSpt->getIdClusterN();
	    
            StSsdClusterList *currentListP_j = mLadders[iLad]->mWafers[iWaf]->getClusterP();
            StSsdCluster     *cluster_P_j   = currentListP_j->first();
            while(cluster_P_j)
	    {
	      if(cluster_P_j->getNCluster()==Id_P_Side) 
                break;
              cluster_P_j = currentListP_j->next(cluster_P_j);
	    }


            StSsdClusterList *currentListN_j = mLadders[iLad]->mWafers[iWaf]->getClusterN();
            StSsdCluster *cluster_N_j       = currentListN_j->first();
            while(cluster_N_j)
	    {
	      if(cluster_N_j->getNCluster()==Id_N_Side) 
		break;
	      cluster_N_j = currentListN_j->next(cluster_N_j);
	    }

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
  	    currentSsdHit->setHardwarePosition(hw);
	    currentSsdHit->setLocalPosition(pSpt->getXl(0),pSpt->getXl(1));
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
//________________________________________________________________________________
void StSsdBarrel::doSideClusterisation(Int_t *barrelNumbOfCluster){
  //  Int_t *wafNumbOfCluster = new int[2];
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
  //  delete[] wafNumbOfCluster;
}
//________________________________________________________________________________
Int_t StSsdBarrel::doClusterMatching(Float_t *CalibArray){
  Int_t NumberOfPackage = 0;
  Int_t nSolved         = 0;
  Int_t nPerfect        = 0;
  
  //for(Int_t i=0 ; i<mNLadder*mNWaferPerLadder;i++) {printf("i=%d gain=%f\n",i,CalibArray[i]);}
  for (Int_t iLad = 0; iLad < mNLadder; iLad++){
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      { 
	mLadders[iLad]->mWafers[iWaf]->doLorentzShift(mDimensions,mShift_hole,mShift_elec);
	NumberOfPackage += mLadders[iLad]->mWafers[iWaf]->doFindPackage(mDimensions, mClusterControl);
	nPerfect  =  mLadders[iLad]->mWafers[iWaf]->doSolvePerfect(mDimensions, mClusterControl,CalibArray[(iLad*mNWaferPerLadder)+iWaf]);
	if (!nPerfect) continue;
	mLadders[iLad]->mWafers[iWaf]->doStatPerfect(nPerfect, mClusterControl);
	nSolved  += mLadders[iLad]->mWafers[iWaf]->doSolvePackage(mDimensions, mClusterControl,CalibArray[(iLad*mNWaferPerLadder)+iWaf]);
      }
  }
  cout<<"****       Remark: "<<nSolved<<"  solved packages     ****\n";
  return NumberOfPackage;
}
//________________________________________________________________________________
void StSsdBarrel::convertDigitToAnalog(StSsdDynamicControl *dynamicControl){
  long   nElectronInAMip    = dynamicControl->getnElectronInAMip();
  long   adcDynamic         = dynamicControl->getadcDynamic();
  Double_t pairCreationEnergy = dynamicControl->getpairCreationEnergy();

  const Int_t NAdcChannel     = (int)pow(2.0,dynamicControl->getnbitEncoding());
  const Double_t convFactor   = (pairCreationEnergy*adcDynamic*nElectronInAMip)/NAdcChannel;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      mLadders[iLad]->mWafers[iWaf]->convertDigitToAnalog(convFactor);
}
//________________________________________________________________________________
void StSsdBarrel::convertUFrameToOther(){
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->convertUFrameToLocal(mDimensions);
	mLadders[iLad]->mWafers[iWaf]->convertLocalToGlobal();
      }
}
//________________________________________________________________________________
void StSsdBarrel::convertGlobalFrameToOther(){
  for (Int_t iLad = 0; iLad < mNLadder; iLad++){
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)
      {
	mLadders[iLad]->mWafers[iWaf]->convertGlobalToLocal();
	mLadders[iLad]->mWafers[iWaf]->convertLocalToUFrame(mDetectorLargeEdge, mDetectorSmallEdge, mTheta);
      }
  }
}
//________________________________________________________________________________
void StSsdBarrel::sortListStrip(){
  StSsdStripList *currentList = 0;
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
void StSsdBarrel::sortListCluster(){
  StSsdClusterList *currentList = 0;
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

StSsdBarrel::StSsdBarrel(const StSsdBarrel & originalBarrel)
{
  mSsdLayer             = originalBarrel.mSsdLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;
  
  mLadders = new StSsdLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
}

StSsdBarrel& StSsdBarrel::operator=(const StSsdBarrel  originalBarrel)
{
  mSsdLayer             = originalBarrel.mSsdLayer;
  mNLadder              = originalBarrel.mNLadder;
  mNWaferPerLadder      = originalBarrel.mNWaferPerLadder;
  mNStripPerSide        = originalBarrel.mNStripPerSide;

  mLadders = new StSsdLadder*[mNLadder];
  for (Int_t iLad=0; iLad < mNLadder; iLad++)
    mLadders[iLad] = new StSsdLadder(iLad,mSsdLayer,mNWaferPerLadder,mNStripPerSide);
  return *this;
}

Int_t StSsdBarrel::idWaferToWaferNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return ((iL-1)*mNWaferPerLadder + iW -1);
}

Int_t StSsdBarrel::idWaferToLadderNumb(Int_t idWafer)
{
  // idwafer = layer*1000+waf*100+ladder
  Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
  Int_t iL = idWafer - mSsdLayer*1000 - iW*100;
  return iL-1;
}


Int_t StSsdBarrel::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  Int_t iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  return mSsdLayer*1000 + iW*100 + iL;
}

Int_t StSsdBarrel::isActiveLadder(Int_t iLadder)
{
  return mActiveLadders[iLadder];
}
void StSsdBarrel::renumHitAfterRemove() {
  Int_t iLast = 0;
  Int_t iNewLast = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      iNewLast = mLadders[iLad]->mWafers[iWaf]->getPoint()->renumHits(iLast);
      iLast = iNewLast;
    }
}
//________________________________________________________________________________
StSsdPointList *StSsdBarrel::getInactiveHitList() {
  StSsdPointList *inactiveHits = new StSsdPointList();
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      //fill the list of  hits in inactive wafer areas
      StSsdPointList *currDeadList = mLadders[iLad]->mWafers[iWaf]->getDeadHits(mDetectorLargeEdge, mDetectorSmallEdge, mStripPitch);
      if(currDeadList->getSize()>0){
      }
      inactiveHits = inactiveHits->addPointList(currDeadList);
      delete currDeadList;
    }
  inactiveHits->sortPoint();
  return inactiveHits;
}
//________________________________________________________________________________
void StSsdBarrel::convertToStrip(Double_t pairCreationEnergy,
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
void  StSsdBarrel::addNoiseToStrip(slsCtrl_st *ctrl)
{
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++) {
      StSsdWafer *wafer = mLadders[iLad]->mWafers[iWaf];
      wafer->sortNoise();
      wafer->sortStrip();
      wafer->addNoiseToStripSignal(ctrl[0].nElectronInAMip,ctrl[0].a128Dynamic);
    }
}
//________________________________________________________________________________
Int_t StSsdBarrel::readStripFromTable(St_sls_strip *sls_strip)
{
  sls_strip_st *strip = sls_strip->GetTable();

  Int_t idWaf  = 0, iWaf = 0, iLad = 0;
  Int_t nStrip = 0;
  Int_t iSide  = 0;
  Int_t i = 0;
  //  Int_t *idMcHit = new int[5];
  Int_t idMcHit[5] = {0,0,0,0,0};
  Int_t e  = 0;
  Int_t my_counterP =0;
  Int_t my_counterN =0;
  for (i = 0 ; i < sls_strip->GetNRows(); i++)
    {
      nStrip  = (int)(strip[i].id_strip/100000.);
      idWaf   = strip[i].id_strip-10000*((int)(strip[i].id_strip/10000.));
      iWaf = (int)((idWaf - mSsdLayer*1000)/100)-1;
      iLad = idWaf - mSsdLayer*1000 - (iWaf+1)*100-1;
      //iWaf    = idWaferToWaferNumb(idWaf);
      //iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
      iSide   = (strip[i].id_strip - nStrip*100000 - idWaf)/10000;
      if(iSide==0) my_counterP++;
      if(iSide==1) my_counterN++;
      for (e = 0 ; e < 5;e++) idMcHit[e] = strip[i].id_mchit[e];
      StSsdStrip *newStrip = new StSsdStrip(nStrip, i, strip[i].adc_count, strip[i].de, idMcHit);
      mLadders[iLad]->mWafers[iWaf]->addStrip(newStrip, iSide);
    }
  //  delete [] idMcHit;
  return sls_strip->GetNRows(); 
}
//________________________________________________________________________________
Int_t  StSsdBarrel::readNoiseFromTable(St_ssdStripCalib *strip_calib)
{
  ssdStripCalib_st *noise = strip_calib->GetTable();
  Int_t NAdcChannel          = (int)pow(2.0,10.0*1.0);
  Int_t nElectronInAMip      = 22500;
  Int_t adcDynamic           = 20;
  const Float_t   AdctoE     =  (adcDynamic*nElectronInAMip)/(float)(NAdcChannel);
  printf("AdctoE = %f\n",AdctoE);
  
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
	iWaf = (int)((idWaf - mSsdLayer*1000)/100)-1;
	iLad = idWaf - mSsdLayer*1000 - (iWaf+1)*100-1;
	iSide   = (noise[i].id - nStrip*100000 - idWaf)/10000;
	//if((idWaf==7101)&&(iSide==0))printf("id=%d stripId=%d  side=%d  waferId=%d  pedestal=%d  noise=%d\n",noise[i].id,nStrip,iSide,idWaf,noise[i].pedestals,noise[i].rms);
	StSpaNoise *newStrip = new StSpaNoise(nStrip ,(int)(noise[i].pedestals*AdctoE),(int)((noise[i].rms*AdctoE)/16.));
	//	mLadders[iLad]->mWafers[iWaf]->addNoise(newStrip,iSide);
	mLadders[iLad]->mWafers[iWaf]->addNoiseToStripSignal(newStrip,iSide);
	ent++;
      }
    }
  printf("Entries = %d\n",ent);
  return ent;
}
//________________________________________________________________________________
Int_t  StSsdBarrel::readConditionDbFromTable(St_sdm_condition_db *sdm_condition)
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
	  iLad    = (int)(idWaf - mSsdLayer*1000 - (iWaf+1)*100 - 1);
	  iSide   = (condition[i].id_strip - nStrip*100000 - idWaf)/10000;
	  mLadders[iLad]->mWafers[iWaf]->setIsActive(condition[i].is_active, iSide, nStrip);
	}
    }
  return sdm_condition->GetNRows();
}
//________________________________________________________________________________
Int_t  StSsdBarrel::writeStripToTable(St_spa_strip *spa_strip)
{
  spa_strip_st out_strip;
  
  Int_t currRecord   = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
    {
      //Int_t idCurrentWaf = waferNumbToIdWafer(iWaf);
      Int_t idCurrentWaf = mSsdLayer*1000 +((iWaf+1)*100)+(iLad+1);
      StSsdStripList *stripP = mLadders[iLad]->mWafers[iWaf]->getStripP();
      StSsdStripList *stripN = mLadders[iLad]->mWafers[iWaf]->getStripN();
					  
      StSsdStrip *pStripP = stripP->first();
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
      
      StSsdStrip *pStripN = stripN->first();
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
Int_t  StSsdBarrel::writeStripToTable(St_spa_strip *spa_strip,St_sls_strip *sls_strip )
{
  spa_strip_st out_strip;
  sls_strip_st *strip = sls_strip->GetTable();
  Int_t currRecord   = 0;
  for (Int_t iLad = 0; iLad < mNLadder; iLad++)
    for (Int_t iWaf = 0; iWaf < mNWaferPerLadder ; iWaf++)
    {
      //Int_t idCurrentWaf = waferNumbToIdWafer(iWaf);
      Int_t idCurrentWaf = mSsdLayer*1000 +((iWaf+1)*100)+(iLad+1);
      StSsdStripList *stripP = mLadders[iLad]->mWafers[iWaf]->getStripP();
      StSsdStripList *stripN = mLadders[iLad]->mWafers[iWaf]->getStripN();
      StSsdStrip *pStripP = stripP->first();
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
      
      StSsdStrip *pStripN = stripN->first();
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
void  StSsdBarrel::doDaqSimulation(slsCtrl_st *ctrl){
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
