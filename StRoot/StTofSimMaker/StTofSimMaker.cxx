/***************************************************************************
 *
 * $Id: StTofSimMaker.cxx,v 1.13 2018/02/26 23:26:51 smirnovd Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: StTofSimMaker class for TOFp Simulations
 *
 **************************************************************************/
//! Time-of-Flight Fast Simulator Maker
/*! \class StTofSimMaker
    \author Frank Geurts

    <p>TOF simulation software. This Maker further processes the simulated
    detector response from GSTAR's GEANT simulation. It takes the G2T tof
    hit tables and build an StEvent Tof SlatCollection.</p>
*/
#include <Stiostream.h>
#include "StTofSimMaker.h"

// SCL
#include "StThreeVectorD.hh"
#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"
#include "TH1.h"
#include "TFile.h"

#include "StTofUtil/StTofCalibration.h"
#include "StTofUtil/StTofSimParam.h"
#include "StTofUtil/StTofGeometry.h"
#include "StEventTypes.h"

// g2t tables and collections
#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_g2t_vpd_hit_Table.h"
//#include "tables/St_g2t_track_Table.h"
//#include "tables/St_g2t_tpc_hit_Table.h"
#include "StTofUtil/StTofDataCollection.h"
#include "StTofUtil/StTofSlatCollection.h"
#include "StTofMCSlat.h"

typedef vector<StTofMCSlat> tofMCSlatVector;
typedef tofMCSlatVector::iterator tofMCSlatVecIter;


static RanluxEngine engine;


/// default constructor
StTofSimMaker::StTofSimMaker(const char *name):StMaker(name){
  mGeomDb  = 0;
  mCalibDb = 0;
  mSimDb   = 0;
}

/// default empty destructor
StTofSimMaker::~StTofSimMaker(){/* nope */}


/// Initialize dBase interfaces and book histograms
Int_t StTofSimMaker::Init(){
  //mGeomDb = new StTofGeometry();
  //mGeomDb->init(this);
  mCalibDb = new StTofCalibration();
  mCalibDb->init();
  mSimDb = new StTofSimParam();
  mSimDb->init();
  
  if (m_Mode){
    // book histograms
    mdE = new TH1F("energy deposition","dE",100,0.,0.011);
    mdS = new TH1F("distance","ds",100,0.,10);
    mNumberOfPhotoelectrons = new TH1F("number of photoelectrons","nphe",1000,0,5000);
    mT = new TH1F("delay corrected tof","time",100,0.,12e-7);
    mTime = new TH1F("only hit-pos resolution added","tt",100,0.,12e-7);
    mTime1 = new TH1F("fully corrected tof","tt1",100,0.,120e-7);
    mPMlength = new TH1F("distance in slat","length",100,0,22);
    mAdc = new TH1F("adc","adc",1025,-0.5,1024.5);
    mTdc = new TH1F("tdc","tdc",2049,-0.5,2048.5);
  }

  return StMaker::Init();
}



/// InitRun method, (re)initialize TOFp data from STAR dBase
Int_t StTofSimMaker::InitRun(int runnumber){
  LOG_INFO << "StTofSimMaker::InitRun  -- initializing TofGeometry --" << endm;
  mGeomDb = new StTofGeometry();
  mGeomDb->init(this);
  return kStOK;
}



/// FinishRun method, clean up TOFp dBase entries
Int_t StTofSimMaker::FinishRun(int runnumber){
  LOG_INFO << "StTofSimMaker::FinishRun -- cleaning up TofGeometry --" << endm;
  if (mGeomDb) delete mGeomDb;
  mGeomDb=0;
  return 0;
}



/// read in GSTAR table and create TOF SlatCollection
Int_t StTofSimMaker::Make(){
  LOG_INFO << "StTofSimMaker  Make() starts" << endm;

  StTofSlatCollection *mSlatCollection =  new StTofSlatCollection;
  tofMCSlatVector tofMC;

  // Check for GEANT data
  St_DataSet *geantData = GetInputDS("geant");
  if (geantData) {
    St_DataSetIter geantIter(geantData);

    // TOFp hits
    St_g2t_ctf_hit *g2t_tof_hit = (St_g2t_ctf_hit*) geantIter("g2t_tof_hit");
    if (g2t_tof_hit){
      g2t_ctf_hit_st* tof_hit = g2t_tof_hit->GetTable();
      int numberOfTofHits = g2t_tof_hit->GetNRows();
      LOG_INFO << "TOF #hits: " << numberOfTofHits << endm;
      
      tofMCSlatVector MCSlatVec;
      MCSlatVec.clear();
      for (int i=0;i<numberOfTofHits;i++,tof_hit++){
	MCSlatVec.push_back(detectorResponse(tof_hit));
      }

      // build ... from MCSlatVec which may have entries with same slatId
      tofMCSlatVector slatTempVec = MCSlatVec;
      tofMCSlatVector slatErasedVec = slatTempVec;
      tofMCSlatVecIter slatTempIter, slatErasedIter;
	
      while (slatTempVec.size()!=0){
	unsigned short fastTdc;
	int nFired=0, accumNPhe=0;
	float accumDe=0., accumDs=0., fastTof=0.;
	slatTempIter=slatTempVec.begin();
	slatErasedIter=slatErasedVec.begin();
	fastTof = slatTempIter->mcInfo().mTof;
	fastTdc = slatTempIter->tdc();
	    
	while(slatErasedIter!=slatErasedVec.end()){
	  if(slatTempIter->slatIndex() == slatErasedIter->slatIndex()){
	    nFired++;
	    accumDe += slatErasedIter->mcInfo().mDe;
	    accumDs += slatErasedIter->mcInfo().mDs;
	    accumNPhe += slatErasedIter->mcInfo().mNPhe;
	    fastTof = min(fastTof, slatErasedIter->mcInfo().mTof);
	    fastTdc = min(fastTdc, slatErasedIter->tdc());
	      
	    slatErasedVec.erase(slatErasedIter);
	    slatErasedIter--;
	  }
	  slatErasedIter++;
	}
	StTofMCSlat MCSlat = *slatTempIter;
	MCSlat.setNHits(nFired);
	MCSlat.setNPhe(accumNPhe);
	MCSlat.setDe(accumDe);
	MCSlat.setDs(accumDs);
	MCSlat.setTof(fastTof);
	MCSlat.setTdc(fastTdc);
	tofMC.push_back(MCSlat);
	
	slatTempVec = slatErasedVec;
      }
      LOG_INFO << "StTofSimMaker::make()  vector size from " << MCSlatVec.size()
	   << " to " << tofMC.size() << endm;
	
      //////
	
      fillRaw();
      if (mSimDb->elec_noise() < 0)  electronicNoise();
      fillEvent();
      for (unsigned int i=0;i<tofMC.size(); i++){
	StTofMCSlat *MCSlatPtr = new StTofMCSlat();
	*MCSlatPtr = tofMC[i];
	//LOG_INFO << *MCSlatPtr ;
	mSlatCollection->push_back(MCSlatPtr);
      }
    }
    else
      LOG_INFO << "StTofSimMaker Make()  no TOF hits found" << endm;

    // pVPD section hits
    St_g2t_vpd_hit *g2t_vpd_hit = (St_g2t_vpd_hit*) geantIter("g2t_vpd_hit");
    if (g2t_vpd_hit){
      //   g2t_vpd_hit_st* vpd_hit = g2t_vpd_hit->GetTable();
      int numberOfVpdHits = g2t_vpd_hit->GetNRows();
      LOG_INFO << "VPD #hits: " << numberOfVpdHits << endm;
    }
    else
      LOG_INFO << "StTofSimMaker Make()  no VPD hits found" << endm;    
  }


  // send off to StEvent
  StTofCollection *mTheTofCollection = new StTofCollection();
  for (size_t j=0;j<mSlatCollection->size();j++){
    mTheTofCollection->addSlat(dynamic_cast<StTofMCSlat*>(mSlatCollection->getSlat(j))); 
    // mTheTofCollection->addSlat(mSlatCollection->getSlat(j)); 
}

  // create tofData collection
  StTofDataCollection *mDataCollection = new StTofDataCollection;
  for (int i=0;i<48;i++){    
    bool slatFound = false;
    int j=0;
    for (j=0;j<(int)mSlatCollection->size();j++){
      StTofSlat *tempSlat = mSlatCollection->getSlat(j);
      unsigned short indexSlat = tempSlat->slatIndex();
      if (indexSlat == mGeomDb->daqToSlatId(i)){
	slatFound = true;
	//	StTofData *rawTofData = new  StTofData(indexSlat,tempSlat->adc(),tempSlat->tdc(),0,0);
	// update for year 5 new format
	StTofData *rawTofData = new  StTofData(indexSlat,tempSlat->adc(),tempSlat->tdc(),0,0,0,0);
	if (Debug()) LOG_INFO << indexSlat << ":  A" << tempSlat->adc() << "  T" << tempSlat->tdc() << endm;
	mDataCollection->push_back(rawTofData);
      }
    }
    if (!slatFound){
      //      StTofData *rawTofData = new  StTofData(mGeomDb->daqToSlatId(i),0,0,0,0);
      // update for year 5 new format
      StTofData *rawTofData = new  StTofData(mGeomDb->daqToSlatId(i),0,0,0,0,0,0);
      mDataCollection->push_back(rawTofData);
    }
  }
  for(size_t jj = 0; jj < mDataCollection->size(); jj++)
    mTheTofCollection->addData(mDataCollection->getData(jj));

  mEvent = (StEvent*) GetInputDS("StEvent");
  if (mEvent) mEvent->setTofCollection(mTheTofCollection);
  else{
    LOG_INFO << "StTofSimMaker: Where is StEvent !?! Unable to store data" << endm;
    return kStWarn;
  }


  // verify existence of tofCollection in StEvent (mEvent) 
  LOG_INFO << "StTofSimMaker: verifying TOF StEvent data ..." << endm;
  StTofCollection *mmTheTofCollection = mEvent->tofCollection();
  if(mmTheTofCollection) {
    LOG_INFO << " + StEvent tofCollection Exists" << endm;
    if(mmTheTofCollection->slatsPresent())
      LOG_INFO << " + StEvent TofSlatCollection Exists" << endm;
    else
      LOG_INFO << " - StEvent TofSlatCollection DOES NOT Exist" << endm;
    if(mmTheTofCollection->hitsPresent())
      LOG_INFO << " + StEvent TofHitCollection Exists" << endm;
    else
      LOG_INFO << " - StEvent TofHitCollection DOES NOT Exist" << endm;
  }
  else {
    LOG_INFO << " - StEvent tofCollection DOES NOT Exist" << endm;
    LOG_INFO << " - StEvent TofSlatCollection DOES NOT Exist" << endm;
    LOG_INFO << " - StEvent TofHitCollection DOES NOT Exist" << endm;
  }

  LOG_INFO << "StTofSimMaker  Make() finished" << endm;
  return kStOK;
}


/// calculate detector response for a single hit
StTofMCSlat StTofSimMaker::detectorResponse(g2t_ctf_hit_st* tof_hit)
{
  if(Debug()){
    // dump the g2t structure ...
    LOG_INFO << " " <<setw( 3) << tof_hit->id      << " " <<setw( 4) << tof_hit->next_tr_hit_p
	 << " " <<setw( 4) << tof_hit->track_p << " " <<setw( 8) << tof_hit->volume_id
	 << " " <<setw(13) << tof_hit->de      << " " <<setw(11) << tof_hit->ds
	 << " " <<setw(12) << tof_hit->p[0]    << " " <<setw(12) << tof_hit->p[1]
	 << " " <<setw(12) << tof_hit->p[2]    << " " <<setw( 7) << tof_hit->s_track
	 << " " <<setw(13) << tof_hit->tof     << " " <<setw(10) << tof_hit->x[0]
	 << " " <<setw(10) << tof_hit->x[1]    << " " <<setw(10) << tof_hit->x[2]
	 << endm;
  }
    // skip the consistency checks for now,

    // determine eta and phi indices from hitpoint and slatId
    StThreeVectorD hitPoint  = StThreeVectorD(tof_hit->x[0],
					      tof_hit->x[1],
					      tof_hit->x[2]);
    //fg    StTofCross*  mTofCross = new StTofCross;
    int slatId= (int) mGeomDb->tofSlatCrossId(hitPoint);
    int volId = (int) mGeomDb->tofSlatCrossId(tof_hit->volume_id);

    if (slatId != volId){
      LOG_INFO << "StTofSimMaker::Make  Warning: volume_id ("<< volId 
           << ") and hit ("<<slatId<<") inconsistent. Switching to volumeid."<< endm;
      slatId=volId;
    }

    // retrieve dbase parameters
    float zmin = mGeomDb->tofSlat(slatId).z_min;
    float zmax = mGeomDb->tofSlat(slatId).z_max;
    float cosang  = mGeomDb->tofSlat(slatId).cosang;
    
    float length = (zmax-tof_hit->x[2])/cosang ;
    float max_distance = (zmax-zmin)/cosang ;
	
    if (length>max_distance || length<0){
      LOG_INFO <<  "StTofSimMaker:  length="<<length<<" max="<<max_distance
	   << " zmin="<<zmin<<" zmax="<<zmax<<" coasng="<<cosang<<endm;
      mGeomDb->printSlat(slatId);
    }

	
    // do the slat response modelling similar to cts
    long numberOfPhotoelectrons;
    if (mSimDb->slat_para()){
      LOG_INFO << "StTofSimMaker  Slat Response Table not implemented yet. "
      << " Switching to exponential model instead" <<endm;
    }
    //numberOfPhotoelectrons = long (tof_hit->de * slatResponseExp(length));
    numberOfPhotoelectrons = long (tof_hit->de * mSimDb->GeV_2_n_photons()
                               * mSimDb->cath_eff() * mSimDb->cath_surf() * mSimDb->surf_loss());

    // prepare some random generator stuff
    RandGauss random(engine);

    // calculate TOFs with all kinds of resolutions
    float time= tof_hit->tof + length*mSimDb->delay();
    float resl=  mSimDb->time_res() * ::sqrt(length);
    if (resl<50e-12) resl=50e-12;
    float tt =  tof_hit->tof + (float) random.shoot()* resl;
    float tt1 =  time +  (float) random.shoot()* mSimDb->start_res();

    // fill the histograms
    if(m_Mode){
      mPMlength->Fill(length);
      mdE->Fill(tof_hit->de);
      mdS->Fill(tof_hit->ds);
      mNumberOfPhotoelectrons->Fill(numberOfPhotoelectrons);
      mT->Fill(time);
      mTime->Fill(tt);
      mTime1->Fill(tt1);
    }

    // fill or update the mcInfo structure of StTofMCslat
    StTofMCSlat slat;
    slat.setSlatIndex(slatId);
    StTofMCInfo slatData;
    slatData.mNHits = 1; // slats will be reorderd and #hits/slat recounted
    slatData.mNPhe  = numberOfPhotoelectrons;
    slatData.mDe    = tof_hit->de;
    slatData.mDs    = tof_hit->ds;
    slatData.mTof =  tof_hit->tof;// warning: this is *NOT* correct ... prop-delay is not included !!!
    slatData.mTime = time;
    slatData.mMTime = tt;
    slatData.mMTimeL = tt1;
    slatData.mPmLength = length;
    slatData.mSLength = tof_hit->s_track;
    StThreeVectorD hitMomentum  = StThreeVectorD(tof_hit->p[0],
						 tof_hit->p[1],
						 tof_hit->p[2]);
    slatData.mPTot = hitMomentum.mag();
    slatData.mGId = tof_hit->id;
    slatData.mTrkId = tof_hit->track_p;
    
    slat.setMCInfo(slatData);

    // do a rough cts-style calibration
    float tdcOffset = mCalibDb->slat(slatId).offset_tdc
                      + random.shoot() * mCalibDb->slat(slatId).ods_tdc;
    float cctdc = mCalibDb->slat(slatId).cc_tdc;
    if(cctdc==0) cctdc=0.0001;
    unsigned short tdc = (unsigned short)((float)tt/cctdc+ tdcOffset);
    float adcOffset = mCalibDb->slat(slatId).offset_adc
                      + random.shoot() * mCalibDb->slat(slatId).ods_adc;
    unsigned short adc =  (unsigned short)((float)numberOfPhotoelectrons
                                     * mSimDb->nphe_to_adc() + adcOffset);
    if (tdc>2048) tdc=2048;
    if (adc>1024) adc=1024;
    slat.setAdc(adc);
    slat.setTdc(tdc);

    if(m_Mode){
      mAdc->Fill(adc);
      mTdc->Fill(tdc);
    }

    if (Debug()){
      LOG_INFO << "StTofmcInfo slatId " << slatId << "  " << slatData;
      LOG_INFO << "    a:" << adc << " t:" << tdc << " dE:"<< slatData.mDe << " dS:"<<  slatData.mDs 
	   << " mTof:" <<  slatData.mTof << " mTime:"<<  slatData.mTime << " mMTime:"<<slatData.mMTime
	   << " mMTimeL:"<< slatData.mMTimeL << " mSLength:" << slatData.mSLength
	   << " mPTot:" << slatData.mPTot  << endm;
    }

    // this part considers X-talk between slats (based on parameter below) ...
    //    LOG_INFO << "PHYSNOISE PARAMETER: " << mSimDb->phys_noise() << endm;
  
    return slat;
}



/// exponential slat response model
float StTofSimMaker::slatResponseExp(float& dz)
{
  // Exponential model for slat response
  return mSimDb->GeV_2_n_photons() * mSimDb->cath_eff()
         * mSimDb->cath_surf() * mSimDb->surf_loss()
         * exp(-dz/mSimDb->attlen());
}


/// write histograms to file
Int_t StTofSimMaker::Finish(){

  if(m_Mode){
    LOG_INFO << "StTofSimMaker::Finish  writing tofsim.root ...";
    TFile theFile("tofsim.root","RECREATE","tofsim");
    theFile.cd();
    mdE->Write();
    mdS->Write();
    mNumberOfPhotoelectrons->Write();
    mT->Write();
    mTime->Write();
    mTime1->Write();
    mPMlength->Write();
    mAdc->Write();
    mTdc->Write();
    LOG_INFO << "done"<<endm;
  }
  return kStOK;
}


/// digitize to ADC and TDC entries (empty)
void StTofSimMaker::fillRaw(){
  //fill the adc and tdc entries.
}


/// simulate electronic noise (empty)
void StTofSimMaker::electronicNoise(){
}


/// fill event (empty)
 void StTofSimMaker::fillEvent(){
}

/***************************************************************************
 *
 * $Log: StTofSimMaker.cxx,v $
 * Revision 1.13  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.12  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.11  2007/04/17 23:02:36  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.10  2006/12/08 18:55:26  dongx
 * Update to avoid zero tdc value in denominator - modified by Jing Liu
 *
 * Revision 1.9  2005/04/13 16:03:29  dongx
 * corresponding changes because of the update of StTofData structure
 *
 * Revision 1.8  2004/04/01 21:33:46  jeromel
 * More than one place where m_Mode should be used
 *
 * Revision 1.6  2003/09/17 19:49:10  geurts
 * zeroed pointers in constructor
 *
 * Revision 1.5  2003/09/02 17:59:10  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2003/08/08 00:22:11  geurts
 * changed location of header files for the local collections
 *
 * Revision 1.3  2003/07/25 04:34:44  geurts
 * - upper adc and tdc limits
 * - geometry initialization moved to InitRun()
 *
 * Revision 1.2  2002/12/12 01:43:46  geurts
 * Introduced InitRun() and FinishRun() members.
 * TofData in TofCollection is filled with adc and tdc data.
 * Extra checks for StEvent object to prevent null pointers.
 * Primitive ADC response function, disabled slatResponseExp().
 *
 * Revision 1.1  2001/09/28 19:11:11  llope
 * first version
 */
