// $Id: StFmsFastSimulatorMaker.cxx,v 1.11 2018/01/03 15:25:19 akio Exp $                                            
//                                                                                                                     
// $Log: StFmsFastSimulatorMaker.cxx,v $
// Revision 1.11  2018/01/03 15:25:19  akio
// update for FPost
//
// Revision 1.10  2017/09/28 17:07:19  akio
// addoing bitshiftgain
//
// Revision 1.9  2017/05/03 15:54:21  akio
// added gain scaling when attenuation was on during geant simulation
//
// Revision 1.8  2016/06/07 15:51:41  akio
// Making code better based on Coverity reports
//
// Revision 1.7  2015/10/29 21:19:16  akio
// adjust debug prints
//
// Revision 1.6  2015/10/20 19:52:36  akio
// Setting default for mFpsNPhotonPerMIP to 100.0
//
// Revision 1.5  2015/09/29 16:28:58  akio
// setFmsZS(int v) and if ADC<v, drop the hit (default=2)
// adding poisson distribution for FPS, with setFpsNPhotonPerMIP(float v)
//  (default=0 for now, which turns this off. It should be around 100?)
//
// Revision 1.4  2015/09/18 18:44:28  akio
// uses StEnumeration
//
// Revision 1.3  2015/02/26 23:53:04  yuxip
// new update from Akio
//                                                                               
// Revision 1.2  2014/05/06 16:05:56  jeromel                                                                          
// Adjust for include - there is no need to specify the path                                                           
//                                                                                                                     
// Revision 1.1  2014/05/06 16:02:04  jeromel                                                                          
// First version of StFmsFastSimulatorMaker deliverred upon review                                                     
//                                                                                                                     
/**
 \file StFmsFastSimulatorMaker.cxx
       Implementation of StFmsFastSimulatorMaker, the FMS fast simulator
 \author Pibero Djawotho <pibero@tamu.edu>
 \date 4 Jan 2011
 */
#include "StFmsFastSimulatorMaker/StFmsFastSimulatorMaker.h"

#include <algorithm>  // For std::fill(), std::max(), std::min()

#include "St_base/StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_fpdm_fmcg_Table.h"


#include "TRandom2.h"

/* Constructor. */
StFmsFastSimulatorMaker::StFmsFastSimulatorMaker(const Char_t* name) : 
    StMaker(name),mFpsDEPerMIP(0.0016),mFpsNPhotonPerMIP(100.0) { }

/* Process one event. */
Int_t StFmsFastSimulatorMaker::Make() {
  LOG_DEBUG << "StFmsFastSimulatorMaker::Make" << endm;
  // Check for the FMS database maker, bail out if it can't be located.
  if (!GetMaker("fmsDb")) {
    LOG_ERROR << "No StFmsDbMaker. StFmsDbMaker library not loaded?" << endm;
    return kStErr;
  }  // if
  // Get the existing StEvent, or add one if it doesn't exist.
  StEvent* event = static_cast<StEvent*>(GetDataSet("StEvent"));
  if (!event) {        
    event = new StEvent;
    AddData(event);
    LOG_DEBUG << "Creating StEvent" << endm;
  }  // if
  // Add an FMS collection to the event if one does not already exist.
  if (!event->fmsCollection()) {
    event->setFmsCollection(new StFmsCollection);
    LOG_DEBUG << "Creating StFmsCollection" << endm;
  }  // if

  // Check if attenuation was on/off
  // Get the wrapper to the FMCG table (may need to play with the path...)
  St_fpdm_fmcg* FMCG = (St_fpdm_fmcg*) GetChain()->  Find("bfc/.make/geant/.const/geom/fpdm_fmcg");
  // Get the first (only) row of the structure
  if(FMCG){
      fpdm_fmcg_st* fmcg = (fpdm_fmcg_st*)FMCG->At(0);
      if(fmcg){
	  // Get the attenuation flag
	  mAttenuation = fmcg->atten;
	  LOG_DEBUG << "Found St_fpdm_fmcg* at bfc/.make/geant/.const/geom/fpdm_fmcg and atten="<<mAttenuation<<endm;
      }else{
	  LOG_INFO << "Fail to get fpdm_fmcg_st" << endm;
      }
  }else{
      LOG_INFO << "Fail to find St_fpdm_fmcg* at bfc/.make/geant/.const/geom/fpdm_fmcg" << endm;
  }

  // Digitize GEANT FPD/FMS hits
  fillStEvent(event);
  if(Debug()) printStEventSummary(event);
  return kStOk;
}

/* Fill an event with StFmsHits. */
void StFmsFastSimulatorMaker::fillStEvent(StEvent* event) {
  // Existence of StFmsDbMaker was already checked in Make()
  // so we don't confirm the pointer again here.
  // Decode detector information from hit:
  StFmsDbMaker* dbMaker = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));
  StFmsCollection * fmscollection = event->fmsCollection();

  // Read the g2t table
  St_g2t_emc_hit* hitTable = static_cast<St_g2t_emc_hit*>(GetDataSet("g2t_fpd_hit"));
  if (!hitTable) {
    LOG_DEBUG << "g2t_fpd_hit table is empty" << endm;
    return;  // Nothing to do
  }  // if

  // Loop over FPD hits and accumurate hits
  const Int_t nHits = hitTable->GetNRows();
  //LOG_DEBUG << "g2t_fpd_hit table has " << nHits << " hits" << endm;
  if(nHits<1){
      LOG_DEBUG << "g2t_fpd_hit table has 0 rows" << endm;
      return;  
  }
  const g2t_emc_hit_st* hit = hitTable->GetTable();
  if(!hit){
      LOG_DEBUG << "g2t_fpd_hit GetTable failed" << endm;
      return;
  }

  StPtrVecFmsHit hits; //temp storage for hits
  //table to keep pointer to hit for each det & channel
  static const int NDET=16, NCH=600;
  //StFmsHit* map[NDET][NCH];   using heap memory (new) instead of stack (local)
  //memset(map,0,sizeof(map));
  auto map = new StFmsHit*[NDET][NCH](); //no need for memset with ()

  for (Int_t i=0; i < nHits; ++i) {
    if (hit) {
      const Int_t detectorId = getDetectorId(*hit);
      Int_t channel;
      if(detectorId==kFpsDetId) {
	  channel=dbMaker->fpsSlatIdFromG2t(hit->volume_id);
      }else if(detectorId==kFpostDetId) {
	  channel=dbMaker->fpostSlatIdFromG2t(hit->volume_id);
      }else{
	  channel=hit->volume_id % 1000;
      }
      LOG_INFO << Form("volid=%8d det=%2d ch=%4d e=%f\n",hit->volume_id,detectorId,channel,hit->de);
      if(detectorId<0 || detectorId>=NDET || channel<0 || channel>=NCH){
	  LOG_DEBUG << Form("det or ch out of range det=%d ch=%d",detectorId,channel) << endm;
	continue;
      }
      Float_t energy = hit->de;
      StFmsHit* fmshit=0;
      if(map[detectorId][channel]==0){ // New hit
	Int_t qtCrate, qtSlot, qtChannel, adc=0, tdc=0;
	if(detectorId==kFpsDetId){ //FPS
	  qtCrate=6;
	  dbMaker->fpsQTMap(channel,&qtSlot,&qtChannel);
	}else if(detectorId==kFpostDetId){ //FPOST
	  qtCrate=7;
	  dbMaker->fpostQTMap(channel,&qtSlot,&qtChannel);
	}else{ //FMS
	  dbMaker->getMap(detectorId, channel, &qtCrate, &qtSlot, &qtChannel);
	}
	fmshit = new StFmsHit(detectorId, channel, qtCrate, qtSlot, qtChannel, adc, tdc, energy);
	hits.push_back(fmshit);
	map[detectorId][channel]=fmshit;
      }else{ // Adding energy to old hit
	fmshit = map[detectorId][channel];
	fmshit->setEnergy(fmshit->energy() + energy);
      }
      hit++;
    }
  }
  int nfmshit=hits.size();
  delete [] map;

  // Loop over hits and digitize
  for(int i=0; i<nfmshit; i++){
    const Int_t detectorId = hits[i]->detectorId();
    const Int_t channel = hits[i]->channel();
    Float_t energy=hits[i]->energy();
    Float_t gain, gainCorrection;
    int adc;
    if(detectorId==kFpsDetId){ //FPS      
	gain = 1.0/dbMaker->fpsGain(channel);    //fpsGain gives ADCch for MIP peak      
	gainCorrection=mFpsDEPerMIP;             //about 1.6MeV per MIP      
	//add smering with poisson distriubtion
	if(mFpsNPhotonPerMIP>0.0){
	    static TRandom2 rnd;
	    int nPixel=static_cast<Int_t>(energy/gainCorrection*mFpsNPhotonPerMIP);
	    int nPixelMod = rnd.Poisson(nPixel);
	    energy = nPixelMod*gainCorrection/mFpsNPhotonPerMIP;
	}
    }else if(detectorId==kFpostDetId){ //FPOST
	gain = 1.0/dbMaker->fpostGain(channel);    //fpostGain gives ADCch for MIP peak      
	gainCorrection=mFpsDEPerMIP;               //about 1.6MeV per MIP      
	//add smering with poisson distriubtion
	if(mFpsNPhotonPerMIP>0.0){
	    static TRandom2 rnd;
	    int nPixel=static_cast<Int_t>(energy/gainCorrection*mFpsNPhotonPerMIP);
	    int nPixelMod = rnd.Poisson(nPixel);
	    energy = nPixelMod*gainCorrection/mFpsNPhotonPerMIP;
	}

    }else{
	// Get gain and correction from the database.
	gain = dbMaker->getGain(detectorId, channel);
	gainCorrection = dbMaker->getGainCorrection(detectorId, channel);
    }
    // Digitize
    if(mAttenuation==0){
	adc = static_cast<Int_t>(energy / (gain * gainCorrection) + 0.5);    
    }else{
	adc = static_cast<Int_t>(energy / mAttenuationGainScale / (gain * gainCorrection) + 0.5);    
    }
    if(mFmsBitShiftGain){
	short bitshift = dbMaker->getBitShiftGain(detectorId,channel);
	if(bitshift>0){
	    adc = adc & (0x0fff << bitshift);
	}else if(bitshift<0){
	    adc = std::min(adc, (0x1<<(12+bitshift))-1 );
	}
    }
    // Check for ADC values outside the allowed range and cap.
    adc = std::max(adc, 0);  // Prevent negative ADC
    adc = std::min(adc, 4095);  // Cap maximum ADC = 4,095
    // Recalculate energy accounting for ADC range
    Float_t digi_energy;
    if(detectorId!=kFpsDetId && detectorId!=kFpostDetId){
	digi_energy = adc * gain * gainCorrection;
    }else{
	digi_energy = adc * gain; //for FPS, this is not really energy but # of MIPs
    }
    if(adc>mFmsZSch){ //store only if significant energy deposit : adc>mFmsZSch(default=2)
      hits[i]->setAdc(adc);
      hits[i]->setEnergy(digi_energy);
      fmscollection->addHit(hits[i]);
      //      if(Debug())     
	  cout << Form("Det=%2d Ch=%3d E=%8.3f gain=%6.3f ADC=%4d digiE=%8.3f\n",detectorId,channel,energy,gain,adc,digi_energy);
    }else{
      delete hits[i];
    }
  }
  LOG_INFO << Form("Found %d g2t hits in %d cells, created %d hits with ADC>0",nHits,nfmshit,fmscollection->numberOfHits()) <<endm;
}

/*
 Returns the standard ID for the detector containing a hit.
 
 Detector parameters are defined as follows in the database:
   Name                ID type ew ns nx ny
   FPD-north           0  0    0  0   7  7
   FPD-south           1  0    0  1   7  7
   FPD-north-preshower 2  1    0  0   7  1
   FPD-south-preshower 3  1    0  1   7  1
   FPD-north-smdv      4  2    0  0  48  1
   FPD-south-smdv      5  2    0  1  48  1
   FPD-north-smdh      6  3    0  0   1 48
   FMS-south-smdh      7  3    0  1   1 48
   FMS-north-large     8  4    1  0  17 34
   FMS-south-large     9  4    1  1  17 34
   FMS-north-small     10 0    1  0  12 24
   FMS-south-small     11 0    1  1  12 24
 where
   - ew is the east-or-west location: 0 = east (the FPD), 1 = west (the FMS)
   - ns is the north-or-south location: 0 = north, 1 = south
   - nx, ny is the number of x, y channels
   - type corresponds to:
      - 0 = Small cell
      - 1 = Preshower
      - 2 = SMD-V
      - 3 = SMD-H
      - 4 = Large cell
      - 5 = Hadron calorimetry
 See:
 http://online.star.bnl.gov/dbExplorer/# --> Geometry/fms/ChannelGeometry
 Look into pams/sim/g2t/g2t_volume_id.g
 and search for FLGR (small pbg) or FLXF (large pbg).
 For FPS, there is no entry in fms/ChannelGeometry.
 Just assign detector Id=15 here.

Email from Akio describing decoding of the g2t volume ID, 16th Jan 2014:

 G2T volume id is currently
 id = ew*10000+nstb*1000+ch
 where
 ew: 1=fpd, 2=fms
 nstb for ew=1(FPD):
       1=north, 2=south, 3=top, 4=bottom
       5=north pbG preshower, 6=south pbg preshower
 nstb for ew=2(FMS):
       1=north large, 2=south large
       3=north small, 4=south small ch: channel#

 For FPS where id>100000:
   id = 100000+ew*10000+quad*1000+layr*100+slat
 where
   ew = always 1 for west
   quad = 1 to 4
   layr = 1 to 3
   slat = 1 to 21
 Fms/fpd volume id does NOT change at all. 
*/

Int_t StFmsFastSimulatorMaker::getDetectorId(const g2t_emc_hit_st& hit) const {
  enum { kFpd = 1, kFms = 2, kNorth = 0, kSouth = 1 };
  const Int_t volumeId = hit.volume_id;
  printf("voldid=%d\n",volumeId);
  // Decode volume ID into detector type, fpd/fms and north/south locations
  const Int_t isFPS    = volumeId / 100000;
  const Int_t fpdOrFms = (volumeId % 100000) / 10000;
  const Int_t module   = (volumeId % 10000) / 1000;
  if(isFPS==1) return kFpsDetId;
  if(isFPS==2) return kFpostDetId;
  switch (fpdOrFms) {
  case kFpd:
    switch (module) {
    case 1: return kFpdNorthDetId;     // north
    case 2: return kFpdSouthDetId;     // south
    case 5: return kFpdNorthPrsDetId;  // preshower north
    case 6: return kFpdSouthPrsDetId;  // preshower south
    }  // switch
    break;
  case kFms:
    switch (module) {
    case 1: return kFmsNorthLargeDetId;  // north large cells
    case 2: return kFmsSouthLargeDetId;  // south large cells
    case 3: return kFmsNorthSmallDetId;  // north small cells
    case 4: return kFmsSouthSmallDetId;  // south small cells
    }  // switch
    break;
  }  // switch
  return -1;
}

/* Dump hit information to LOG_INFO. */
void StFmsFastSimulatorMaker::printStEventSummary(const StEvent* event) {
  const Int_t NDETECTORS = 16;
  const Char_t* detectorNames[NDETECTORS] = {
      "FPD-North ",
      "FPD-South",
      "FPD-North-Pres",
      "FPD-South-Pres",
      "FPD-North-SMDV",
      "FPD-South-SMDV",
      "FPD-North-SMDH",
      "FPD-South-SMDH",
      "FMS-North-Large",
      "FMS-South-Large",
      "FMS-North-Small",
      "FMS-South-Small",
      "FHC-North",
      "FHC-South",
      "FMS-PreShower",
      "FMS-PostShower"
  };
  // Array of total hits per subdetector, initialised to all zeros
  Int_t nhits[NDETECTORS];
  std::fill(nhits, nhits + NDETECTORS, 0);
  // Array of total energy per sub-detector, initialised to all zeros
  Float_t detectorEnergy[NDETECTORS];
  std::fill(detectorEnergy, detectorEnergy + NDETECTORS, 0.f);
  // Sum number of hits and energies
  const StSPtrVecFmsHit& hits = event->fmsCollection()->hits();
  for (size_t i = 0; i < hits.size(); ++i) {
    const StFmsHit* hit = hits[i];
    ++nhits[hit->detectorId()];
    detectorEnergy[hit->detectorId()] += hit->energy();
    if(Debug()>1) hit->print();
  }  // for
  // Print detectors summary
  LOG_INFO << "ID\tNAME\t\tNHITS\tENERGY" << endm;
  for (Int_t detectorId = 0; detectorId < NDETECTORS; ++detectorId) {
    if(nhits[detectorId]>0)
      LOG_INFO << detectorId << '\t' << detectorNames[detectorId] << '\t'
	       << nhits[detectorId] << '\t' << detectorEnergy[detectorId] << endm;
  }  // for
}
