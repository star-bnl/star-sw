/***************************************************************************
 * $Id: StFstCalibrationMaker.cxx$
 *
 * Author: Shenghui Zhang, Oct. 2021
 ****************************************************************************
 * Description: 
 * See header file.
 ****************************************************************************
 ****************************************************************************
 * StFstCalibrationMaker.cxx,v 1.0
 * Revision 1.0 2021/10/15 Shenghui Zhang
 * Initial version
 ****************************************************************************/

#include <string>
#include "StEvent.h"
#include "StEvent/StEnumerations.h"
#include "StIOMaker/StIOMaker.h"
#include "StFstCalibrationMaker.h"
#include "StRoot/StFstUtil/StFstCollection.h"
#include "StRoot/StFstUtil/StFstRawHitCollection.h"
#include "StRoot/StFstUtil/StFstRawHit.h"
#include "StRoot/StFstDbMaker/StFstDb.h"
#include "StRoot/StFstUtil/StFstConsts.h"

#include "tables/St_fstMapping_Table.h"
#include "tables/St_fstControl_Table.h"

#include "TMath.h"

const string StFstCalibrationMaker::sectionLabel[72]={  "1I","1O","2I","2O","3I","3O","4I","4O","5I","5O","6I","6O","7I","7O","8I","8O","9I","9O","10I","10O","11I","11O","12I","12O","13I","13O","14I","14O","15I","15O","16I","16O","17I","17O","18I","18O","19I","19O","20I","20O","21I","21O","22I","22O","23I","23O","24I","24O","25I","25O","26I","26O","27I","27O","28I","28O","29I","29O","30I","30O","31I","31O","32I","32O","33I","33O","34I","34O","35I","35O","36I","36O"};

// constructor
StFstCalibrationMaker::StFstCalibrationMaker( const char* name ) : StMaker( name ), mTimeBinMask(0xFF), mRunHist(true), mDoPedCut(true), evtIdx(0), mHasFinished(0), mFstDb(0) {
  for(unsigned char iTb=0; iTb<kFstNumTimeBins; iTb++) {
    hist_meanPed[iTb]     = NULL;
    hist_rmsPed[iTb]      = NULL;
    hist_cmNoise[iTb]     = NULL;
    hist_ranNoise[iTb]    = NULL;
    hist_sumPed[iTb]      = NULL;
    hist_sumRms[iTb]      = NULL;
    hist_sumCmn[iTb]      = NULL;
    hist_sumRan[iTb]      = NULL;
    hist_adcSpectrum[iTb] = NULL;
  }

  mPedVec1stLoop.resize(kFstNumTimeBins*kFstNumElecIds);
  mRmsVec1stLoop.resize(kFstNumTimeBins*kFstNumElecIds);
  mRanVec1stLoop.resize(kFstNumTimeBins*kFstNumElecIds);

  mMathPedVec.resize(kFstNumTimeBins*kFstNumElecIds);
  mMathRmsVec.resize(kFstNumTimeBins*kFstNumElecIds);
  mMathCouVec.resize(kFstNumTimeBins*kFstNumElecIds);

  mMathRanVec.resize(kFstNumTimeBins*kFstNumElecIds);
  mMathPedRanVec.resize(kFstNumTimeBins*kFstNumElecIds);
  mMathRmsRanVec.resize(kFstNumTimeBins*kFstNumElecIds);
  mMathCouRanVec.resize(kFstNumTimeBins*kFstNumElecIds);

  if(mRunHist) 
  {
    mHistPedVec.assign( kFstNumTimeBins*kFstNumElecIds, (TH1S*)0 );
    mHistRanVec.assign( kFstNumTimeBins*kFstNumElecIds, (TH1F*)0 );
  }

  mHistCmnVec.assign( kFstNumTimeBins*kFstNumApvs*kFstNumRStripsPerSensor, (TH1F*)0 );
  mPedVec.resize( kFstNumTimeBins*kFstNumElecIds );                      // set to all zeros
  mCmnVec.resize( kFstNumTimeBins*kFstNumApvs*kFstNumRStripsPerSensor ); // set to all zeros

  mMappingVec.resize( kFstNumElecIds );
};

// deconstructor
StFstCalibrationMaker::~StFstCalibrationMaker()
{
  mPedVec1stLoop.clear();
  mRmsVec1stLoop.clear();
  mRanVec1stLoop.clear();

  mMathPedVec.clear();
  mMathRmsVec.clear();
  mMathCouVec.clear();

  mMathRanVec.clear();
  mMathPedRanVec.clear();
  mMathRmsRanVec.clear();
  mMathCouRanVec.clear();

  if(mRunHist) 
  {
    while (!mHistPedVec.empty()) delete mHistPedVec.back(), mHistPedVec.pop_back();
    while (!mHistRanVec.empty()) delete mHistRanVec.back(), mHistRanVec.pop_back();
  }

  while (!mHistCmnVec.empty()) delete mHistCmnVec.back(), mHistCmnVec.pop_back();

  mPedVec.clear();
  mCmnVec.clear();
  mMappingVec.clear();
};

// initialize
Int_t StFstCalibrationMaker::Init()
{
  Int_t ierr = kStOk;

  char buffer[100];
  for(unsigned char iTB=0; iTB<kFstNumTimeBins; iTB++) {
    sprintf(buffer,"hist_meanPedestal_TimeBin%d",iTB);
    hist_meanPed[iTB] = new TH1F(buffer, buffer, kFstNumElecIds, 0, kFstNumElecIds);
    hist_meanPed[iTB]->SetStats(false);
    hist_meanPed[iTB]->GetXaxis()->SetTitle("Channel Geometry Index");
    hist_meanPed[iTB]->GetXaxis()->SetNdivisions(-72,false); //sections
    hist_meanPed[iTB]->GetYaxis()->SetTitle("ADC count");

    sprintf(buffer,"hist_rmsPedestal_TimeBin%d",iTB);
    hist_rmsPed[iTB] = new TH1F(buffer, buffer, kFstNumElecIds, 0, kFstNumElecIds);
    hist_rmsPed[iTB]->SetStats(false);
    hist_rmsPed[iTB]->GetXaxis()->SetTitle("Channel Geometry Index");
    hist_rmsPed[iTB]->GetXaxis()->SetNdivisions(-72,false);//sections
    hist_rmsPed[iTB]->GetYaxis()->SetTitle("ADC count");

    sprintf(buffer,"hist_cmNoise_TimeBin%d",iTB);
    hist_cmNoise[iTB] = new TH1F(buffer, buffer, kFstNumApvs*kFstNumRStripsPerSensor, 0, kFstNumApvs*kFstNumRStripsPerSensor);
    hist_cmNoise[iTB]->SetStats(false);
    hist_cmNoise[iTB]->GetXaxis()->SetTitle("APV Geometry Index");
    hist_cmNoise[iTB]->GetXaxis()->SetNdivisions(-72,false);//sections
    hist_cmNoise[iTB]->GetYaxis()->SetTitle("ADC count");

    sprintf(buffer,"hist_ranNoise_TimeBin%d",iTB);
    hist_ranNoise[iTB] = new TH1F(buffer, buffer, kFstNumElecIds, 0, kFstNumElecIds);
    hist_ranNoise[iTB]->SetStats(false);
    hist_ranNoise[iTB]->GetXaxis()->SetTitle("Channel Geometry Index");
    hist_ranNoise[iTB]->GetXaxis()->SetNdivisions(-72,false);//sections
    hist_ranNoise[iTB]->GetYaxis()->SetTitle("ADC count");

    for(int i=0; i<72; i++) {
      TString binBuffer = "";
      binBuffer = sectionLabel[i];
      hist_meanPed[iTB]->GetXaxis()->SetBinLabel(i*512+256, binBuffer);
      hist_rmsPed[iTB]->GetXaxis()->SetBinLabel(i*512+256, binBuffer);
      hist_cmNoise[iTB]->GetXaxis()->SetBinLabel(i*4+4, binBuffer);
      hist_ranNoise[iTB]->GetXaxis()->SetBinLabel(i*512+256, binBuffer);
    }

    sprintf(buffer,"hist_sumPedestal_TimeBin%d",iTB);
    hist_sumPed[iTB] = new TH1F(buffer, buffer, 128, 0, 4096);
    hist_sumPed[iTB]->SetStats(kTRUE);
    hist_sumPed[iTB]->GetXaxis()->SetTitle("Pedestal [ADC counts]");
    hist_sumPed[iTB]->GetYaxis()->SetTitle("Counts");

    sprintf(buffer,"hist_sumRmsNoise_TimeBin%d",iTB);
    hist_sumRms[iTB] = new TH1F(buffer, buffer, 128, 0, 256);
    hist_sumRms[iTB]->SetStats(kTRUE);
    hist_sumRms[iTB]->GetXaxis()->SetTitle("RMS Noise [ADC counts]");
    hist_sumRms[iTB]->GetYaxis()->SetTitle("Counts");

    sprintf(buffer,"hist_sumCmNoise_TimeBin%d",iTB);
    hist_sumCmn[iTB] = new TH1F(buffer, buffer, 128, 0, 256);
    hist_sumCmn[iTB]->SetStats(kTRUE);
    hist_sumCmn[iTB]->GetXaxis()->SetTitle("CM Noise [ADC counts]");
    hist_sumCmn[iTB]->GetYaxis()->SetTitle("Counts");

    sprintf(buffer,"hist_sumRanNoise_TimeBin%d",iTB);
    hist_sumRan[iTB] = new TH1F(buffer, buffer, 128, 0, 256);
    hist_sumRan[iTB]->SetStats(kTRUE);
    hist_sumRan[iTB]->GetXaxis()->SetTitle("RAN Noise [ADC counts]");
    hist_sumRan[iTB]->GetYaxis()->SetTitle("Counts");

    sprintf(buffer,"hist_adcSpectrum_TimeBin%d",iTB);
    hist_adcSpectrum[iTB] = new TH2S(buffer, buffer, kFstNumElecIds, 0, kFstNumElecIds, 512, 0, 4096);
    hist_adcSpectrum[iTB]->SetStats(false);
    hist_adcSpectrum[iTB]->GetXaxis()->SetTitle("Channel Electronics Index");
    hist_adcSpectrum[iTB]->GetYaxis()->SetTitle("ADC counts");
  }  

  //read the pedestal/rms value from 1st loop for signal-like channel excluding in 2nd loop
  if(mDoPedCut) {
    TString infile = "fstPedNoiseTable.dat";
    std::ifstream in(infile);
    if (!in.is_open()) {
      mDoPedCut=false;
      LOG_WARN << "Could not find fstPedNoiseTable.dat! Set mDoPedCut to false" << endm;
    } else {
      LOG_INFO << "Read Pedestal and RMS from fstPedNoiseTable.dat!"<< endm;
      int chElecId, rdo, arm, apv, chan, chTimeBin, chCode;
      float chPed, chRms, chRan;
      while(!in.eof()) {
	in >> chElecId >> rdo >> arm >> apv >> chan >> chTimeBin >> chPed >> chRms >> chRan;

	chCode = kFstNumTimeBins * chElecId + chTimeBin;
	mPedVec1stLoop[chCode] = chPed;
	mRmsVec1stLoop[chCode] = chRms;
	mRanVec1stLoop[chCode] = chRan;
      }
    }
    in.close();
  }

  return ierr;
};

Int_t StFstCalibrationMaker::InitRun(Int_t runnumber)
{
  Int_t ierr = kStOk;

  TObjectSet *fstDbDataSet = (TObjectSet *)GetDataSet("fst_db");
  if (fstDbDataSet) {
    mFstDb = (StFstDb *)fstDbDataSet->GetObject();
    assert(mFstDb);
  }
  else {
    LOG_ERROR << "InitRun : no fstDb" << endm;
    return kStErr;
  }

  //control parameter
  const fstControl_st *fstControlTable = mFstDb->getControl();
  if (!fstControlTable)  {
    LOG_ERROR << "Pointer to FST control table is null" << endm;
    ierr = kStErr;
  }
  else
    mPedCut = fstControlTable[0].kFstPedCutDefault;

  //channel mapping
  const fstMapping_st *gM = mFstDb->getMapping();
  if( !gM ) {
    LOG_ERROR << "Pointer to FST mapping table is null" << endm;
    ierr = kStErr;
  }
  else {
    for(int i=0; i<kFstNumElecIds; i++) {
      LOG_DEBUG<<Form(" Print entry %d : geoId=%d ",i,gM[0].mapping[i])<<endm;
      mMappingVec[i] = gM[0].mapping[i];
    }
  }

  return ierr;
};

Int_t StFstCalibrationMaker::Make()
{
  Int_t ierr = kStOk;
  //obtain raw hit information from temporary dataset
  TObjectSet* fstDataSet = (TObjectSet*)GetDataSet("fstRawHitAndCluster");
  if (! fstDataSet) {
    LOG_WARN << "Make() - there is no fstDataSet (raw hits container) " << endm;
    ierr = kStWarn;
  }

  StFstCollection* fstCollectionPtr = (StFstCollection*)fstDataSet->GetObject();
  if(!fstCollectionPtr) {
    LOG_WARN << "Make() - no fstCollection."<<endm;
    ierr = kStWarn;
  }

  if( !ierr ){
    if( evtIdx%10 == 0 )
      cout << "event index: " << evtIdx << endl;

    std::stringstream ss, sc;
    int sumAdcPerEvent[kFstApvsPerWedge][kFstNumRStripsPerSensor][kFstNumTimeBins];
    int channelCountsPerEvent[kFstApvsPerWedge][kFstNumRStripsPerSensor][kFstNumTimeBins];
    double cmNoisePerEvent[kFstApvsPerWedge][kFstNumRStripsPerSensor][kFstNumTimeBins];

    for( unsigned char wedgeIdx=0; wedgeIdx<kFstNumWedges; ++wedgeIdx ){
      StFstRawHitCollection *rawHitCollectionPtr = fstCollectionPtr->getRawHitCollection( wedgeIdx );

      for(int i=0; i<kFstApvsPerWedge; i++) {
	for(int j=0; j<kFstNumRStripsPerSensor; j++) {
	  for(int k=0; k<kFstNumTimeBins; k++) {
	    sumAdcPerEvent[i][j][k] = 0;
	    channelCountsPerEvent[i][j][k] = 0;
	    cmNoisePerEvent[i][j][k] = 0.;
	  }
	}
      }

      if( rawHitCollectionPtr ){
	std::vector<StFstRawHit*>& rawHitVec = rawHitCollectionPtr->getRawHitVec();
	std::vector< StFstRawHit* >::iterator rawHitIter;

	for( rawHitIter = rawHitVec.begin(); rawHitIter != rawHitVec.end(); ++rawHitIter ){
	  int elecId        = (*rawHitIter)->getChannelId();
	  int geoId         = (*rawHitIter)->getGeoId();           //channel geometry Id, counting from 0 to 36863
	  int apvId         = elecId/kFstNumApvChannels;           //APV chip geometry Id, counting from 0 to 287
	  int ChPerRdo      = kFstNumArmsPerRdo*kFstNumChanPerArm; // 6144: 128 channels * 8 APVs * 6 modules
	  int rdoIdx        = elecId/ChPerRdo + 1;                                     // 1-6
	  int armIdx        = (elecId - (rdoIdx-1)*ChPerRdo)/kFstNumChanPerArm;        // 0-2
	  int refElecChanId = elecId - (rdoIdx-1)*ChPerRdo - armIdx*kFstNumChanPerArm; // 0-2047
	  int refApvIdx     = refElecChanId/kFstNumApvChannels;                        // 0-15
	  int portIdx       = refApvIdx/8;                                             // 0-1
	  int lclApvIdx     = refApvIdx-portIdx*8;                                     // 0-7

	  if(elecId >= kFstNumElecIds ||  geoId >= kFstNumElecIds || apvId >= kFstNumApvs)	
	    continue;

	  for( unsigned char timeBin = 0; timeBin < kFstNumTimeBins; ++timeBin ){
	    Int_t adc = (*rawHitIter)->getCharge( timeBin );			
	    if(adc) {
	      int t = (int)timeBin;
	      if(mTimeBinMask==0) t=0;
	      int code = kFstNumTimeBins * elecId + t; 

	      //exclude signal-like channels time bin by time bin in current event
	      bool pass = kTRUE;
	      if(mDoPedCut) {
		// pass = (adc > (mPedVec1stLoop[code]-mPedCut*mRmsVec1stLoop[code])) && (adc < (mPedVec1stLoop[code]+mPedCut*mRmsVec1stLoop[code]));
		pass = (adc < (mPedVec1stLoop[code]+mPedCut*mRmsVec1stLoop[code]));
		hist_adcSpectrum[t]->Fill(elecId, adc);
	      }
	      if(!pass) continue;

	      mMathPedVec[code] += adc;
	      mMathRmsVec[code] += adc*adc;
	      mMathCouVec[code] ++;

	      if(mRunHist) {
		TH1S* histPed = mHistPedVec[ code ];
		if( !histPed ){
		  ss.str("");
		  ss.clear();
		  ss << "hist_Pedestal_Ch" << code / kFstNumTimeBins << "_TB" << code % kFstNumTimeBins;
		  histPed = new TH1S( ss.str().data(), "", 512, 0, kFstMaxAdc );
		  mHistPedVec[ code ] = histPed;
		}
		histPed->Fill( adc );
	      }

	      if(mDoPedCut) { // need the third run to be accurate
		int rstrip = (geoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor))/kFstNumPhiSegPerWedge;
		if(rstrip > 3) rstrip = rstrip-4;
		sumAdcPerEvent[lclApvIdx][rstrip][t] += adc - mPedVec1stLoop[code];
		channelCountsPerEvent[lclApvIdx][rstrip][t] ++;
	      }
	    }//adc cut
	  }//time bin loop
	}//raw hits loop

	if(mDoPedCut) {
	  //common mode calculation per event
	  for(int i=0; i<kFstApvsPerWedge; i++) {
	    for(int j=0; j<kFstNumRStripsPerSensor; j++) {
	      for(int k=0; k<kFstNumTimeBins; k++) {
		if( channelCountsPerEvent[i][j][k] ) {
		  cmNoisePerEvent[i][j][k] = (double)sumAdcPerEvent[i][j][k] / channelCountsPerEvent[i][j][k];

		  int apvId = wedgeIdx*kFstApvsPerWedge + i;
		  int groupId   = apvId*kFstNumRStripsPerSensor + j;
		  int code  = groupId*kFstNumTimeBins + k;

		  TH1F* histCmn = mHistCmnVec[ code ];

		  if( !histCmn ){
		    sc.str("");
		    sc.clear();
		    sc << "hist_CMNoise_APV" << apvId << "_R" << j << "_TB" << k;
		    histCmn = new TH1F( sc.str().data(), "", -1024, -kFstMaxAdc, kFstMaxAdc);
		    mHistCmnVec[ code ] = histCmn;
		  }
		  histCmn->Fill( cmNoisePerEvent[i][j][k] );
		}
	      }
	    }
	  }

	  for( rawHitIter = rawHitVec.begin(); rawHitIter != rawHitVec.end(); ++rawHitIter ){
	    int elecId = (*rawHitIter)->getChannelId();
	    int geoId  = (*rawHitIter)->getGeoId(); //channel geometry Id, counting from 0 to 36863
	    int apvId         = elecId/kFstNumApvChannels;           //APV chip geometry Id, counting from 0 to 287
	    int ChPerRdo      = kFstNumArmsPerRdo*kFstNumChanPerArm; // 6144: 128 channels * 8 APVs * 6 modules
	    int rdoIdx        = elecId/ChPerRdo + 1;                                     // 1-6
	    int armIdx        = (elecId - (rdoIdx-1)*ChPerRdo)/kFstNumChanPerArm;        // 0-2
	    int refElecChanId = elecId - (rdoIdx-1)*ChPerRdo - armIdx*kFstNumChanPerArm; // 0-2047
	    int refApvIdx     = refElecChanId/kFstNumApvChannels;                        // 0-15
	    int portIdx       = refApvIdx/8;                                             // 0-1
	    int lclApvIdx     = refApvIdx-portIdx*8;                                     // 0-7

	    if(elecId >= kFstNumElecIds ||  geoId >= kFstNumElecIds || apvId >= kFstNumApvs)	
	      continue;

	    for( unsigned char timeBin = 0; timeBin < kFstNumTimeBins; ++timeBin ){
	      Int_t adc = (*rawHitIter)->getCharge( timeBin );			
	      if(adc) {
		int t = (int)timeBin;
		if(mTimeBinMask==0) t=0;
		int code = kFstNumTimeBins * elecId + t; 

		//exclude signal-like channels time bin by time bin in current event
		bool pass = (adc < (mPedVec1stLoop[code]+mPedCut*mRmsVec1stLoop[code]));
		if(!pass) continue;

		int rstrip = (geoId % (kFstNumInnerSensorsPerWedge * kFstNumStripsPerInnerSensor + kFstNumOuterSensorsPerWedge * kFstNumStripsPerOuterSensor))/kFstNumPhiSegPerWedge;
		if(rstrip > 3) rstrip = rstrip-4;

		mMathPedRanVec[code] += adc-mPedVec1stLoop[code]-cmNoisePerEvent[lclApvIdx][rstrip][t];
		mMathRmsRanVec[code] += (adc-mPedVec1stLoop[code]-cmNoisePerEvent[lclApvIdx][rstrip][t])*(adc-mPedVec1stLoop[code]-cmNoisePerEvent[lclApvIdx][rstrip][t]);
		mMathCouRanVec[code] ++;

		if(mRunHist) {
		  TH1F* histRan = mHistRanVec[ code ];
		  if( !histRan ){
		    ss.str("");
		    ss.clear();
		    ss << "hist_RanNoise_Ch" << code / kFstNumTimeBins << "_TB" << code % kFstNumTimeBins;
		    histRan = new TH1F( ss.str().data(), "", 1024, -kFstMaxAdc, kFstMaxAdc );
		    mHistRanVec[ code ] = histRan;
		  }
		  histRan->Fill( adc-mPedVec1stLoop[code]-cmNoisePerEvent[lclApvIdx][rstrip][t] );
		}
	      }//adc cut
	    }//time bin loop
	  }//raw hits loop
	}// calculate random noise

	//clear raw hit objects in vector
	while (!rawHitVec.empty()) delete rawHitVec.back(), rawHitVec.pop_back();
      }//raw hit collection cut
    }//wedgeIdx loop
  }//ierr cut
  evtIdx++;

  return ierr;
};

// save as needed
Int_t StFstCalibrationMaker::Finish()
{
  Int_t ierr = kStOk;

  if( !mHasFinished ){
    mHasFinished = 1;
    cout << "StFstCalibrationMaker::Finish()" << endl;

    //calculate pedestal/RMS with mathematical method
    for(int i=0; i<kFstNumTimeBins*kFstNumElecIds; i++) {
      double mathPed, mathRms;
      if(mMathCouVec[i] < 1) {
	mathPed = 0.;
	mathRms = 100.;
      }
      else {
	mathPed = mMathPedVec[i]/mMathCouVec[i];
	mathRms = mMathRmsVec[i]/mMathCouVec[i];
      }

      double variance = mathRms - mathPed*mathPed;
      if(variance > 0)
	mathRms = sqrt( variance );
      else
	mathRms = 100.;

      mMathPedVec[i] = mathPed;
      mMathRmsVec[i] = mathRms;
    }

    // calculate random noise with mathematical method
    for(int i=0; i<kFstNumTimeBins*kFstNumElecIds; i++) {
      double mathPedRan, mathRmsRan;
      if(mMathCouRanVec[i] < 1) {
	mathPedRan = 0.;
	mathRmsRan = 10000.;
      }
      else {
	mathPedRan = mMathPedRanVec[i]/mMathCouRanVec[i];
	mathRmsRan = mMathRmsRanVec[i]/mMathCouRanVec[i];
      }

      double variance = mathRmsRan - mathPedRan*mathPedRan;
      if(variance > 0)
	mathRmsRan = sqrt( variance );
      else
	mathRmsRan = 100.;

      mMathRanVec[i] = mathRmsRan;
    }

    //calculate pedestal/RMS with histogram method
    if(mRunHist) {
      std::vector< TH1S* >::iterator mHistPedVecIter;
      int elecIdx = 0;

      for( mHistPedVecIter = mHistPedVec.begin(); mHistPedVecIter != mHistPedVec.end(); ++mHistPedVecIter ){
	TH1S *histPed = *mHistPedVecIter;
	elecIdx = std::distance( mHistPedVec.begin(), mHistPedVecIter );
	short timebin = elecIdx % kFstNumTimeBins;
	int chanIdx   = elecIdx / kFstNumTimeBins;
	if(histPed)  {
	  int   entries = histPed->GetEntries();
	  float meanPed = histPed->GetMean();
	  float rmsPed  = histPed->GetRMS(); 

	  if( entries == 0 ) {
	    meanPed = 0.;
	    rmsPed  = 100.; //marked as dead channel
	  }

	  pedNoiseData_t &data = mPedVec[ elecIdx ];
	  data.n    = entries;
	  data.ped  = meanPed;
	  data.rms  = rmsPed;

	  if(mDoPedCut) {//only fill histograms in the 2nd loop
	    int geomIdx  = mMappingVec[chanIdx];
	    hist_meanPed[timebin]->SetBinContent(geomIdx+1, meanPed);
	    hist_meanPed[timebin]->SetBinError(geomIdx+1, 0.);
	    hist_rmsPed[timebin]->SetBinContent(geomIdx+1, rmsPed);
	    hist_rmsPed[timebin]->SetBinError(geomIdx+1, 0.);
	    hist_sumPed[timebin]->Fill( meanPed );
	    hist_sumRms[timebin]->Fill( rmsPed );
	  }
	}
      }
    }

    //calculate random noise with histogram method
    if(mRunHist) {
      std::vector< TH1F* >::iterator mHistRanVecIter;
      int elecIdx = 0;

      for( mHistRanVecIter = mHistRanVec.begin(); mHistRanVecIter != mHistRanVec.end(); ++mHistRanVecIter ){
	TH1F *histRan = *mHistRanVecIter;
	elecIdx = std::distance( mHistRanVec.begin(), mHistRanVecIter );
	short timebin = elecIdx % kFstNumTimeBins;
	int chanIdx   = elecIdx / kFstNumTimeBins;
	if(histRan)  {
	  int   entries = histRan->GetEntries();
	  float rmsRan  = histRan->GetRMS(); 

	  if( entries == 0 ) {
	    rmsRan = 100.; //marked as dead channel
	  }

	  pedNoiseData_t &data = mPedVec[ elecIdx ];
	  data.ran = rmsRan;

	  if(mDoPedCut) {//only fill histograms in the 2nd loop
	    int geomIdx  = mMappingVec[chanIdx];
	    hist_ranNoise[timebin]->SetBinContent(geomIdx+1, rmsRan);
	    hist_ranNoise[timebin]->SetBinError(geomIdx+1, 0.);
	    hist_sumRan[timebin]->Fill( rmsRan );
	  }
	}
      }
    }

    std::vector< TH1F* >::iterator mHistCmnVecIter;
    int groupIdx = 0;
    for( mHistCmnVecIter = mHistCmnVec.begin(); mHistCmnVecIter != mHistCmnVec.end(); ++mHistCmnVecIter ){
      TH1F *histCmn = *mHistCmnVecIter;
      groupIdx = std::distance( mHistCmnVec.begin(), mHistCmnVecIter );

      short timebin = groupIdx % kFstNumTimeBins;
      int   gouIdx  = groupIdx / kFstNumTimeBins;

      if(histCmn)  {
	int   entries = histCmn->GetEntries();
	float cmNoise = histCmn->GetRMS();

	if(entries == 0) {
	  cmNoise = 100.; //marked as dead chip
	}

	cmNoiseData_t &data = mCmnVec[ groupIdx ];
	data.n    = entries;
	data.cmn  = cmNoise;

	if(mDoPedCut) {
	  hist_cmNoise[timebin]->SetBinContent(gouIdx+1, cmNoise);
	  hist_cmNoise[timebin]->SetBinError(gouIdx+1, 0.);
	  hist_sumCmn[timebin]->Fill(cmNoise);
	}
      }
    }

    ierr = saveToFile();
  }

  return ierr;
};

// functions that actually do the saving
Int_t StFstCalibrationMaker::saveToFile()
{
  Int_t ierr = kStOk;

  //create output file
  StIOMaker *ioMaker = (StIOMaker * )GetMaker("inputStream");
  if (!ioMaker) {
    gMessMgr->Warning() << "StFstCalibrationMaker::Init(): No StIOMaker" << endm;
  }

  TString filename = TString(ioMaker->GetFile());
  int found = filename.Last('/');
  if(found >= 0){
    filename.Replace(0, found + 1, "");
  }
  found = filename.First(".");
  if(found == 0) found = filename.Length();

  TString mRootFilename = filename.Data();
  TString mPedNoiseFilename_hist = filename.Data();
  TString mCmNoiseFilename = filename.Data();    
  TString mPedNoiseFilename_math = filename.Data();

  mRootFilename.Replace(found, mRootFilename.Length() - found, ".fstCaliQa.root");
  LOG_INFO << "FST Calibration QA File Name: " << mRootFilename << endm;

  //create QA file
  myRootFile = new TFile(mRootFilename.Data(),"RECREATE");
  if( !myRootFile ) {
    LOG_WARN << "Error recreating file '" << mRootFilename << "'" << endl;
    ierr = kStWarn;
  }
  //save calibration QA histograms
  for(unsigned char iTB=0; iTB<kFstNumTimeBins; iTB++) {
    myRootFile->WriteTObject(hist_meanPed[iTB]);
    myRootFile->WriteTObject(hist_rmsPed[iTB]);
    myRootFile->WriteTObject(hist_cmNoise[iTB]);
    myRootFile->WriteTObject(hist_ranNoise[iTB]);
    myRootFile->WriteTObject(hist_sumPed[iTB]);
    myRootFile->WriteTObject(hist_sumRms[iTB]);
    myRootFile->WriteTObject(hist_sumCmn[iTB]);
    myRootFile->WriteTObject(hist_sumRan[iTB]);
    myRootFile->WriteTObject(hist_adcSpectrum[iTB]);
  }
  myRootFile->Close();

  //save pedestal and rms noise with mathematical method
  mPedNoiseFilename_math.Replace(found, mPedNoiseFilename_math.Length() - found, ".fstPedNoise_math.dat");
  LOG_INFO << "FST Pedestal and RMS File Name using math method: " << mPedNoiseFilename_math << endm;

  std::ofstream fout_ped_math( mPedNoiseFilename_math.Data(), std::ios_base::out & std::ios_base::trunc );
  if( !fout_ped_math ){
    LOG_ERROR << "Error opening file '" << mPedNoiseFilename_math << "'" << endm;
    ierr = kStFatal;
  }
  fout_ped_math.setf(std::ios::fixed, std::ios::floatfield);
  fout_ped_math.precision(5);

  for(int i=0; i<kFstNumTimeBins*kFstNumElecIds; i++) {
    //obtain rdo/arm/apv/chan
    int rdo = 0, arm = -1, apv = -1, chan = -1;
    short timebin = i % kFstNumTimeBins;
    int elecId    = i / kFstNumTimeBins;
    rdo           = 1 + elecId/(kFstNumArmsPerRdo*kFstNumChanPerArm);
    arm           = (elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))/kFstNumChanPerArm;
    apv           = ((elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))%kFstNumChanPerArm)/kFstNumApvChannels;
    chan          = ((elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))%kFstNumChanPerArm)%kFstNumApvChannels;

    fout_ped_math << elecId << ' ' << rdo << ' ' << arm << ' ' << apv << ' ' << chan << ' ' << timebin << ' ' << mMathPedVec[i] << ' ' << mMathRmsVec[i] << ' ' << mMathRanVec[i] << endl;
  }
  fout_ped_math.close();

  //create a link as fstPedNoiseTable.dat in 1st loop
  if(!mDoPedCut) {
    char cmd[128];
    sprintf(cmd, "/bin/ln -f -s %s fstPedNoiseTable.dat", mPedNoiseFilename_math.Data());
    system(cmd);
  }

  //save pedestal and rms noise with histogram method
  if(mRunHist) {
    mPedNoiseFilename_hist.Replace(found, mPedNoiseFilename_hist.Length() - found, ".fstPedNoise_hist.dat");
    LOG_INFO << "FST Pedestal and RMS File Name using histogram method: " << mPedNoiseFilename_hist << endm;

    std::ofstream fout_ped_hist( mPedNoiseFilename_hist.Data(), std::ios_base::out & std::ios_base::trunc );
    if( !fout_ped_hist ){
      LOG_ERROR << "Error opening file '" << mPedNoiseFilename_hist << "'" << endm;
      ierr = kStFatal;
    }
    fout_ped_hist.setf(std::ios::fixed, std::ios::floatfield);
    fout_ped_hist.precision(5);

    pedNoiseDataVec_t::iterator pedDataVecIter;
    int idx = 0;
    for( pedDataVecIter = mPedVec.begin(); pedDataVecIter != mPedVec.end() && !ierr; ++pedDataVecIter, ++idx ){
      //obtain rdo/arm/apv/chan
      int rdo = 0, arm = -1, apv = -1, chan = -1;
      short timebin = idx % kFstNumTimeBins;
      int elecId    = idx / kFstNumTimeBins;
      rdo           = 1 + elecId/(kFstNumArmsPerRdo*kFstNumChanPerArm);
      arm           = (elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))/kFstNumChanPerArm;
      apv           = ((elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))%kFstNumChanPerArm)/kFstNumApvChannels;
      chan          = ((elecId%(kFstNumArmsPerRdo*kFstNumChanPerArm))%kFstNumChanPerArm)%kFstNumApvChannels;

      fout_ped_hist << elecId << ' ' << rdo << ' ' << arm << ' ' << apv << ' ' << chan << ' ' << timebin << ' ' << pedDataVecIter->ped << ' ' << pedDataVecIter->rms << ' ' << pedDataVecIter->ran << endl;
    }
    fout_ped_hist.close();
  }

  //save common mode noise
  mCmNoiseFilename.Replace(found, mCmNoiseFilename.Length() - found, ".fstCmNoise.dat");
  LOG_INFO << "FST Common Mode Noise File Name: " << mCmNoiseFilename << endm;

  std::ofstream fout_cmn( mCmNoiseFilename.Data(), std::ios_base::out & std::ios_base::trunc );
  if( !fout_cmn ){
    LOG_ERROR << "Error opening file '" << mCmNoiseFilename << "'" << endm;
    ierr = kStFatal;
  }
  fout_cmn.setf(std::ios::fixed, std::ios::floatfield);
  fout_cmn.precision(5);

  cmNoiseDataVec_t::iterator cmnDataVecIter;
  int idx = 0;
  for( cmnDataVecIter = mCmnVec.begin(); cmnDataVecIter != mCmnVec.end() && !ierr; ++cmnDataVecIter, ++idx ){
    short timebin   = idx % kFstNumTimeBins;
    int groupId     = idx / kFstNumTimeBins;
    int rId         = groupId % kFstNumRStripsPerSensor;
    int apvId       = groupId / kFstNumRStripsPerSensor;
    int wedgeGeomId = 1 + apvId/kFstApvsPerWedge;
    int rdo         = (wedgeGeomId-1)/kFstNumWedsPerRdo + 1;                 //1-6
    int arm         = ((wedgeGeomId-1)%kFstNumWedsPerRdo)/kFstNumWedsPerArm; //0-2
    // int apv         = apvId%(kFstNumArmsPerRdo*kFstNumRdos);                 //0-15
    int apv         = apvId%kFstNumApvsPerArm;                 //0-15

    fout_cmn << apvId << ' ' << rdo << ' ' << arm << ' '<< apv << ' ' <<  rId << ' ' << timebin << ' ' << cmnDataVecIter->cmn << endl;
  }
  fout_cmn.close();

  return ierr;
};

ClassImp( StFstCalibrationMaker );
