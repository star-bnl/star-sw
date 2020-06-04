/***************************************************************************
 *
 * $Id: StIstSlowSimMaker.cxx,v 1.6 2018/04/18 18:10:19 jwebb Exp $
 *
 * Author: Leszek Kosarzewski, March 2014
 ****************************************************************************
 * Description: 
 * See header file.
 ****************************************************************************
 * StIstSlowSimMaker.h,v 1.1
 * Revision 1.1 2014/08/05 10:54:12 ypwang
 * Update to integrated into IST offline chain
 * 1) process IST MC hits, and discretize to individual IST channel
 * 2) translate to ADC by simple GeV-2-ADC factor, raw ADC output
 ****************************************************************************
 * StIstSlowSimMaker.cxx,v 1.0
 * Revision 1.0 2014/03/07 11:25:30 lkosarz
 * Initial version
 ****************************************************************************/

//#include <Stiostream.h>
//#include <StHit.h>
//#include <StEventTypes.h>
//#include <StEvent.h>
#include <StMcEvent.hh>
#include <StMcEvent/StMcHit.hh>
#include <StMcIstHit.hh>
//#include <StMcEvent/StMcIstHit.hh>
#include <StMcEvent/StMcIstHitCollection.hh>
//#include <StMcEventTypes.hh>

//#include <stdio.h>
//#include <map>
//#include <exception>
using namespace std;
//#include <stdexcept>
#include "tables/St_g2t_ist_hit_Table.h"
#include "tables/St_HitError_Table.h"
#include "TGeoManager.h"
//#include "TGeoMatrix.h"
//#include "TDataSet.h"
#include "TF1.h"

#include "StIstDbMaker/StIstDb.h"
#include "StIstUtil/StIstCollection.h"
#include "StIstUtil/StIstRawHitCollection.h"
#include "StIstUtil/StIstRawHit.h"
#include "StIstUtil/StIstConsts.h"
#include "StIstSlowSimMaker.h"
#include "TRandom3.h"
#include "tables/St_istSimPar_Table.h"

#include "tables/St_istMapping_Table.h"
#include "tables/St_istControl_Table.h"

#include <SystemOfUnits.h>

ClassImp(StIstSlowSimMaker)

StIstSlowSimMaker::StIstSlowSimMaker(const char* name): StMaker(name), mIstDb(NULL), mBuildIdealGeom(kFALSE), mIstCollectionPtr(NULL), mHitEffMode(0), mMomCut(0.),  mHitEff(1.0), mRndGen(nullptr)
{
	mMappingGeomVec.resize( kIstNumElecIds ); //! initialize to number of channels in StIstUtil/StIstConsts.h
   mDefaultTimeBin = 9; //! default ADC timebins, load from control table
   mCurrentTimeBinNum = 9; //! current ADC timebins, load from control table
   fAdc = new TF1("fAdc","landau",0,9);
   fAdc->SetParameters(1.14288,3.8,1.168);
}

Int_t StIstSlowSimMaker::Init()
{
	LOG_DEBUG<<"StIstSlowSimMaker::Init()"<<endm;

	//prepare output dataset
	mIstCollectionPtr = new StIstCollection();
   ToWhiteConst("istRawAdcSimu",mIstCollectionPtr);

	if(!mIstCollectionPtr ) {
		LOG_WARN << "Error constructing istCollection" << endm;
	   return kStErr;
	}

	return kStOk;
}

//____________________________________________________________
Int_t StIstSlowSimMaker::InitRun(Int_t runnumber)
{
	LOG_DEBUG<<"StIstSlowSimMaker::InitRun "<<runnumber<<endm;

	Int_t ierr = kStOk;

	TObjectSet *istDbDataSet = (TObjectSet *)GetDataSet("ist_db");
	if (istDbDataSet) {
		mIstDb = (StIstDb *)istDbDataSet->GetObject();
		assert(mIstDb);
	}
	else {
		LOG_ERROR << "InitRun : no istDb" << endm;
		return kStErr;
	}

	// IST control parameters
	const istControl_st *istControlTable = mIstDb->getControl() ;
	if (!istControlTable)  {
		LOG_ERROR << "Pointer to IST control table is null" << endm;
		ierr = kStErr;
	}
	else {
		mDefaultTimeBin = istControlTable[0].kIstDefaultTimeBin;
		mCurrentTimeBinNum = istControlTable[0].kIstCurrentTimeBinNum;
	}

	// IST mapping table
	const istMapping_st *gM = mIstDb->getMapping();
	if( !gM ) {
		LOG_ERROR << "Pointer to IST mapping table is null" << endm;
		ierr = kStErr;
	}
	else {
		for(Int_t i=0; i<kIstNumElecIds; i++) {
			LOG_DEBUG<<Form(" PrInt_t entry %d : geoId=%d ",i,gM[0].mapping[i])<<endm;
			Int_t geomIdTemp = gM[0].mapping[i];
			mMappingGeomVec[geomIdTemp-1] = i;
		}
	}

        //! tunable parameter
        mRndGen = (TRandom3 *)gRandom;
        // MC->RC hit efficiency (momentum dependence - default)
        istSimPar_st const* const istSimParTable = mIstDb->istSimPar();
        mHitEffMode = istSimParTable[0].mode;
        mMomCut = istSimParTable[0].pCut;
        mHitEff = istSimParTable[0].effIst; // best knowledge from ZF cosmic ray study - tunable
            
        LOG_INFO << " IST MC hit efficiency mode used for IST slow simulator: " << mHitEffMode << endm;
        LOG_INFO << "     +++ Hit Efficiency at p > " << mMomCut << " GeV/c = " << mHitEff << endm;
            
	return ierr;
}

//______________________________________________________________________________
Int_t StIstSlowSimMaker::Make()
{
	// Get the input data structures from StMcEvent
	StMcEvent* mcEvent = (StMcEvent *) GetInputDS("StMcEvent");
	if (! mcEvent) {LOG_INFO << "No StMcEvent on input" << endl; return kStWarn;}

	TDataSetIter geant(GetInputDS("geant"));
	if ( mBuildIdealGeom && !gGeoManager ) {
		GetDataBase("VmcGeometry");
	}

	StThreeVectorF mHitError(0.,0.,0.);

	// Get MC Ist hit collection. This contains all ist hits.
	const StMcIstHitCollection* istMcHitCol = mcEvent->istHitCollection();

	mIstCollectionPtr->setNumTimeBins(mCurrentTimeBinNum);

   // Calculate pad center position from mc hits. Convert dE to ADC value. Save hits to raw hit collection.
	if(istMcHitCol){
		LOG_DEBUG<<"ist MC hit collection found"<<endm;
		Int_t nIsthits=istMcHitCol->numberOfHits();
		if(nIsthits){
			if(istMcHitCol->layer(0)){
            StSPtrVecMcIstHit mcHitVec = istMcHitCol->layer(0)->hits();
				for(std::vector<StMcIstHit*>::iterator mcHitIt = mcHitVec.begin();mcHitIt!=mcHitVec.end(); ++mcHitIt){
					LOG_DEBUG << "IST MC hit found ...... " << endm;
					StMcIstHit* mcIstHit = (*mcHitIt);
					if(!mcIstHit) continue;
                                        float hitEff = 1.0;
                                        switch (mHitEffMode) {
                                          case 0:   // ideal case 100% efficiency
                                                  break;
                                          case 1:
                                            if(mcIstHit->parentTrack()) {
                                              float const ptot = mcIstHit->parentTrack()->momentum().mag();
                                              hitEff = 1.0 - (1. - mHitEff)*ptot/mMomCut;
                                              if(hitEff<mHitEff) hitEff = mHitEff;
                                            }
                                                  break;
                                          case 2:
                                              hitEff = mHitEff;
                                                  break;
                                          default:
                                                  break;
                                        }
                                        
                                        if( mRndGen->Rndm()>hitEff ) continue;

					//mcIstHits stored local position
					Double_t localIstHitPos[3]={mcIstHit->position().x(),mcIstHit->position().y(),mcIstHit->position().z()};
					LOG_DEBUG << "StMcIstHit local position (before discretization) = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endm;
					Int_t ladder = mcIstHit->ladder(); //coded from 1 to 24
					Int_t sensor = mcIstHit->wafer(); //coded from 1 to 6
					UShort_t meanColumn, meanRow;

               //get column and row value from mc hit local position
					getMCHitRowAndColumn(mcIstHit, meanColumn, meanRow);

					//discretize hit local position (2D structure of IST sensor pads)
					Float_t rPhiPos_mean = (meanRow-1) * kIstPadPitchRow + 0.5 * kIstPadPitchRow; //unit: cm
					Float_t zPos_mean    = (meanColumn-1) * kIstPadPitchColumn + 0.5 * kIstPadPitchColumn; //unit: cm
					localIstHitPos[0]    = kIstSensorActiveSizeRPhi/2.0 - rPhiPos_mean;
					localIstHitPos[2]    = zPos_mean - kIstSensorActiveSizeZ/2.0;

					LOG_DEBUG << "geometry position ladder = " << ladder << "\tsensor = " << sensor << "\tcolumn = " << meanColumn << "\trow = " << meanRow << endm;
					LOG_DEBUG << "StMcIstHit local position (after discretization) = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endm;

					StMcTrack *trackMC = mcIstHit->parentTrack();
					Long_t pdgId = trackMC->pdgId();
					Long_t idTruth = trackMC->key();
					LOG_DEBUG << "trackMC pdgId = " << pdgId << "\tidTruth = " << idTruth << endm;

               //
					generateRawHits(mcIstHit);
				}//MC hits loop over
			}//end layer=0 cut
		}//end MC hits number cut
		LOG_DEBUG << "StIstSlowSimMaker::Make() -I- Loaded " << nIsthits << " IST MC raw hits. \n";
	}
	else{
		LOG_DEBUG <<"No Ist MC hits found."<<endm;
	}

	LOG_DEBUG << "Number of IST MC raw hits produced by slow simulator: " << mIstCollectionPtr->getNumRawHits() << endm;

	return kStOK;
}

void StIstSlowSimMaker::Clear( Option_t *opts )
{
	if(mIstCollectionPtr ) {
		for ( UChar_t i = 0; i < kIstNumLadders; ++i ) {
			mIstCollectionPtr->getRawHitCollection(i)->Clear(opts);
		}
   }
   return StMaker::Clear();
}

/**
 * calculate the pad row and column value in IST sensor by using the local position of mc hit in the sensor local coordinates. Origin of local coordinates sits at the corner of the sensor, zPos and rPhiPos are shifted from origin at center to at corner to calculate the row and column. Row is from 1-64, column is from 1-12. 
 *
 **/

void StIstSlowSimMaker::getMCHitRowAndColumn(const StMcIstHit *istMChit, UShort_t &meanColumn, UShort_t &meanRow) const
{
	Double_t localIstHitPos[3]={istMChit->position().x(),istMChit->position().y(),istMChit->position().z()};
	Float_t rPhiPos   = kIstSensorActiveSizeRPhi/2.0 - localIstHitPos[0];
	Float_t zPos      = localIstHitPos[2] + kIstSensorActiveSizeZ/2.0;
   if(rPhiPos<0||zPos<0){
      LOG_WARN<<"StIstSlowSimMaker::getMCHitRowAndColumn Wrong local position rPhiPos = "<<rPhiPos<<" zPos = "<<zPos<<endm;
   }
	meanColumn  = (UShort_t)floor( zPos/kIstPadPitchColumn ) + 1;
	meanRow     = (UShort_t)floor( rPhiPos/kIstPadPitchRow ) + 1;

}

void StIstSlowSimMaker::generateRawHits(const StMcIstHit *istMChit) const
{
	LOG_DEBUG << "StIstSlowSimMaker::generateRawHits - Calculating pad row and column ... " << endm;
	Int_t ladderId = istMChit->ladder();
	Int_t sensorId = istMChit->wafer();
	UShort_t meanColumn, meanRow;
	getMCHitRowAndColumn(istMChit, meanColumn, meanRow);

	Double_t dS = istMChit->dS(); //distance traveled in sensor active volume
	StThreeVectorD midPos(istMChit->position().x(),istMChit->position().y(),istMChit->position().z());
	Double_t mcLocPx = istMChit->localMomentum().x();
	Double_t mcLocPy = istMChit->localMomentum().y();
	Double_t mcLocPz = istMChit->localMomentum().z();

	StThreeVectorD mcLocP(mcLocPx,mcLocPy,mcLocPz);
	Double_t mcLocTot = mcLocP.mag();
	StThreeVectorD mcLocalDir = mcLocP.unit();

	StThreeVectorD inPos  = midPos - mcLocalDir * dS / 2.0;
	StThreeVectorD outPos = midPos + mcLocalDir * dS / 2.0;

	LOG_DEBUG<<"istMcHit geometry position ladder = " << ladderId << "\tsensor = " << sensorId << "\tcolumn = " << meanColumn << "\trow = " << meanRow << endm;
	LOG_DEBUG<<"istMcHit local position  x = "<<midPos.x()<< "\ty = "<<midPos.y()<<"\tz = "<<midPos.z()<<endm;
	LOG_DEBUG<<"istMcHit local position enter  x = "<<inPos.x()<< "\ty = "<<inPos.y()<<"\tz = "<<inPos.z()<<endm;
	LOG_DEBUG<<"istMcHit local position exit  x = "<<outPos.x()<< "\ty = "<<outPos.y()<<"\tz = "<<outPos.z()<<endm;
	LOG_DEBUG<<"istMcHit local momentum  x = "<<mcLocP.x()<< "\ty = "<<mcLocP.y()<<"\tz = "<<mcLocP.z()<<endm;
	LOG_DEBUG<<"istMcHit local direction  x = "<<mcLocalDir.x()<< "\ty = "<<mcLocalDir.y()<<"\tz = "<<mcLocalDir.z()<<endm;
	LOG_DEBUG<<"istMcHit dS = "<<dS<<"\ttotMom = "<<mcLocTot<<endl;

	vector<StThreeVectorD> crossVec;
	LOG_DEBUG<<"transforming to sensor coordinates"<<endm;
	transformToSensor(inPos); 
	transformToSensor(outPos);
	LOG_DEBUG<<"inPos x = "<<inPos.x()<<"\ty = "<<inPos.y()<<"\tz = "<<inPos.z()<<endm;
	LOG_DEBUG<<"outPos x = "<<outPos.x()<<"\ty = "<<outPos.y()<<"\tz = "<<outPos.z()<<endm;

	checkPadCrossing(inPos, outPos, mcLocalDir, dS, crossVec);
	LOG_DEBUG<<"StIstSlowSimMaker::generateRawHits crossVec.size() = "<<crossVec.size()<<endm;
	if(crossVec.size()==1) {
		LOG_WARN<<"StIstSlowSimMaker: McHit is outside of active area -> skip!"<<endm;
		return;
	}

	StThreeVectorD totalPath = crossVec[crossVec.size()-1]-crossVec[0];		
	Double_t pathLengthTotal = totalPath.mag();
	LOG_DEBUG<<"inPos x = "<<crossVec[0].x()<<"\ty = "<<crossVec[0].y()<<"\tz = "<<crossVec[0].z()<<endm;

	//Energy-to-ADC translation factor + signal time bin dependence factor
	//silicon dE/dx = 3.87 Mev/cm, thickness of IST sensor = 300 micros, ADC MIP = 441.0 ADC counts
   const Double_t sidEdx = 3.87; //MeV/cm
   const Double_t sensorThickness = 0.03; //cm
   const Double_t mip = 441.0; //MIP adc
	const Double_t kIstMPV = mip / (sidEdx * 1e-3 * sensorThickness);
	Double_t kIstTimeBinFrac[mCurrentTimeBinNum];

   Int_t maxTB = mCurrentTimeBinNum/2;
   Double_t mean = fAdc->GetParameter(1);
   Double_t sum = fAdc->Integral(mean-0.5-maxTB,mean-0.5+(mCurrentTimeBinNum-maxTB));
	for(Int_t iTB=0; iTB<mCurrentTimeBinNum; iTB++){
      if(fabs(sum)>1e-6){
		   kIstTimeBinFrac[iTB] = fAdc->Integral(mean-0.5-maxTB+iTB,mean-0.5-maxTB+iTB+1)/sum; //time-bin dependence
      }else{
         LOG_WARN<<"StIstSlowSimMaker: integral of adc spectrum = 0"<<endm;
      }
   }

	StIstRawHitCollection *istRawHitCollection =  mIstCollectionPtr->getRawHitCollection(ladderId-1);
	if(istRawHitCollection) {
		for (UInt_t i = 0; i < crossVec.size()-1; ++i) {
			LOG_DEBUG<<"i = "<<i+1<<"\tpos x = "<<crossVec[i+1].x()<<"\ty = "<<crossVec[i+1].y()<<"\tz = "<<crossVec[i+1].z()<<endm;
			StThreeVectorD path = crossVec[i+1]-crossVec[i];
			StThreeVectorD meanPos = (crossVec[i+1]+crossVec[i])/2.0;

			Double_t pathLength = path.mag();
			Double_t rPhiPos_mean, zPos_mean; //unit: cm
			findPad(meanPos, meanColumn, meanRow, rPhiPos_mean, zPos_mean);

         //Nrow = 64 = kIstNumApvChannels/2, Ncolumn = 12 = kIstNumApvsPerArm/2
         //One APV reads 64 rows x 12 columns
			Int_t geoId = (ladderId-1)*kIstNumApvChannels*kIstApvsPerLadder+ (sensorId-1)*(kIstNumApvsPerArm/2)*(kIstNumApvChannels/2) + (meanColumn-1)*kIstNumApvChannels/2 + meanRow;
			Int_t elecId = mMappingGeomVec[geoId-1];
			LOG_DEBUG << "elecID = " << elecId << "\tgeoId = " << geoId << endm;

			Int_t idTruth = istMChit->parentTrack()->key();

			StThreeVectorD normal(0,1,0);
			Float_t angleCorr = fabs(mcLocalDir.dot(normal));
			LOG_DEBUG<<"incident angle w.r.t. sensor surface normal direction: "<<angleCorr<<endm;

			StIstRawHit *rawHit = istRawHitCollection->getRawHit(elecId);

         // calculate ADC by dE for every timebin
			for (UChar_t t = 0; t < mCurrentTimeBinNum; t++) {
				Float_t adcSum = 0.0;
				if(rawHit->getChannelId()>=0) {
					adcSum += rawHit->getCharge(t);
				}

				LOG_DEBUG<<"dE = "<<1e6*istMChit->dE()<<" keV \tpathLength = "<<pathLength<<"\tpathLengthTotal = "<<pathLengthTotal<<endm;
				Float_t charge = 0;
				if(pathLengthTotal>1e-6) charge = istMChit->dE() * pathLength / pathLengthTotal;
				if(kIstTimeBinFrac[maxTB]>0) charge *= kIstTimeBinFrac[t]*kIstMPV/kIstTimeBinFrac[maxTB]; //translate energy Int_to ADC with GeV-to-ADC factor
				if ( charge > adcSum ) {
					rawHit->setIdTruth(idTruth);
				}
				adcSum += charge; 

				rawHit->setCharge(adcSum, t);
				LOG_DEBUG<<"charge = "<<adcSum<<"\tat TB"<<(Int_t)t<<endm;
			}
			rawHit->setChannelId( elecId );
			rawHit->setGeoId( geoId );
			rawHit->setDefaultTimeBin( mDefaultTimeBin );

			LOG_DEBUG << "rawHitPosition ladder = " <<(Int_t)rawHit->getLadder()<< "\tsensor = " <<(Int_t)rawHit->getSensor()<<"\tcolumn = "<<(Int_t)rawHit->getColumn()<<"\trow = "<<(Int_t)rawHit->getRow()<< endm;
		}
	}
	else {
		LOG_WARN << "StIstSlowSimMaker: No rawHitCollection found for ladder " << ladderId << endm;
	}
}

/**
 * check if hit crosses a pad, return the in and out position for the pad.
 **/

void StIstSlowSimMaker::checkPadCrossing(const StThreeVectorD inPos, const StThreeVectorD outPos, StThreeVectorD mcLocalDir, Double_t dS, vector<StThreeVectorD> &cross_vec) const
{
	UShort_t column;
	UShort_t row;
	UShort_t column_out;
	UShort_t row_out;
	Double_t rPhiPosMean;
	Double_t zPosMean;
	Double_t rPhiPosMean_out;
	Double_t zPosMean_out;

   cross_vec.clear();
	cross_vec.push_back(inPos);
	StThreeVectorD current = inPos;

	findPad(outPos, column_out, row_out, rPhiPosMean_out, zPosMean_out);
	findPad(inPos, column, row, rPhiPosMean, zPosMean);

	LOG_DEBUG<<"StIstSlowSimMaker::checkPadCrossing"<<endm;
	LOG_DEBUG<<"\tcolumn_out = "<<column_out<<"\trow_out = "<<row_out<<endm;
	LOG_DEBUG<<"\tcolumn_in = "<<column<<"\trow_in = "<<row<<endm;

	if(column==65535||column_out==65535) return;

	mcLocalDir.setX(-mcLocalDir.x());

	Short_t row_dist = abs(row-row_out);
	Short_t column_dist = abs(column-column_out);

	LOG_DEBUG<<"StIstSlowSimMaker::checkPadCrossing column_dist = "<<column_dist<<"\trow_dist = "<<row_dist<<endm;

	StThreeVectorD mean(rPhiPosMean, 0.0, zPosMean);

        unsigned short row_next = row;
        unsigned short column_next = column;

	while(row_dist>=1 || column_dist>=1)
	{
		StThreeVectorD distance;

		StThreeVectorD halfPad(direction(mcLocalDir.x())*kIstPadPitchRow/2.0,
		direction(mcLocalDir.y())*scaleFromYvsX(mcLocalDir, kIstPadPitchRow/2.0),
		direction(mcLocalDir.z())*kIstPadPitchColumn/2.0);

		LOG_DEBUG<<"current x = "<<current.x()<<"\ty = "<<current.y()<<"\tz = "<<current.z()<<endm;

		distanceToBorder(current, mcLocalDir, mean, distance);
		LOG_DEBUG<<"current x = "<<current.x()<<"\ty = "<<current.y()<<"\tz = "<<current.z()<<endm;

		current+=distance;
		StThreeVectorD current_next = current+distance/100.0;
		LOG_DEBUG<<"current_next x = "<<current_next.x()<<"\ty = "<<current_next.y()<<"\tz = "<<current_next.z()<<endm;

//		findPad(current_next, column, row, rPhiPosMean, zPosMean);
                findPad(current_next, column_next, row_next, rPhiPosMean, zPosMean);
                
                if(row_next==row && column_next==column) {
                   LOG_WARN << " distance calculating not yielding to the next row/column! Break! " << endm;
                   break;
                }
                
                row = row_next;
                column = column_next;
		
                cross_vec.push_back(current);
		mean = StThreeVectorD(rPhiPosMean, 0.0, zPosMean);
		row_dist = abs(row-row_out);
		column_dist = abs(column-column_out);
	}

	cross_vec.push_back(outPos);
}

/**
 * get pad row and column values, and the local rphi and z. Use sensor corner as (0,0)
 **/

void StIstSlowSimMaker::findPad(const StThreeVectorD hitPos, UShort_t &column, UShort_t &row, Double_t &rPhiPos_mean, Double_t &zPos_mean) const
{
	Double_t rPhiPos   = hitPos.x();
	Double_t zPos      = hitPos.z();
   //allow +/- 0.5 pad at edge
   if(rPhiPos<-kIstPadPitchRow/2.||rPhiPos>kIstSensorActiveSizeRPhi+kIstPadPitchRow/2.||zPos<-kIstPadPitchColumn/2.||zPos>kIstSensorActiveSizeZ+kIstPadPitchColumn/2.){
      LOG_ERROR<<"StIstSlowSimMaker::findPad Wrong local position rPhiPos = "<<rPhiPos<<" zPos = "<<zPos<<endm;
      column = row = rPhiPos_mean = zPos_mean = 65535;
      return;
   }
	row     = (UShort_t)floor( rPhiPos/kIstPadPitchRow ) + 1;
   if(row<1) row = 1;
   if(row>kIstNumRowsPerSensor) row = kIstNumRowsPerSensor;
	column  = (UShort_t)floor( zPos/kIstPadPitchColumn ) + 1;
   if(column<1) column = 1;
   if(column>kIstNumRowsPerSensor) column = kIstNumColumnsPerSensor;
	rPhiPos_mean = (row-1) * kIstPadPitchRow + 0.5 * kIstPadPitchRow; //unit: cm
	zPos_mean    = (column-1) * kIstPadPitchColumn + 0.5 * kIstPadPitchColumn; //unit: cm
}

void StIstSlowSimMaker::transformToSensor(StThreeVectorD &hitPos) const
{
	hitPos.setX(kIstSensorActiveSizeRPhi/2.0 - hitPos.x());
	hitPos.setZ(hitPos.z() + kIstSensorActiveSizeZ/2.0);
}

Double_t StIstSlowSimMaker::distanceToBorder(const StThreeVectorD hitPos, const StThreeVectorD dir, const StThreeVectorD mean, StThreeVectorD &dist) const
{
	StThreeVectorD halfPad(kIstPadPitchRow/2.0, 0.0150, kIstPadPitchColumn/2.0);
	LOG_DEBUG<<"distanceToBorder() mean x = "<<mean.x()<<"\ty = "<<mean.y()<<"\tz = "<<mean.z()<<endm;

	// yz plane at x
	Double_t plane_x = mean.x()+direction(dir.x())*halfPad.x();
	Double_t diff_x = plane_x-hitPos.x();

	// xy plane at z
	Double_t plane_z = mean.z()+direction(dir.z())*halfPad.z();
	Double_t diff_z = plane_z-hitPos.z();

	LOG_DEBUG<<"yz plane x = "<<plane_x<<"\txy plane z = "<<plane_z<<endm;
	LOG_DEBUG<<"diff_x = "<<diff_x<<"\tdiff_z = "<<diff_z<<endm;
	LOG_DEBUG<<"dir x = "<<dir.x()<<"\ty = "<<dir.y()<<"\tz = "<<dir.z()<<endm;

	if(fabs(dir.x())<1e-6)
	{
		dist.setZ(diff_z);
		dist.setY(scaleFromYvsZ(dir, diff_z));
		return dist.mag();	// 3D distance
	}
	if(fabs(dir.z())<1e-6)
	{
		dist.setX(diff_x);
		dist.setY(scaleFromYvsX(dir, diff_x));
		return dist.mag();	// 3D distance
	}
	if(diff_x/dir.x()<=diff_z/dir.z())
	{
		dist.setX(diff_x);
		dist.setZ(diff_x*dir.z()/dir.x());
	}
	else
	{
		dist.setX(diff_z*dir.x()/dir.z());
		dist.setZ(diff_z);
	}

	dist.setY(scaleFromYvsX(dir, dist.x()));
	LOG_DEBUG<<"dist x = "<<dist.x()<<"\ty = "<<dist.y()<<"\tz = "<<dist.z()<<endm;
	return dist.mag();	// 3D distance
}


Double_t StIstSlowSimMaker::direction(const Double_t x) const
{
   return (x>0) - (x<0);
}

Double_t StIstSlowSimMaker::scaleFromYvsX(const StThreeVectorD vec, const Double_t a) const
{
	if(fabs(vec.x())>1e-6) return a*vec.y()/vec.x(); /// scale a by y/x
   else{ 
      LOG_WARN<<"StIstSlowSimMaker::scaleFromYvsX x component is 0"<<endm;
      return 0.;
   }
}

Double_t StIstSlowSimMaker::scaleFromYvsZ(const StThreeVectorD vec, const Double_t a) const
{
   if(fabs(vec.z())>1e-6) return a*vec.y()/vec.z(); // scale a by y/z
   else{ 
      LOG_WARN<<"StIstSlowSimMaker::scaleFromYvsZ z component is 0"<<endm;
      return 0.;
   }
}
