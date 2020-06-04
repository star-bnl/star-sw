/*
 *
 * Author: M. Mustafa
 *
 *
 **********************************************************
 * $Log: StPxlDigmapsSim.cxx,v $
 * Revision 1.6  2018/03/16 06:40:01  dongx
 * Suppress warning output
 *
 * Revision 1.5  2018/03/15 21:37:42  dongx
 * Added the single hit efficiency loaded from pxlSimPar table
 *
 * Revision 1.4  2017/11/08 23:14:51  smirnovd
 * StPxlDigmapsSim: Remove pointless pointer validation
 *
 * Revision 1.3  2017/11/08 23:14:44  smirnovd
 * StPxlDigmapsSim: Initialize member pointer in constructor
 *
 * Revision 1.2  2017/10/19 19:38:17  jeromel
 * Merging PXL201709UPD back to MAIN
 *
 * Revision 1.1.2.3  2017/09/22 19:29:56  dongx
 * Review comments from Victor implemented
 *
 * Revision 1.1.2.2  2017/09/20 21:13:02  dongx
 * Review comments from Jason implemented
 *   - Int_t -> int etc.
 *   - documentation
 *   - random generator using STAR standard
 *
 * Revision 1.1.2.1  2017/09/11 20:15:14  dongx
 * Pxl slow simulator added
 *
 */

#include <stdio.h>

#include "StMessMgr.h"
#include "Stypes.h"
#include "StPxlDigmapsSim.h"
#include "StMcEvent/StMcPxlHit.hh"
#include "StMcEvent/StMcPxlHitCollection.hh"
#include "StPxlDbMaker/StPxlDb.h"
#include "tables/St_pxlControl_Table.h"
#include "tables/St_pxlDigmapsSim_Table.h"
#include "tables/St_pxlSimPar_Table.h"
#include "tables/St_HitError_Table.h"
#include "StPxlRawHitMaker/StPxlRawHitCollection.h"
#include "StPxlRawHitMaker/StPxlRawHit.h"

#include "TDataSet.h"
#include "TMath.h"
#include "TRandom3.h"
#include "TDataSet.h"
#include "TObjectSet.h"
#include "TF1.h"
#include "TVector3.h"

#include "DIGMAPS/digplane.h"
#include "DIGMAPS/digadc.h"
#include "DIGMAPS/digtransport.h"
#include "DIGMAPS/digparticle.h"
#include "DIGMAPS/digevent.h"

StPxlDigmapsSim::StPxlDigmapsSim(const Char_t *name): StPxlISim(name),
  mRndGen(nullptr), mOwnRndSeed(0), mDigPlane(new DIGPlane()), mDigAdc(new DIGADC()), mDigTransport(new DIGTransport()),
  mPxlDb(nullptr),
  mdEdxvsBGNorm(nullptr), mHitEffMode(0), mMomCut(0.), mHitEffInner(1.0), mHitEffOuter(1.0)
{}

StPxlDigmapsSim::~StPxlDigmapsSim()
{
  delete mRndGen;
  delete mDigAdc;
  delete mDigPlane;
  delete mDigTransport;
  delete mPxlDb;
  delete mdEdxvsBGNorm;
}

int StPxlDigmapsSim::initRun(const TDataSet& calib_db, const TObjectSet* pxlDbDataSet, const Int_t run)
{
  LOG_INFO << "StPxlDigmapsSim::init()" << endm;

  mRndGen = (TRandom3 *)gRandom;
  if ( 0 == mRndGen || mOwnRndSeed > 0 ) mRndGen = new TRandom3(mOwnRndSeed);

  if (pxlDbDataSet != 0)
  {
    mPxlDb = (StPxlDb *)pxlDbDataSet->GetObject();
    if (!mPxlDb)
    {
      LOG_ERROR << "StPxlDigmapsSim - E - mPxlDb is not available" << endm;
      return kStFatal;
    }
  }
  else
  {
    LOG_ERROR << "StPxlDigmapsSim - E - no PxlDb" << endm;
    return kStFatal;
  }

  pxlControl_st const* const pxlControlTable = mPxlDb->pxlControl();
  mSensorGoodStatusMin = pxlControlTable[0].sensorGoodStatusMin;
  mSensorGoodStatusMax = pxlControlTable[0].sensorGoodStatusMax;
  mRowColumnGoodStatus = pxlControlTable[0].rowColumnGoodStatus;

  pxlDigmapsSim_st const* const pxlDigmapsSimTable = mPxlDb->pxlDigmapsSim();
  // set ADC threshold(s)
  int nAdcBits = 1;
  int nAdcThresholds = int(TMath::Power(2.0, nAdcBits) - 1);
  Bool_t adcLinear = 0;
  float adcElectronConversion = -999;
  float adcThresholds[] = {pxlDigmapsSimTable[0].adcThreshold}; // one threshold only
  float adcLsb = adcThresholds[0];

  mDigAdc->SetNbits(nAdcBits);
  mDigAdc->SetNThresholds(nAdcThresholds);
  mDigAdc->SetADC_linear(adcLinear);
  mDigAdc->SetLSB(adcLsb);
  mDigAdc->SetElectron_Conversion(adcElectronConversion);
  mDigAdc->SetADC_thresholds(adcThresholds, nAdcThresholds);

  // set transport
  int transportChargeModel = (int)pxlDigmapsSimTable[0].transportChargeModel;
  float   transportRangeLimit_InPitchUnit = pxlDigmapsSimTable[0].transportRangeLimit;
  float   transport_l1dimgauslor_Norm_g_1st = pxlDigmapsSimTable[0].transport_Norm_g_1st;
  float   transport_l1dimgauslor_x0_g_1st = pxlDigmapsSimTable[0].transport_x0_g_1st;
  float   transport_l1dimgauslor_sigma_g_1st = pxlDigmapsSimTable[0].transport_sigma_g_1st;
  float   transport_l1dimgauslor_Gamma_lor_1st = pxlDigmapsSimTable[0].transport_Gamma_lor_1st;
  float   transport_l1dimgauslor_x0_lor_1st = pxlDigmapsSimTable[0].transport_x0_lor_1st;
  float   transport_l1dimgauslor_norm_lor_1st = pxlDigmapsSimTable[0].transport_norm_lor_1st;
  float   transport_l1dimgauslor_Norm_g_2nd = pxlDigmapsSimTable[0].transport_Norm_g_2nd;
  float   transport_l1dimgauslor_x0_g_2nd = pxlDigmapsSimTable[0].transport_x0_g_2nd;
  float   transport_l1dimgauslor_sigma_g_2nd = pxlDigmapsSimTable[0].transport_sigma_g_2nd;
  float   transport_l1dimgauslor_Gamma_lor_2nd = pxlDigmapsSimTable[0].transport_Gamma_lor_2nd;
  float   transport_l1dimgauslor_x0_lor_2nd = pxlDigmapsSimTable[0].transport_x0_lor_2nd;
  float   transport_l1dimgauslor_norm_lor_2nd = pxlDigmapsSimTable[0].transport_norm_lor_2nd;

  mDigTransport->SetChargeModel(transportChargeModel);
  mDigTransport->SetRangeLimit_InPitchUnit(transportRangeLimit_InPitchUnit);
  mDigTransport->Setf1dimgauslor_Norm_g_1st(transport_l1dimgauslor_Norm_g_1st);
  mDigTransport->Setf1dimgauslor_x0_g_1st(transport_l1dimgauslor_x0_g_1st);
  mDigTransport->Setf1dimgauslor_sigma_g_1st(transport_l1dimgauslor_sigma_g_1st);
  mDigTransport->Setf1dimgauslor_Gamma_lor_1st(transport_l1dimgauslor_Gamma_lor_1st);
  mDigTransport->Setf1dimgauslor_x0_lor_1st(transport_l1dimgauslor_x0_lor_1st);
  mDigTransport->Setf1dimgauslor_norm_lor_1st(transport_l1dimgauslor_norm_lor_1st);
  mDigTransport->Setf1dimgauslor_Norm_g_2nd(transport_l1dimgauslor_Norm_g_2nd);
  mDigTransport->Setf1dimgauslor_x0_g_2nd(transport_l1dimgauslor_x0_g_2nd);
  mDigTransport->Setf1dimgauslor_sigma_g_2nd(transport_l1dimgauslor_sigma_g_2nd);
  mDigTransport->Setf1dimgauslor_Gamma_lor_2nd(transport_l1dimgauslor_Gamma_lor_2nd);
  mDigTransport->Setf1dimgauslor_x0_lor_2nd(transport_l1dimgauslor_x0_lor_2nd);
  mDigTransport->Setf1dimgauslor_norm_lor_2nd(transport_l1dimgauslor_norm_lor_2nd);

  // set plane
  float planePitchX = StPxlConsts::kPixelSize * 1e4;
  float planePitchY = StPxlConsts::kPixelSize * 1e4;
  float planeEpitaxialThickness = pxlDigmapsSimTable[0].planeEpitaxialThickness;
  float planeNoiseElectrons = pxlDigmapsSimTable[0].planeNoiseElectrons;
  int planeNpixelsX = kNumberOfPxlRowsOnSensor;   // row - local X - DIGMAPS X
  int planeNpixelsY = kNumberOfPxlColumnsOnSensor; // column - local z - DIGMAPS Y
  float planeTemprature = pxlDigmapsSimTable[0].planeTemprature;
  float planeIonizationEnergy = pxlDigmapsSimTable[0].planeIonizationEnergy;
  float planeSegmentSize = pxlDigmapsSimTable[0].planeSegmentSize;
  float planeMaximumSegmentSize = pxlDigmapsSimTable[0].planeMaxSegmentSize;
  float planeMaximumChargePerSegment = pxlDigmapsSimTable[0].planeMaxChargePerSegment;
  float planeDiffusionMaximumRangeInX = pxlDigmapsSimTable[0].planeDiffusionMaxX;
  float planeDiffusionMaximumRangeInY = pxlDigmapsSimTable[0].planeDiffusionMaxY;
  float planeReflexionCoefficient = pxlDigmapsSimTable[0].planeReflexCoefficient;
  float planeBasicModel_SigmaTenMicrons = pxlDigmapsSimTable[0].planeMod_SigTenMicrons;

  mDigPlane->SetPitch(planePitchX, planePitchY);
  mDigPlane->SetNpixels(planeNpixelsX, planeNpixelsY);
  mDigPlane->SetDimensions(planePitchX * planeNpixelsX, planePitchY * planeNpixelsY, planeEpitaxialThickness);
  mDigPlane->SetNoiseElectrons(planeNoiseElectrons);
  mDigPlane->SetTemperature(planeTemprature);
  mDigPlane->SetIonizationEnergy(planeIonizationEnergy);
  mDigPlane->SetSegmentSize(planeSegmentSize);
  mDigPlane->SetMaximumSegmentSize(planeMaximumSegmentSize);
  mDigPlane->SetMaximumChargePerSegment(planeMaximumChargePerSegment);
  mDigPlane->SetDiffusionMaximumRange(planeDiffusionMaximumRangeInX, planeDiffusionMaximumRangeInY);
  mDigPlane->SetReflexionCoefficient(planeReflexionCoefficient);
  mDigPlane->SetBasicModel_SigmaTenMicrons(planeBasicModel_SigmaTenMicrons);

  mEnergyLandauMean = pxlDigmapsSimTable[0].energyLandauMean;
  mEnergyLandauSigma = pxlDigmapsSimTable[0].energyLandauSigma;
  for(int i=0;i<10;i++) {
    mScalePar[i] = (double)(pxlDigmapsSimTable[0].par[i]);
  }
  mdEdxvsBGNorm = new TF1("dEdxvsBGNorm",this,&StPxlDigmapsSim::dEdxvsBGNorm,0.1,1e5,6);
  mdEdxvsBGNorm->SetParameters(&mScalePar[0]);

  mResAddX = pxlDigmapsSimTable[0].resAddX;
  mResAddZ = pxlDigmapsSimTable[0].resAddZ;
  
  // initialize the sensor offset values due to other contributions
  for(int i=0;i<kNumberOfPxlSectors;i++) {
    for(int j=0;j<kNumberOfPxlLaddersPerSector;j++) {
      for(int k=0;k<kNumberOfPxlSensorsPerLadder;k++) {
        mOffsetX[i][j][k] = mRndGen->Gaus(0.,mResAddX);
        mOffsetZ[i][j][k] = mRndGen->Gaus(0.,mResAddZ);
      }
    }
  }
  
  // MC->RC hit efficiency (momentum dependence - default)
  pxlSimPar_st const* const pxlSimParTable = mPxlDb->pxlSimPar();
  mHitEffMode = pxlSimParTable[0].mode;
  mMomCut = pxlSimParTable[0].pCut;
  mHitEffInner = pxlSimParTable[0].effPxlInner; // best knowledge from ZF cosmic ray study - inner tunable
  mHitEffOuter = pxlSimParTable[0].effPxlOuter; // best knowledge from ZF cosmic ray study - checked for outer
  
  LOG_INFO << " PXL MC hit efficiency mode used for PXL slow simulator: " << mHitEffMode << endm;
  LOG_INFO << "     +++ Hit Efficiency at p > " << mMomCut << " GeV/c (Inner/Outer) = " << mHitEffInner << "/" << mHitEffOuter << endm; 
      
  return kStOk;
}
//____________________________________________________________
int StPxlDigmapsSim::addPxlRawHits(const StMcPxlHitCollection& mcPxlHitCol,
                                     StPxlRawHitCollection& pxlRawHitCol)
{
  for (unsigned int iSec = 0; iSec < mcPxlHitCol.numberOfSectors(); ++iSec)
  {
    const StMcPxlSectorHitCollection* mcPxlSectorHitCol = mcPxlHitCol.sector(iSec);
    if (!mcPxlSectorHitCol) continue;

    for (unsigned int iLad = 0; iLad < mcPxlSectorHitCol->numberOfLadders(); ++iLad)
    {
      const StMcPxlLadderHitCollection* mcPxlLadderHitCol = mcPxlSectorHitCol->ladder(iLad);
      if (!mcPxlLadderHitCol) continue;

      for (unsigned int iSen = 0; iSen < mcPxlLadderHitCol->numberOfSensors(); ++iSen)
      {
        const StMcPxlSensorHitCollection* mcPxlSensorHitCol = mcPxlLadderHitCol->sensor(iSen);
        if (!mcPxlSensorHitCol) continue;

        if (!goodSensor(iSec+1, iLad+1, iSen+1))
        {
          LOG_DEBUG << " ##Skip bad sensor " << iSec << "/" << iLad << "/" << iSen << " StatusCode = " << mPxlDb->sensorStatus(iSec + 1, iLad + 1, iSen + 1) << endm;
          continue;
        }

        unsigned int nSenHits = mcPxlSensorHitCol->hits().size();
        LOG_DEBUG << "Sector/Ladder/Sensor = " << iSec+1 << "/" << iLad+1 << "/" << iSen+1 << ". Number of sensor hits = " << nSenHits << endm;

        // Loop over hits in the sensor
        for (unsigned int iHit = 0; iHit < nSenHits; ++iHit)
        {
          StMcPxlHit const* const mcPix = mcPxlSensorHitCol->hits()[iHit];
          if (!mcPix) continue;

          float hitEff = 1.0;
          switch (mHitEffMode) {
            case 0:    // ideal case 100% efficiency
                    break;
            case 1:    // momemum dependent efficiency
              if(mcPix->parentTrack()) {
                float const ptot = mcPix->parentTrack()->momentum().mag();
                if( iLad==0 ) { // linear dependence 0 at p=0 and minimum at p=mMomCut
                  hitEff = 1.0 - (1. - mHitEffInner)*ptot/mMomCut;
                  if(hitEff<mHitEffInner) hitEff = mHitEffInner;
                } else {
                  hitEff = 1.0 - (1. - mHitEffOuter)*ptot/mMomCut;
                  if(hitEff<mHitEffOuter) hitEff = mHitEffOuter;
                }
              }
                    break;
            case 2:    // constant efficiency
              if( iLad==0 ) {
                hitEff = mHitEffInner;
              } else {
                hitEff = mHitEffOuter;
              }
                    break;
            default:
                    break;
          }
          
          if( mRndGen->Rndm()>hitEff ) continue;

          int sensorId = ( iSec * kNumberOfPxlLaddersPerSector + iLad ) * kNumberOfPxlSensorsPerLadder + iSen + 1;
          DIGEvent fdigevent{};
          fillDigmapsEvent(sensorId, mcPix, fdigevent);
          
          int n_pxl = 0; // number of pixels passing digQ threshold
          int n_pxl_wmask = 0; // number of pixels passing digQ threshold and with mask
          for (int j = 0; j < fdigevent.GetReadoutmap()->GetNpixels(); ++j)
          {
            if(fdigevent.GetReadoutmap()->GetDigitalCharge()[j] > 0)
            {
              ++n_pxl;
              int const Npixel = fdigevent.GetReadoutmap()->GetPixelMap()[j];
              int const iy = Npixel / mDigPlane->GetNpixelsX();
              int const ix = (mDigPlane->GetNpixelsX()-1) - Npixel % mDigPlane->GetNpixelsX();  // local X direction goes from row number MAX->0

              if (goodPixel(iSec+1 , iLad+1, iSen+1, ix, iy))
              {
                ++n_pxl_wmask;
                int const idTruth = mcPix->parentTrack()? mcPix->parentTrack()->key() : 0;
                LOG_DEBUG << "  adding a new pxlRawHit  sector/ladder/sensor = " << iSec + 1 << "/" << iLad + 1 << "/" << iSen + 1 << " ix/iy=" << ix << "/" << iy << " idTruth=" << idTruth << endm;

                pxlRawHitCol.addRawHit(StPxlRawHit(iSec + 1, iLad + 1, iSen + 1, ix, iy, idTruth));
              }
            }
          }

          LOG_DEBUG << " ReadoutMap Npixels = " << fdigevent.GetReadoutmap()->GetNpixels() << "\t Npixels Dig Cut = " << n_pxl << "\t Npixels Mask Cut = " << n_pxl_wmask << endm;
        }  // end for hits
      } // end sensor loop
    } // end ladder loop
  } // end sector loop

  return kStOK;
}

void StPxlDigmapsSim::fillDigmapsEvent(int sensorId, StMcPxlHit const* const mcPix, DIGEvent& fdigevent) const
{
  TVector3 inPos{};
  TVector3 outPos{};
  calculateIncidencePositions(sensorId, mcPix, inPos, outPos);
  float const betagamma = betaGamma(mcPix->parentTrack());
  float const depositedEnergy = calculateDepositedEnergy((inPos-outPos).Mag(), betagamma);
  LOG_DEBUG << " Energy deposit for this hit = " << depositedEnergy << "\t totalLength = " << (inPos-outPos).Mag() << "\t betagamma = " << betagamma << endm;

  DIGParticle fdigparticle(inPos.X(), inPos.Y(), inPos.Z(), outPos.X(), outPos.Y(), outPos.Z(), depositedEnergy);
  LOG_DEBUG << "  InPos = " << inPos.X() << "/" << inPos.Y() << "/" << inPos.Z() << "\t outPos = " << outPos.X() << "/" << outPos.Y() << "/" << outPos.Z() << " E = " << depositedEnergy << endm;

  //---------charge generation
  fdigparticle.ComputeChargeDeposition(mDigPlane->GetSegmentSize(), mDigPlane->GetMaximumSegmentSize(), mDigPlane->GetMaximumChargePerSegment());
  //---------charge transport
  fdigparticle.ComputeChargeTransport(mDigPlane, mDigTransport);
  //---------random noise (should be removed if one wants to avoid double noise on double hit pixels)
  fdigparticle.AddRandomNoise(mDigPlane);
  //---------ADC (stored only for reference)
  fdigparticle.AnalogToDigitalconversion(mDigAdc, mDigPlane);

  fdigevent.AddParticle(fdigparticle);
  auto chargevector = fdigparticle.GetAnalogCharge();
  auto pixmapvector = fdigparticle.GetPixelMap();

  for (int ipix = 0 ; ipix < fdigparticle.GetNpixels() ; ++ipix)
  {
    (fdigevent.GetReadoutmap())->UpdatePixel(chargevector[ipix], pixmapvector[ipix]);
  }

  //---------Build readout map:
  (fdigevent.GetReadoutmap())->AnalogToDigitalconversion(mDigAdc, mDigPlane);

  LOG_DEBUG << " DigPlane NpixX = " << mDigPlane->GetNpixelsX() << "\t DigPlane NpixY = " << mDigPlane->GetNpixelsY() << endm;
}

float StPxlDigmapsSim::calculateDepositedEnergy(float const totalLength, float const betagamma) const
{
  float const energyMPV = mEnergyLandauMean * totalLength;
  float const energySigma = mEnergyLandauSigma * totalLength;
  float energy = mRndGen->Landau(energyMPV, energySigma) * mdEdxvsBGNorm->Eval(betagamma);
  LOG_DEBUG << " energyMPV/Sigma = " << energyMPV << " " << energySigma << "\t betagamma = " << betagamma << " dEdx correction = " << mdEdxvsBGNorm->Eval(betagamma) << endm;

  int count=0;
  while (energy > 50000 && count < 50) // count to avoid infinite loop in case of large energy deposit
  {
    LOG_DEBUG << "Energy too high -> Energy regenerated " << energy << " MPV/sigma= " << energyMPV << " " << energySigma << " betagamma=" << betagamma << " Correction=" << mdEdxvsBGNorm->Eval(betagamma) << "  Seed = " << mRndGen->GetSeed() << endm;
    mRndGen->SetSeed(mRndGen->GetSeed()*mRndGen->GetSeed());
    energy = mRndGen->Landau(energyMPV, energySigma) * mdEdxvsBGNorm->Eval(betagamma);
    count++;
  }
  if(count==50)  
    LOG_WARN << " Failed in Sample: energy= " << energy << " MPV/sigma= " << energyMPV << " " << energySigma << " betagamma=" << betagamma << " Correction=" << mdEdxvsBGNorm->Eval(betagamma) << "  Seed = " << mRndGen->GetSeed() << endm;

  return energy;
}

double StPxlDigmapsSim::dEdxvsBGNorm(double *x, double *par)
{
  static const double threshold = 10.;
  double beta2 = x[0]*x[0]/(1+x[0]*x[0]);
  double delta = TMath::Log(x[0])+par[2];
  if(x[0]<=threshold) {
    return par[0]/beta2*(0.5*TMath::Log(x[0]*x[0]*par[1])-beta2-delta/2.);
  } else {
    return par[3] + par[4]*TMath::Log(x[0])+par[5]*TMath::Log(x[0])*TMath::Log(x[0]);
  }
}

void StPxlDigmapsSim::calculateIncidencePositions(int sensorId, StMcPxlHit const* const mcPix, TVector3& inPos, TVector3& outPos) const
{
  int iSec = (sensorId - 1) / (kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder);
  int iLad = (sensorId - 1) % (kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder) / kNumberOfPxlSensorsPerLadder;
  int iSen = (sensorId - 1) % kNumberOfPxlSensorsPerLadder;
  
  double localPixHitPos[3] = {mcPix->position().x() + mOffsetX[iSec][iLad][iSen], mcPix->position().y(), mcPix->position().z() + mOffsetZ[iSec][iLad][iSen]};
  double localPxlMom[3] = {mcPix->localMomentum().x(), mcPix->localMomentum().y(), mcPix->localMomentum().z()};

  // convert to um (all DIGMAPS units are in um)
  localPixHitPos[0] *= 10000.0;
  localPixHitPos[1] *= 10000.0;
  localPixHitPos[2] *= 10000.0;

  //               LOG_DEBUG << "globalPixHitPos = " << globalPixHitPos[0] << " " << globalPixHitPos[1] << " " << globalPixHitPos[2] << endm;
  LOG_DEBUG << "localPixHitPos = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << "\n";
  LOG_DEBUG << "localPxlMom    = " << localPxlMom[0] << " " << localPxlMom[1] << " " << localPxlMom[2] << "\n";
  LOG_DEBUG << " DigPlane dimensions " << mDigPlane->GetXdimension() << " " << mDigPlane->GetYdimension() << " " << mDigPlane->GetZdimension() << endm;

  inPos.SetX(localPixHitPos[0] + mDigPlane->GetXdimension() / 2.0 + (mDigPlane->GetZdimension() / 2.0 - localPixHitPos[1]) * localPxlMom[0] / localPxlMom[1]);
  inPos.SetY(localPixHitPos[2] + mDigPlane->GetYdimension() / 2.0 + (mDigPlane->GetZdimension() / 2.0 - localPixHitPos[1]) * localPxlMom[2] / localPxlMom[1]);
  inPos.SetZ(mDigPlane->GetZdimension() / 2.0);

  outPos.SetX(localPixHitPos[0] + mDigPlane->GetXdimension() / 2.0 + (-mDigPlane->GetZdimension() / 2.0 - localPixHitPos[1]) * localPxlMom[0] / localPxlMom[1]);
  outPos.SetY(localPixHitPos[2] + mDigPlane->GetYdimension() / 2.0 + (-mDigPlane->GetZdimension() / 2.0 - localPixHitPos[1]) * localPxlMom[2] / localPxlMom[1]);
  outPos.SetZ(-mDigPlane->GetZdimension() / 2.0);

  LOG_DEBUG << "inHitPos = " << inPos.X() << " " << inPos.Y() << " " << inPos.Z() << "\n";
  LOG_DEBUG << "outHitPos = " << outPos.X() << " " << outPos.Y() << " " << outPos.Z() << endm;
}

float StPxlDigmapsSim::betaGamma(StMcTrack const* const mcTrk) const
{
  if (!mcTrk) return 1.;

  float betagamma = 1.;
  float const m = mcTrk->fourMomentum().m();
  if(m>0) betagamma = mcTrk->momentum().mag()/m;
  LOG_DEBUG << " track info: " << mcTrk->momentum().mag() << " " << m << " " << betagamma << endm;
  if(m>1.0) LOG_DEBUG << "  This is a large mass particle: geantId=" << mcTrk->geantId() << " pdgId=" << mcTrk->pdgId() << endm;

  return betagamma;
}

bool StPxlDigmapsSim::goodPixel(int const sec, int const lad, int const sen, int const ix, int const iy) const
{
  // check raw and column status and hot pixels
  return (mPxlDb->rowStatus(sec, lad, sen, ix) == mRowColumnGoodStatus) &&
         (mPxlDb->columnStatus(sec, lad, sen, iy) == mRowColumnGoodStatus) &&
         (!mPxlDb->pixelHot(sec, lad, sen, ix, iy));
}

bool StPxlDigmapsSim::goodSensor(int const sec, int const lad, int const sen) const
{
  return (mPxlDb->sensorStatus(sec, lad, sen) >= mSensorGoodStatusMin) &&
         (mPxlDb->sensorStatus(sec, lad, sen) <= mSensorGoodStatusMax);
}
