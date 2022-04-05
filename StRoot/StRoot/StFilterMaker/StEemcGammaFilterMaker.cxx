// $Id: StEemcGammaFilterMaker.cxx,v 1.4 2016/05/25 21:35:26 jwebb Exp $

// STL
#include <vector>

// ROOT Libraries
#include "TVector3.h"

// StEvent Libraries
#include "StEvent.h"

// StEvent Libraries
#include "StEventTypes.h"

// StMcEvent Libraries
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"

// StEmc Libraries
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/others/emcDetectorName.h"

// Endcap Libraries
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcSimulatorMaker/StEEmcFastMaker.h"

// Class Header
#include "StEemcGammaFilterMaker.h"

#include "tables/St_eemcGammaFilterMakerParams_Table.h"

ClassImp(StEemcGammaFilterMaker)

////////////////////////////////////////////////////////////
//                      Constructor                       //
////////////////////////////////////////////////////////////
StEemcGammaFilterMaker::StEemcGammaFilterMaker():StMaker("eemcGammaFilterMaker.1.00") 
{
  
  LOG_DEBUG << "StEemcGammaFilterMaker()" << endm;
  
  // Set reasonable defaults
  
  mMaxVertex = 120.0;
  
  mSeedEnergyThreshold = 3.8; //2.8; //3.4;//2.8;//2 sigma
  mClusterEtThreshold = 5.0; //3.8; //4.5;// 4.2;
  mEemcSamplingFraction = StEEmcFastMaker::getSamplingFraction();
  
  
  mMomentum = new TVector3();
  
  ////////////////////////////////////////
  //       Calorimeter Geometry       //
  ////////////////////////////////////////
  
  //EEMC Geometry
  mEemcGeom = new   EEmcGeomSimple();
  
  // Zero counters
  mTotal = 0;
  mAccepted = 0;
  mVertexRejected = 0;
  mFilterRejected = 0;
  //     mFilterMode = 1;
  //     mUseDbParams = true;
  mFilterMode = 1;
  mUseDbParams = false;
  
  if(!mFilterMode)
  {
    LOG_INFO <<" StEemcGammaFilterMaker is running in test mode.";
    LOG_INFO <<" set mFilterMode to 1 to run in analysis mode."<<endm;
  }
  
}

////////////////////////////////////////////////////////////
//                       Destructor                       //
////////////////////////////////////////////////////////////
StEemcGammaFilterMaker::~StEemcGammaFilterMaker()
{
  LOG_DEBUG << "~StEemcGammaFilterMaker()" << endm;
  
  // Clean up
  delete mMomentum;
  delete mEemcGeom;
  
}

////////////////////////////////////////////////////////////
//                      Maker Init                        //
////////////////////////////////////////////////////////////
Int_t StEemcGammaFilterMaker::Init()
{
  
  LOG_DEBUG << "Init()" << endm;
  
  // Force the BFC to listen to the filter
  SetAttr(".Privilege", 1);
  
  // Display maker information
  
  if (this->getUseDbParams())
  { // get parameter values from the database
  LOG_INFO << "Getting parameters from the database" << endm;
  TDataSet *DB = this->GetInputDB("Calibrations/emc");
  if (DB)
  {
    St_eemcGammaFilterMakerParams* params = (St_eemcGammaFilterMakerParams*)DB->Find("eemcGammaFilterMakerParams");
    if (params)
    {
      eemcGammaFilterMakerParams_st *p = params->GetTable();
      if (p)
      {
	this->mEemcSamplingFraction = p->eemcSamplingFraction;
	this->mSeedEnergyThreshold = p->seedEnergyThreshold;
	this->mClusterEtThreshold = p->clusterEtThreshold;
	this->mMaxVertex = p->maxVertexZ;
	this->mFilterMode = p->filterMode;
	LOG_DEBUG << "Successfully read parameters from the database table" << endm;
      } 
      else
      {
	LOG_ERROR << "The database table has no entry!" << endl;
	return kStFATAL;
      }
    } 
    else
    {
      LOG_ERROR << "Cannot find the table in the database!" << endm;
      return kStFATAL;
    }
  } 
  else
  {
    LOG_ERROR << "Cannot read the database!" << endm;
    return kStFATAL;
  }
  }//done getting db parameters
  
  LOG_INFO << "StEEmcDbMaker::Init() : Using gamma filter on the EEMC" << endm;
  LOG_INFO << "StEEmcDbMaker::Init() : EEMC Sampling Fraction = " << mEemcSamplingFraction << endm;
  
  LOG_INFO << "StEEmcDbMaker::Init() : Seed energy threshold = " << mSeedEnergyThreshold << " GeV " << endm;
  LOG_INFO << "StEEmcDbMaker::Init() : Cluster eT threshold = " << mClusterEtThreshold << " GeV " << endm;
  LOG_INFO << "StEEmcDbMaker::Init() : Maximum vertex = +/- " << mMaxVertex << " cm" << endm;
  if (!mFilterMode)
    LOG_INFO << "StEEmcDbMaker::Init() : Running the TEST mode (accepting all events). Set mFilterMode=1 to actually reject events in BFC"<< endm;
  
  
  
  // If using the EEMC ensure that a sampling 
  // fraction has been entered by the user 
  if (mEemcSamplingFraction != StEEmcFastMaker::getSamplingFraction())
  {
    LOG_WARN << "StEEmcDbMaker::Init() : EEMC Sampling fraction (" << mEemcSamplingFraction << ") is different from what is in the Fast Simulator (" << StEEmcFastMaker::getSamplingFraction() <<")" << endm;
  }
  
  
  return kStOK;
  
}

////////////////////////////////////////////////////////////
//                     Maker Clear                        //
////////////////////////////////////////////////////////////    
void StEemcGammaFilterMaker::Clear(const Option_t*)
{
  LOG_DEBUG << "Clear()" << endm;
  
  // Clear the StEmcRawHit arrays
  
  double *point2 = mEemcTowerHits;
  for(unsigned int i = 0; i < nEemcTowers; ++i)
  {
    *point2 = 0;
    ++point2;
  }
  
  
  
}



////////////////////////////////////////////////////////////
//       Set maximum vertex allowed by the filter         //
////////////////////////////////////////////////////////////
void StEemcGammaFilterMaker::setMaxVertex(double maxVertex)
{
  LOG_DEBUG << "setMaxVertex()" << endm;
  mMaxVertex = maxVertex;
}

////////////////////////////////////////////////////////////
//                 Set filter thresholds                  //
////////////////////////////////////////////////////////////
void StEemcGammaFilterMaker::setThresholds(double seed, double cluster)
{
  
  LOG_DEBUG << "setThresholds()" << endm;
  
  if(seed > 0) mSeedEnergyThreshold = seed;
  if(cluster > 0) mClusterEtThreshold = cluster;
  
}

////////////////////////////////////////////////////////////
//               Set EEMC sampling fraction               //
////////////////////////////////////////////////////////////
void StEemcGammaFilterMaker::setEEMCSamplingFraction(double f)
{
  mEemcSamplingFraction = f;
}


////////////////////////////////////////////////////////////
//                      Apply filter                      //
////////////////////////////////////////////////////////////
Int_t StEemcGammaFilterMaker::Make()
{
  
  LOG_DEBUG << "Make()" << endm;
  
  // Acquire StEvent from the chain
  StEvent *mEvent = static_cast<StEvent*>(GetDataSet("StEvent"));
  
  // Acquire StMcEvent from the chain
  StMcEvent *mMcEvent = static_cast<StMcEvent*>(GetDataSet("StMcEvent"));
  
  ++mTotal;
  
  // Store the first event to ensure that a MuDst is created
  // in the case of no events passing the filter
  //     if(mEvent->id() == 1)
  //     {
    //         LOG_INFO << "Make() : Storing first event without applying filter" << endm;   
    //         
    //         ++mAccepted;
    //         return kStOK;
    //     }
    
    // Check for functional StMcEvent
    if(!mMcEvent)
    {
      LOG_WARN << "Reject() : Bad StMcEvent!" << endm;
      return kStWarn;
    }
    
    // Acquire vertex information from the geant record
    // and abort the event if the vertex is too extreme
    double zVertex = mMcEvent->primaryVertex()->position().z();
    
    if(fabs(zVertex) > mMaxVertex)
    {
      LOG_INFO << "Reject() : Aborting Event " << mEvent->id()
      << " due to extreme geant vertex of " << zVertex << " cm " << endm;
      
      ++mVertexRejected;
      if(mFilterMode)
	return kStSKIP;
      else
	return kStOK;
      
    }
    
    // Look for clusters in the chosen calorimeter
    Int_t status = 0;
    
    // Pick up calorimeter information from StEvent
    if (mEvent) {
      StEmcCollection *emcCollection = mEvent->emcCollection();
      status = makeEEMC(emcCollection, zVertex);
    }
    
    if(status == kStSKIP)
    {
      ++mFilterRejected;
      LOG_INFO << "Make() : Aborting Event " << mEvent->id() << " due to gamma filter rejection" << endm;
      LOG_DEBUG << "Make() : Vertex = " << zVertex << " (cm)" << endm;
    }
    
    if(status == kStOK)
    {
      ++mAccepted;
//    LOG_INFO << "Make() : Event " << mEvent->id() << " accepted!" << endm; // Reduce noise and coverity complaint about possible NULL ptr
    }
    
    if (mFilterMode==0) status = kStOK;
    return status;
    
}


////////////////////////////////////////////////////////////
//       Construct crude EEMC clusters of 3x3 towers,     // 
//        returning kStOK if a satisifactory cluster      // 
//             is found and kStSKIP otherwise             //
////////////////////////////////////////////////////////////
Int_t StEemcGammaFilterMaker::makeEEMC(StEmcCollection *emcCollection, double zVertex)
{
  
  LOG_DEBUG << "makeEEMC()" << endm;
  
  //////////////////////////////
  //   Instantiate variables  //
  //////////////////////////////
  
  unsigned int nSub = 5;
  int eta = 0;
  int phi = 0;
  
  double clusterEnergy = 0;
  double clusterEt = 0;
  double seedEnergy = 0;
  
  int neighborEta = 0;
  int neighborPhi = 0;
  
  // Access EEMC information
  StEmcDetector* eemc = emcCollection ? emcCollection->detector(kEndcapEmcTowerId) : 0;  
  if(!eemc) return kStSKIP;
  
  //////////////////////////////
  //    Fill mEemcTowerHits   //
  //       with hit data      //
  //////////////////////////////
  for(unsigned int m = 1; m < eemc->numberOfModules() + 1; ++m)
  {
    
    StEmcModule *module = eemc->module(m);
    if (module) {
      StSPtrVecEmcRawHit &rawHits = module->hits();
      
      for(unsigned int l = 0 ; l < rawHits.size(); ++l)
      {
	
	StEmcRawHit *tempHit = rawHits[l];
	if (tempHit) {
	  eta = tempHit->eta() - 1;
	  phi = (m - 1) * nSub + tempHit->sub() - 1;
	  mEemcTowerHits[eta * nEemcPhiBins + phi] = tempHit->energy() / mEemcSamplingFraction;
	}
      }
    }
    
  }
  
  //////////////////////////////
  //    Search for clusters   //
  //////////////////////////////
  
  for(unsigned int e = 0; e < nEemcEtaBins; ++e)
  {
    
    for(unsigned int p = 0; p < nEemcPhiBins; ++p)
    {
      
      clusterEnergy = 0;
      
      seedEnergy = mEemcTowerHits[e * nEemcPhiBins + p];
      
      // Start with a seed tower...
      if(seedEnergy > mSeedEnergyThreshold)
      {
	
	clusterEnergy = seedEnergy;
	
	// and accumulate energy from the neighboring towers
	for(int i = -1; i < 2; ++i)
	{
	  
	  for(int j = -1; j < 2; ++j)
	  {
	    
	    if( !i && !j ) 
	    {
	      LOG_DEBUG << "\t\tTower (eta, phi) = (" << e << ", " << p 
	      << "), E = " << seedEnergy << " (Seed)" << endm;
	      continue;
	    }
	    
	    neighborEta = e + i;
	    if(neighborEta < 0) continue;
	    if(neighborEta > int(nEemcEtaBins - 1)) continue;
	    
	    neighborPhi = (p + j) % nEemcPhiBins;
//	    if(neighborPhi == - 1) neighborPhi = nEemcPhiBins - 1; // Cannot be < 0 after % on previous line
	    LOG_DEBUG << "\t\tTower (eta, phi) = (" << neighborEta << ", " << neighborPhi 
	    << "), E = " << mEemcTowerHits[neighborEta * nEemcPhiBins + neighborPhi] << endm;
	    clusterEnergy += mEemcTowerHits[neighborEta * nEemcPhiBins + neighborPhi];
	    
	  }
	  
	}
	
	// Project cluster energy into transverse plane
	if (mMomentum && mEemcGeom) {
	  *mMomentum = mEemcGeom->getTowerCenter(p / nSub, p % nSub, e);
	  mMomentum->SetZ(mMomentum->Z() - zVertex);
	  mMomentum->SetX(mMomentum->X() * clusterEnergy / mMomentum->Mag() );
	  mMomentum->SetY(mMomentum->Y() * clusterEnergy / mMomentum->Mag() );
	  mMomentum->SetZ(mMomentum->Z() * clusterEnergy / mMomentum->Mag() );
	  clusterEt = mMomentum->Perp();
	  
	  LOG_DEBUG << "\tCluster Energy = " << clusterEnergy << " GeV" << endm;
	  LOG_DEBUG << "\tCluster eT  (corrected for vertex) = " << clusterEt << " GeV" << endm;
	  LOG_DEBUG << "\tCluster eta (corrected for vertex) = " << mMomentum->Eta() << endm;
	  LOG_DEBUG << "\tCluster phi = " << mMomentum->Phi() << endm;
	  LOG_DEBUG << "" << endm;
	}
	
	// Apply filter
	if(clusterEt > mClusterEtThreshold)
	{
	  return kStOK;
	}
	
	
      } // Seed tower check
      
    } // Seed tower phi loop
    
  } // Seed tower eta loop
  
  return kStSKIP;
  
}

////////////////////////////////////////////////////////////
//                 Display filter stats                   //
////////////////////////////////////////////////////////////
Int_t StEemcGammaFilterMaker::Finish()
{
  LOG_DEBUG << "::Finish()" << endm;
  
  LOG_INFO << "Finish() : " << GetName() << " finishing with " << endm;
  LOG_INFO << "Finish() : \t" << mAccepted << " of " << mTotal << " events passing the filter" << endm;
  LOG_INFO << "Finish() : \t" << mVertexRejected << " rejected for bad vertex" << endm;
  LOG_INFO << "Finish() : \t" << mFilterRejected << " rejected for no clusters" << endm;
  
  return kStOK;
}

// $Log: StEemcGammaFilterMaker.cxx,v $
// Revision 1.4  2016/05/25 21:35:26  jwebb
// Removed deadcode (coverity) and removed uneccessary printout.
//
// Revision 1.3  2010/08/09 21:51:22  seluzhen
// updated comment field
//
