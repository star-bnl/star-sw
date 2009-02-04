//
// Hal Spinka <hms@anl.gov>
// Argonne National Laboratory
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//

// ROOT
#include "TFile.h"
#include "TTree.h"
#include "TRandom.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "TH2.h"
#include "TChain.h"

// STAR
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMcEvent/StMcEventTypes.hh"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcSimulatorMaker/StEEmcFastMaker.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

// Local
#include "StEEmcPool/StEEmcDataDrivenMcEventInfo/StEEmcDataDrivenMcEventInfo.h"
#include "StEEmcShowerShape.h"
#include "StEEmcDataDrivenMcMaker.h"

ClassImp(StEEmcDataDrivenMcMaker);

StEEmcDataDrivenMcMaker::StEEmcDataDrivenMcMaker(const char* name) : StMaker(name)
{
  mUsePed = true;
  mEEmcDb = 0;
}

void StEEmcDataDrivenMcMaker::Clear(Option_t* option)
{
  memset(mStrips, 0, sizeof(mStrips));
  mDataDrivenMcEventInfo->Clear(option);
  StMaker::Clear(option);
}

int StEEmcDataDrivenMcMaker::Init()
{
  // Initialize pointer to Monte Carlo event
  mMcEvent = 0;

  //mNumberOfStripsReplaced = 12; // default number of strips replaced
  //mShowerShapeScalingMethod = 1; // default scaling method
  // Load shower shape library
  TFile* file = new TFile(mLibraryFile);
  LOG_INFO << "Shower Shape Library = " << mLibraryFile << endm;
  assert(file && !file->IsZombie());
  TTree* tree = (TTree*)file->Get("etas");
  assert(tree);
  StEEmcShowerShape* showerShape = 0;
  tree->SetBranchAddress("StEEmcSmdResponse", &showerShape);

  for (int i = 0; i < NUMBER_OF_ENERGY_BINS; ++i) {
    for (int j = 0; j < NUMBER_OF_PRESHOWER_BINS; ++j) {
      mShowerShapes[i][j] = new TClonesArray("StEEmcShowerShape");
    }
  }

  for (int iEntry = 0; tree->GetEntry(iEntry) > 0; ++iEntry) {
    int uid = showerShape->highUstripId();
    int vid = showerShape->highVstripId();
    // Drop shapes that are at the edges of the SMD
//    if (uid < 12 || uid > 275 || vid < 12 || vid > 275) continue;
    if (uid < mNumberOfStripsReplaced || uid > 288 - mNumberOfStripsReplaced-1 || vid < mNumberOfStripsReplaced || vid > 288-mNumberOfStripsReplaced-1) continue; // Added  by Ilya Selyuzhenkov
    assert(0 <= showerShape->sector() && showerShape->sector() < 12);
    int i = getEnergyBin(showerShape);
    int j = getPreshowerBin(showerShape);
    assert(i >= 0 && i < NUMBER_OF_ENERGY_BINS);
    assert(j >= 0 && j < NUMBER_OF_PRESHOWER_BINS);
    TClonesArray& tca = *mShowerShapes[i][j];
    mLibraryMap[new (tca[tca.GetEntriesFast()]) StEEmcShowerShape(*showerShape)] = iEntry;
  }

  LOG_INFO << "Number of shower shapes in library = " << tree->GetEntriesFast() << endm;

  for (int i = 0; i < NUMBER_OF_ENERGY_BINS; ++i) {
    for (int j = 0; j < NUMBER_OF_PRESHOWER_BINS; ++j) {
      LOG_INFO << "Energy bin ="   << i
	       << ", preshower bin=" << j
	       << ", number of shower shapes=" << mShowerShapes[i][j]->GetEntriesFast()
	       << endm;
    } // for-loop
  } // for-loop

  file->Close();

  // MuDst utility to translate between StMuEmcHit id and sector, subsector, and strip id
  mMuEmcUtil = new StMuEmcUtil;

  // Initialize log file to 0
  mLogFile = 0;

  if (mLogFileName != "") {
    mLogFile = TFile::Open(mLogFileName, "recreate");
    assert(mLogFile);
  }

  // Data-driven MC event info
  mDataDrivenMcEventInfo = new StEEmcDataDrivenMcEventInfo;

  // Create tree
  mTree = new TTree("DataDrivenMcEventInfo", "Data-driven Monte Carlo event info");
  mTree->Branch("DataDrivenMcEventInfo", &mDataDrivenMcEventInfo);

  // Get Energy to ADC maker
  mA2E = (StEEmcA2EMaker*)GetMakerInheritsFrom("StEEmcA2EMaker");
  assert(mA2E);

  // Load pedestals from database
  mEEmcDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(mEEmcDb);

  // Clear ped & gain arrays
  memset(mPed, 0, sizeof(mPed));
  memset(mGain, 0, sizeof(mGain));

  return StMaker::Init();
}

int StEEmcDataDrivenMcMaker::InitRun(int runNumber)
{
  for (int sector = 0; sector < 12; ++sector) {
    for (int plane = 0; plane < 2; ++plane) {
      char uv = plane+'U';
      for (int strip = 0; strip < 288; ++strip) {
	//Database ranges: sector=1-12, plane=U-V, strip=1-288
	const EEmcDbItem* x = mEEmcDb->getByStrip(sector+1, uv, strip+1);
	assert(x);
	mPed[sector][plane][strip] = mUsePed ? x->ped : 0;
	mGain[sector][plane][strip] = mUsePed ? x->gain : StEEmcFastMaker::getSmdGain();
      }
    }
  }
  
  return StMaker::InitRun(runNumber);
}

int StEEmcDataDrivenMcMaker::Make()
{
  // Get MuDst
  if (!GetDataSet("MuDst")) {
    LOG_WARN << "No MuDst" << endm;
    return kStWarn;
  }

  // Get MuDst EMC collection
  StMuEmcCollection* emc = StMuDst::muEmcCollection();
  if (!emc) {
    LOG_WARN << "No MuDst EMC collection" << endm;
    return kStWarn;
  }

  // Load SMDU hits from MuDst
  for (int plane = 1; plane <= 2; ++plane) {
    char uv = 'U' + plane - 1;
    for (int i = 0; i < emc->getNEndcapSmdHits(uv); ++i) {
      int sector, strip;
      StMuEmcHit* hit = emc->getEndcapSmdHit(uv, i, sector, strip);
      hit->setAdc(int(hit->getAdc() + mPed[sector-1][plane-1][strip-1]));
      mStrips[sector-1][plane-1][strip-1] = hit;
    }
  }

  // Get StMcEvent
  mMcEvent = (StMcEvent*)GetDataSet("StMcEvent");

  if (!mMcEvent) {
    LOG_WARN << "No StMcEvent" << endm;
    return kStWarn;
  }

  // Fill MC event info
  mDataDrivenMcEventInfo->SetRunId(mMcEvent->runNumber());
  mDataDrivenMcEventInfo->SetEventId(mMcEvent->eventNumber());

  // Get MuDst file name
  if (StMuDstMaker* mudst = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"))) {
    mDataDrivenMcEventInfo->SetFileName(mudst->chain()->GetFile()->GetName());
  }

  // Start with primary particles and look for final states
  // photons recursively.
  processVertex(mMcEvent->primaryVertex());

  // Fill tree
  if (mDataDrivenMcEventInfo->NumberOfReplacements()) mTree->Fill();

  return kStOk;
}

void StEEmcDataDrivenMcMaker::processVertex(StMcVertex* mcVertex)
{
  for (unsigned int i = 0; i < mcVertex->numberOfDaughters(); ++i)
    processTrack(mcVertex->daughter(i));
}

void StEEmcDataDrivenMcMaker::processTrack(StMcTrack* mcTrack)
{
  if (mcTrack->stopVertex()) {
    // The particle decays; check its daughters.
    processVertex(mcTrack->stopVertex());
  }
  else {
    // This is a final state particle, i.e. it doesn't decay.
    // Is it a photon, electron or positron?
    if (mcTrack->geantId() < 1 || mcTrack->geantId() > 3) return;

    // We must have hits in both SMD planes
    if (mcTrack->esmduHits().empty() && mcTrack->esmdvHits().empty()) return;

    // SMD hits must be contained in a single sector
    if (multiSector(mcTrack->esmduHits()) || multiSector(mcTrack->esmdvHits())) return;

    // Get photon momentum and polar/zenith angle
    TVector3 momentum(mcTrack->momentum().xyz());
    float cosTheta = momentum.CosTheta();
    if (!cosTheta) return;

    // Get z-position of SMD plane
    float zSMD = EEmcGeomSimple::Instance().getZSMD();

    // Get z-position of collision vertex
    TVector3 vertex(mcTrack->startVertex()->position().xyz());

    // Project photon into the SMD plane.
    // First, we take the momentum vector and stretch it out
    // to the SMD plane. Second, we add the collision vertex
    // to get the position of the photon in the SMD plane
    // starting at the origin of the STAR coordinate system.
    TVector3 position = momentum;
    float magnitude = (zSMD - vertex.z()) / cosTheta;
    position.SetMag(magnitude);
    position += vertex;

    // Photon must extrapolate to a valid tower
    int sector    = -1;
    int subsector = -1;
    int etabin    = -1;
    if (!EEmcGeomSimple::Instance().getTower(position, sector, subsector, etabin)) return;

    // Save replace information
    StEEmcDataDrivenMcReplaceInfo* replaceInfo = mDataDrivenMcEventInfo->newReplaceInfo();

    int sectors[2];
    int strips[2];
    StMcCalorimeterHit* maxHits[2];

    for (int plane = 0; plane < 2; ++plane) {
      // Get U and V strips closest to the extrapolated position of the photon in
      // the SMD plane.
      float dca;
      const StructEEmcStrip* eemcStrip = EEmcSmdGeom::instance()->getDca2Strip(plane, position, &dca);
      sectors[plane] = eemcStrip->stripStructId.sectorId - 1; // sector=0-11
      strips [plane] = eemcStrip->stripStructId. stripId - 1; // strip=0-287

      // Get MC shower max hits
      StPtrVecMcCalorimeterHit& hits = (plane == 0) ? mcTrack->esmduHits() : mcTrack->esmdvHits();

      // Loop over hits and determine max hit
      StMcCalorimeterHit* maxHit = 0;

      for (size_t k = 0; k < hits.size(); ++k) {
	StMcCalorimeterHit* hit = hits[k];
	if (plane == 0)
	  replaceInfo->addMcHitEsmdU(hit);
	else
	  replaceInfo->addMcHitEsmdV(hit);
	if (!maxHit || hit->dE() > maxHit->dE()) maxHit = hit;
      }	// End hit loop

      // Save max hits for later
      maxHits[plane] = maxHit;	// Can be 0

      if (maxHit) {
	int maxId = maxHit->eta() - 1; // 0-287
	if (plane == 0) {
	  replaceInfo->highStripShiftU = maxId - strips[plane];
	}
	else {
	  replaceInfo->highStripShiftV = maxId - strips[plane];
	}
      }
    } // End plane loop

    getEnergies(mcTrack, replaceInfo);

    // Pick a random shower shape from the library. The shower shape and the Monte Carlo
    // photon must be in the same sector configuration, same energy bin, and same preshower bin.
    int iEnergyBin = !(mcTrack->energy() < 8);

    // Check range
    assert(iEnergyBin >= 0 && iEnergyBin < NUMBER_OF_ENERGY_BINS);

    // Get preshower hit from MuDst
    float pre1 = 0;
    float pre2 = 0;
    float post = 0;

    // Preshower hits loop
    for (int i = 0; i < StMuDst::muEmcCollection()->getNEndcapPrsHits(); ++i) {
      int sec;			// 1-12
      int sub;			// 1-5
      int eta;			// 1-12
      int layer;		// 1-3 (1=pre1, 2=pre2, 3=post)
      StMuEmcHit* hit = StMuDst::muEmcCollection()->getEndcapPrsHit(i, sec, sub, eta, layer);
      if (sector+1 == sec && subsector+1 == sub && etabin+1 == eta) {
	switch (layer) {
	case 1: pre1 += hit->getEnergy(); break;
	case 2: pre2 += hit->getEnergy(); break;
	case 3: post += hit->getEnergy(); break;
	default: break;
	}
      }
    } // Preshower hits loop

    int iPreshowerBin = -1;

    if (pre1 == 0 && pre2 == 0)
      iPreshowerBin = 0;
    else if (pre1 == 0 && pre2 > 0)
      iPreshowerBin = 1;
    else if (pre1 > 0 && pre1 < 4e-3)
      iPreshowerBin = 2;
    else if (pre1 >= 4e-3)
      iPreshowerBin = 3;
    else {
      LOG_ERROR << "Could not determine preshower bin" << endm;
      exit(1);
    }

    // Check range
    assert(iPreshowerBin >= 0 && iPreshowerBin < NUMBER_OF_PRESHOWER_BINS);

    int iEntry = gRandom->Integer(mShowerShapes[iEnergyBin][iPreshowerBin]->GetEntriesFast());
    StEEmcShowerShape* showerShape = (StEEmcShowerShape*)mShowerShapes[iEnergyBin][iPreshowerBin]->At(iEntry);
    assert(showerShape);

    // Save replacement info
    replaceInfo->pid = mcTrack->geantId();
    replaceInfo->parentPid = mcTrack->parent()->geantId();
    replaceInfo->libraryShapeId = mLibraryMap[showerShape];
    replaceInfo->libraryBinId = iEnergyBin * NUMBER_OF_PRESHOWER_BINS + iPreshowerBin;
    replaceInfo->energy = mcTrack->energy();
    replaceInfo->momentum = mcTrack->momentum().xyz();

    // Determine the first hadron PID from the Pythia record
    // (PDG code of grand parent for now)
    if (mcTrack->parent()->parent())
      replaceInfo->firstHadronPid = mcTrack->parent()->parent()->pdgId();

    // Match the Monte Carlo energy of the photon to the energy
    // of the shower shape from the library computed by the
    // cluster finder.
    assert(showerShape->energy());
//    float scale = mcTrack->energy() / showerShape->energy();

    // Replace simulation strips with strips from the shower shape
    // in the library out to +mNumberOfStripsReplaced/-mNumberOfStripsReplaced
    // strips of the max strip.
    for (int plane = 0; plane < 2; ++plane) {
      int& sector = sectors[plane];
      int& strip  = strips [plane];

      float scale = GetShowerShapeScale(mcTrack, showerShape, sector, plane, strip);
      if (plane == 0){ replaceInfo->energyScaleU = scale; }else{ replaceInfo->energyScaleV = scale; }

      for (int dx = -mNumberOfStripsReplaced; dx <= mNumberOfStripsReplaced; ++dx) {
	int id = strip + dx;
	if (id < 0 || id >= 288) continue;
	int highStrip = (plane == 0) ? showerShape->highUstripId() : showerShape->highVstripId();
	int id2 = highStrip + dx;
	assert(0 <= id2 && id2 < 288);
	StMuEmcHit* hit2 = (plane == 0) ? showerShape->uStrip(id2) : showerShape->vStrip(id2);
	StMuEmcHit* hit = mStrips[sector][plane][id]; // Can be 0 for simulation!
	if (!hit) {
	  // There was no Monte Carlo hit for that strip.
	  // Create an empty hit in the MuDst and set
	  // its id using SMuEmcUtil.
	  int det = (plane == 0) ? esmdu : esmdv;
	  StMuDst::muEmcCollection()->addSmdHit(det);
	  int n = StMuDst::muEmcCollection()->getNSmdHits(det);
	  hit = StMuDst::muEmcCollection()->getSmdHit(n - 1, det);

	  // int StMuEmcUtil::getEndcapId(int detector, int module, int eta, int sub, int& id);
	  // Parameters:
	  //   detector=(bemc=1, bprs=2, bsmde=3, bsmdp=4, eemc=5, eprs=6, esmdu=7, esmdv=8)
	  //   module=sector (1-12)
	  //   eta=strip (1-288)
	  //   sub=not used (always set to 1)
	  //   id=hit id
	  // Return value:
	  //   0=okay
	  //   1=error
	  // Description:
	  //   The id is encoded as 300*(module-1)+eta for ESMDU and ESMDV.
	  int id3;
	  assert(mMuEmcUtil->getEndcapId(det, sector+1, id+1, 1, id3) == 0);
	  hit->setId(id3);
	  hit->setAdc((int)mPed[sector][plane][id]);
	  hit->setEnergy(0);

	  // Insert the newly created hit into
	  // the local strip table.
	  mStrips[sector][plane][id] = hit;
	}

	// Find the corresponing Monte Carlo hit (identified by sector & strip id)
	// and subtract its energy contribution to the SMD strip in the MuDst.
	// If it is the only hit that deposited energy into that SMD strip, then
	// the MC energy and MuDst energy are identical.
	float dE = 0;
	StPtrVecMcCalorimeterHit& mcHits = (plane == 0) ? mcTrack->esmduHits() : mcTrack->esmdvHits();
	for (size_t i = 0; i < mcHits.size(); ++i) {
	  StMcCalorimeterHit* mcHit = mcHits[i];
	  if (mcHit->module() == sector+1 && mcHit->eta() == id+1) {
	    dE = mcHit->dE();
	    break;
	  }
	}

	hit->setEnergy(hit->getEnergy() - dE);

	// Replace the energy contribution of the Monte Carlo hit with the
	// corresponding hit from the shower shape library, appropriately
	// scaled and translated, by adding the energy of the hit from the
	// library to the strip in the MuDst.
	hit->setEnergy(hit->getEnergy() + scale * hit2->getEnergy());

	// Set the ADC. Below is probably the correct way to do it,
	// however the ADC were not saved for hits in the shower shape
	// library!
	//hit->setAdc(int(hit->getAdc() + scale * hit2->getAdc()));
	
	int adc = int(hit->getEnergy() * mGain[sector][plane][id] + mPed[sector][plane][id]);
//	cout <<"sec " << sector<< " plane " << plane << " strip " << id << " gain " << mGain[sector][plane][strip] << " ped " << mPed[sector][plane][strip]<< endl;
	if (adc < 0) adc = 0;
	if (adc > StEEmcFastMaker::getMaxAdc()) adc = StEEmcFastMaker::getMaxAdc();
	hit->setAdc(adc);
      }	// Loop over strips
    } // Loop over planes
  }
}

int StEEmcDataDrivenMcMaker::Finish()
{
  if (mLogFile) {
    mLogFile->Write();
    mLogFile->Close();
  }

  return kStOk;
}

bool StEEmcDataDrivenMcMaker::multiSector(const vector<StMcCalorimeterHit*>& hits) const
{
  for (size_t i = 0; i < hits.size(); ++i)
    if (hits[i]->module() != hits[0]->module())
      return true;
  return false;
}

int StEEmcDataDrivenMcMaker::getEnergyBin(StEEmcShowerShape* showerShape) const
{
  return !(showerShape->energy() < 8);
}

int StEEmcDataDrivenMcMaker::getPreshowerBin(StEEmcShowerShape* showerShape) const
{
  float pre1 = showerShape->preshower1();
  float pre2 = showerShape->preshower2();

  if (pre1 == 0 && pre2 ==   0) return 0;
  if (pre1 == 0 && pre2 >    0) return 1;
  if (pre1 >  0 && pre1 < 4e-3) return 2;
  if (pre1 >=             4e-3) return 3;

  return -1;
}

void StEEmcDataDrivenMcMaker::getEnergies(StMcTrack *mcTrack, StEEmcDataDrivenMcReplaceInfo *replaceInfo)
{
  // Loop over towers
  const int NUMBER_OF_EEMC_LAYERS = 6;

  StPtrVecMcCalorimeterHit* hits[NUMBER_OF_EEMC_LAYERS];
  StMcEmcHitCollection* emc[NUMBER_OF_EEMC_LAYERS];

  hits[0] = &mcTrack->eemcHits();
  hits[1] = &mcTrack->eprsHits();
  hits[4] = &mcTrack->esmduHits();
  hits[5] = &mcTrack->esmdvHits();

  emc[0] = mMcEvent->eemcHitCollection();
  emc[1] = mMcEvent->eprsHitCollection();
  emc[4] = mMcEvent->esmduHitCollection();
  emc[5] = mMcEvent->esmdvHitCollection();

 for (int layer = 0; layer < NUMBER_OF_EEMC_LAYERS; ++layer) {
    if (layer == 2 || layer == 3) continue; // not implemented in StMcEvent
    replaceInfo->nTowerFired[layer] = hits[layer]->size();

    vector<int> ids;

    replaceInfo->dEnergy[layer] = 0;
    replaceInfo->totalEnergyScaled[layer] = 0;
 
    for (size_t i = 0; i < hits[layer]->size(); ++i) {
      StMcCalorimeterHit* hit = hits[layer]->at(i);
      replaceInfo->dEnergy[layer] += hit->dE();
      int id = hit->sub() + 100 * hit->module() + 100000 * hit->eta();
      ids.push_back(id);

      if (layer < 4) {
	StEEmcTower tower = mA2E->tower(hit->module()-1, hit->sub()-1, hit->eta()-1);
	replaceInfo->totalEnergyScaled[layer] += tower.energy();
      }
      else {
	StEEmcStrip strip = mA2E->strip(hit->module()-1, hit->sub()-1, hit->eta()-1);
	replaceInfo->totalEnergyScaled[layer] += strip.energy();
      }
    }

    replaceInfo->totalEnergy[layer] = 0;  

    for (size_t m = 1; m <= emc[layer]->numberOfModules(); ++m) {
      for (size_t k = 0; k < emc[layer]->module(m)->numberOfHits(); ++k) {
	StMcCalorimeterHit* hit = emc[layer]->module(m)->hits().at(k);
	int id = hit->sub() + 100 * hit->module() + 100000 * hit->eta();
	if (find(ids.begin(), ids.end(), id) != ids.end()) {
	  replaceInfo->totalEnergy[layer] += hit->dE();
	}
      }
    }
  }
}

float StEEmcDataDrivenMcMaker::GetShowerShapeScale
(
	StMcTrack *mcTrack,
	StEEmcShowerShape* showerShape,
	int sector,
	int plane,
	int geantPhotonCentralStrip
)
{
	float scale = 0;
	switch (mShowerShapeScalingMethod)
	{
	case 1: // method 1: scale = E_smd^geant / E_smd^library
	{
		float smdEnergyGeant  = 0; // SMD energy sum from Geant photon from +/- mNumberOfStripsReplaced strips
		float smdEnergyLibrary  = 0; // SMD energy sum from data-driven library photon in +/- mNumberOfStripsReplaced strips
		for (int dx = -mNumberOfStripsReplaced; dx <= mNumberOfStripsReplaced; ++dx)
		{
			int id = geantPhotonCentralStrip + dx;
			if (id < 0 || id >= 288) continue;
			int highStrip = (plane == 0) ? showerShape->highUstripId() : showerShape->highVstripId();
			int id2 = highStrip + dx;
			assert(0 <= id2 && id2 < 288);
			StMuEmcHit* hit2 = (plane == 0) ? showerShape->uStrip(id2) : showerShape->vStrip(id2);

			StPtrVecMcCalorimeterHit& mcHits = (plane == 0) ? mcTrack->esmduHits() : mcTrack->esmdvHits();
			for (size_t i = 0; i < mcHits.size(); ++i)
			{
				StMcCalorimeterHit* mcHit = mcHits[i];
				if (mcHit->module() == sector+1 && mcHit->eta() == id+1)
				{
					smdEnergyGeant += mcHit->dE();
					break;
				}
			}
			smdEnergyLibrary += hit2->getEnergy();
		}
		scale = smdEnergyGeant/smdEnergyLibrary;
		break;
	}
	case 2: // method 2: scale = E_gamma^geant / E_gamma^library
	{
		scale = mcTrack->energy() / showerShape->energy();
		break;
	}
	default: break;
	}
	return scale;
}

