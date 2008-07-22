//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
// and
// Hal Spinka <hms@anl.gov>
// Argonne National Laboratory
// Feb-Mar 2008
//

// ROOT
#include "TFile.h"
#include "TTree.h"
#include "TRandom.h"
#include "TClonesArray.h"
#include "TH1.h"
#include "TH2.h"

// STAR
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMcEvent/StMcEventTypes.hh"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcSimulatorMaker/StEEmcFastMaker.h"

// Local
#include "StEEmcShowerShape.h"
#include "StEEmcDataDrivenMcMaker.h"

ClassImp(StEEmcDataDrivenMcMaker);

void StEEmcDataDrivenMcMaker::Clear(Option_t* option)
{
  memset(mStrips, 0, sizeof(mStrips));
  StMaker::Clear(option);
}

int StEEmcDataDrivenMcMaker::Init()
{
  // Get pointer to EEMC database
  mEEmcDb = dynamic_cast<StEEmcDbMaker*>(GetMakerInheritsFrom("StEEmcDbMaker"));
  assert(mEEmcDb);

  // Initialize pointer to Monte Carlo event
  mcEvent = 0;

  // Load shower shape library
  TFile* file = new TFile(mLibraryFile);
  LOG_INFO << "Shower Shape Library = " << mLibraryFile << endm;
  assert(file && !file->IsZombie());
  TTree* tree = (TTree*)file->Get("etas");
  assert(tree);
  StEEmcShowerShape* showerShape = 0;
  tree->SetBranchAddress("StEEmcSmdResponse", &showerShape);

  for (int i = 0; i < 2; ++i) {
    for (int j = 0; j < 4; ++j) {
      mShowerShapes[i][j] = new TClonesArray("StEEmcShowerShape");
    }
  }

  for (int iEntry = 0; tree->GetEntry(iEntry) > 0; ++iEntry) {
    int uid = showerShape->highUstripId();
    int vid = showerShape->highVstripId();
    // Drop shapes that are at the edges of the SMD
    if (uid < 12 || uid > 275 || vid < 12 || vid > 275) continue;
    assert(0 <= showerShape->sector() && showerShape->sector() < 12);
    int i = getEnergyBin(showerShape);
    int j = getPreshowerBin(showerShape);
    assert(i >= 0 && i < 2);
    assert(j >= 0 && j < 4);
    TClonesArray& tca = *mShowerShapes[i][j];
    new (tca[tca.GetEntriesFast()]) StEEmcShowerShape(*showerShape);
  }

  LOG_INFO << "Number of shower shapes in library = " << tree->GetEntriesFast() << endm;

  for (int i = 0; i < 2; ++i) {
    for (int j = 0; j < 4; ++j) {
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

  // Create QA histograms
  hLibEntry = new TH1F("hLibEntry", "Shower shape library index", 100, 0, 3400); // WWJ library has 3334 entries
  hNumberOfPhotons = new TH1F("hNumberOfPhotons", "Number of photons/event", 20, 0, 20);
  hMcPhotonEnergyEta = new TH2F("hMcPhotonEnergyEta", "MC photon eta vs. energy;E [GeV];#eta_{detector}", 100, 0, 40, 100, 1, 2);
  hMcPhotonEnergyPt = new TH2F("hMcPhotonEnergyPt", "MC photon p_{T} vs. energy;E [GeV];p_{T} [GeV/c]", 100, 0, 50, 100, 0, 15);
  hMcPhotonParent = new TH1F("hMcPhotonParent", "MC photon parent particle", 5, 0, 5);
  hMcPhotonXY = new TH2F("hPhotonXY", "MC photon position in SMD plane;x [cm];y [cm]", 100, -250, 250, 100, -250, 250);
  hResidualSmdu = new TH1F("hResidualSmdu", "u-strip energy residual=before-after;#deltaE [GeV]", 100, -0.1, 0.1);
  hResidualSmdv = new TH1F("hResidualSmdv", "v-strip energy residual=before-after;#deltaE [GeV]", 100, -0.1, 0.1);
  hCorrSmdu = new TH2F("hCorrSmdu", "u-strip energy;E_{before} [GeV];E_{after} [GeV]", 100, 0, 0.1, 100, 0, 0.1);
  hCorrSmdv = new TH2F("hCorrSmdv", "v-strip energy;E_{before} [GeV];E_{after} [GeV]", 100, 0, 0.1, 100, 0, 0.1);
  hDiffStripSmdu = new TH2F("hDiffStripSmdu", "Residual of u-strip;u-strip;#deltaE=before-after", 25, -12, 13, 100, -0.1, 0.1);
  hDiffStripSmdv = new TH2F("hDiffStripSmdv", "Residual of v-strip;v-strip;#deltaE=before-after", 25, -12, 13, 100, -0.1, 0.1);
  hAsymStripSmdu = new TH2F("hAsymStripSmdu", "Asymmetry of u-strip;u-strip;#deltaE=before-after", 25, -12, 13, 100, -1, 1);
  hAsymStripSmdv = new TH2F("hAsymStripSmdv", "Asymmetry of v-strip;v-strip;#deltaE=before-after", 25, -12, 13, 100, -1, 1);
  hHighStripsUVid = new TH2F("hHighStripsUVid", ";u-strip id;v-strip id", 288, 0, 288, 288, 0, 288);
  hHighStripsUVenergy = new TH2F("hHighStripsUVenergy", ";E_{u} [GeV];E_{v} [GeV]", 100, 0, 0.2, 100, 0, 0.2);

  if (mLogFile) gROOT->cd();

  return StMaker::Init();
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
      mStrips[sector-1][plane-1][strip-1] = hit;
    }
  }

  // Get StMcEvent
  mcEvent = (StMcEvent*)GetDataSet("StMcEvent");

  if (!mcEvent) {
    LOG_WARN << "No StMcEvent" << endm;
    return kStWarn;
  }

  // Start with primary particles and look for final states
  // photons recursively.
  mNumberOfPhotons = 0;
  processVertex(mcEvent->primaryVertex());
  hNumberOfPhotons->Fill(mNumberOfPhotons);

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
    if (mcTrack->esmduHits().empty() || mcTrack->esmdvHits().empty()) return;

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

    // Get U and V strips closest to the position of the photon in
    // the SMD plane.
    float dca;
    int sectors[2];
    int strips[2];

    for (int plane = 0; plane < 2; ++plane) {
      const StructEEmcStrip* strip = EEmcSmdGeom::instance()->getDca2Strip(plane, position, &dca);
      sectors[plane] = strip->stripStructId.sectorId - 1; // sector=0-11
      strips [plane] = strip->stripStructId. stripId - 1; // strip=0-287

      // Find maximum strip within +/-20 strips of the intersection
      int xmin = max(0  , strips[plane] - 20);
      int xmax = min(287, strips[plane] + 20);

      StMuEmcHit* maxHit = mStrips[sectors[plane]][plane][strips[plane]];

      for (int strip = xmin; strip <= xmax; ++strip) {
	StMuEmcHit* hit = mStrips[sectors[plane]][plane][strip];
	if (hit) {
	  if (!maxHit || hit->getEnergy() > maxHit->getEnergy()) {
	    maxHit = hit;
	    strips[plane] = strip;
	  }
	}
      }
    }

    // Pick a random shower shape from the library. The shower shape and the Monte Carlo
    // photon must be in the same sector configuration, same energy bin, and same preshower bin.
    int iEnergyBin = !(mcTrack->energy() < 8);

    // Check range
    assert(iEnergyBin >= 0 && iEnergyBin < 2);

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
    assert(iPreshowerBin >= 0 && iPreshowerBin < 4);

    int iEntry = gRandom->Integer(mShowerShapes[iEnergyBin][iPreshowerBin]->GetEntriesFast());
    StEEmcShowerShape* showerShape = (StEEmcShowerShape*)mShowerShapes[iEnergyBin][iPreshowerBin]->At(iEntry);
    assert(showerShape);
    hLibEntry->Fill(iEntry);

    // Match the Monte Carlo energy of the photon to the energy
    // of the shower shape from the library computed by the
    // cluster finder.
    assert(showerShape->energy());
    float scale = mcTrack->energy() / showerShape->energy();

    // Replace simulation strips with strips from the shower shape
    // in the library out to +/-12 strips of the max strip.
    for (int plane = 0; plane < 2; ++plane) {
      int& sector = sectors[plane];
      int& strip = strips[plane];

      for (int dx = -12; dx <= 12; ++dx) {
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
	  hit->setAdc(0);
	  hit->setEnergy(0);

	  // Insert the newly created hit into
	  // the local strip table.
	  mStrips[sector][plane][id] = hit;
	}

	// Save value of energy before making hit substitution
	float energyBefore = hit->getEnergy();

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

	int adc = int(dE * StEEmcFastMaker::getSmdGain());

	hit->setAdc(hit->getAdc() - adc);
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
	adc = int(hit->getEnergy() * StEEmcFastMaker::getSmdGain());
	if (adc < 0) adc = 0;
	if (adc > StEEmcFastMaker::getMaxAdc()) adc = StEEmcFastMaker::getMaxAdc();
	hit->setAdc(adc);

	// The residual is defined as the energy of the SMD hit before
	// replacement minus the energy of the SMD hit after replacement.
	float energyAfter = hit->getEnergy();
	float residual = energyBefore - energyAfter;
	float sum = energyBefore + energyAfter;
	float asym = 0;

	if (sum) asym = residual / sum;

	switch (plane) {
	case 0:
	  hResidualSmdu->Fill(residual);
	  hCorrSmdu->Fill(energyBefore, energyAfter);
	  hDiffStripSmdu->Fill(dx, residual);
	  hAsymStripSmdu->Fill(dx, asym);
	  break;
	case 1:
	  hResidualSmdv->Fill(residual);
	  hCorrSmdv->Fill(energyBefore, energyAfter);
	  hDiffStripSmdv->Fill(dx, residual);
	  hAsymStripSmdv->Fill(dx, asym);
	  break;
	default:
	  break;
	}

	//hHighStripsUVid->Fill(strips[0], strips[1]);
	//hHighStripsUVenergy->Fill(mStrips[sectors[0]][0][strips[0]]->getEnergy(), mStrips[sectors[1]][1][strips[1]]->getEnergy());
      }	// Loop over strips
    } // Loop over planes

    // Increment photon counter
    ++mNumberOfPhotons;

    hMcPhotonEnergyEta->Fill(mcTrack->energy(), position.Eta());
    hMcPhotonEnergyPt->Fill(mcTrack->energy(), mcTrack->pt());
    StParticleDefinition* def = mcTrack->parent()->particleDefinition();
    TString name = def ? def->name().c_str() : Form("pdgId=%d", mcTrack->parent()->pdgId());
    hMcPhotonParent->Fill(name, 1);
    hMcPhotonXY->Fill(position.x(), position.y());
  }
}

int StEEmcDataDrivenMcMaker::Finish()
{
  hMcPhotonParent->LabelsDeflate();

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
