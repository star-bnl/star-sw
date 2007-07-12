// TreeEntryClasses.cxx
// M. Horsley 
// basis borrowed from M.L. Miller

#include "TreeEntryClasses.h"
#include "StRichTrack.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StRichMaterialsDb.h"

#include "StRichPidTraits.h"
#include "StRichPid.h"

#include "StRichPhotonInfo.h"

#include "StParticleTypes.hh"

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TClonesArray.h"
//---------------------------------------------
ClassImp(HitEntry)
HitEntry::HitEntry() {};

HitEntry::HitEntry(Float_t ix, Float_t iy, Float_t id, Float_t isig, Float_t iang, 
		         Int_t iring, Int_t iadc, Int_t inpads, Int_t isize)
{
  x = ix;
  y = iy;
  d = id;
  sig = isig;
  ang = iang;
  ring = iring;
  adc = iadc;
  npads = inpads;
  nhits = isize;
}

HitEntry::~HitEntry() {}


//--------------------------------------------- 
ClassImp(TrackEntry)

TrackEntry::TrackEntry()
{
    hitArray = new TClonesArray("HitEntry",500);
    clear();
}

//Copy constructor
TrackEntry::TrackEntry(const TrackEntry& t) 
{
    hitArray = new TClonesArray("HitEntry",500);
    copyTrack(t);
}


TrackEntry::~TrackEntry()
{
    hitArray->Delete();
    delete hitArray;
}


void TrackEntry::clear()
{
    hitArray->Clear();
    
    vertexz = 0;
    eventrun = 0;
    eventn = 0;
    refit = 0;
    q = 0;
    nrichtracks = 0;
    
    curvature = 0;

    gpx = 0.;
    gpy = 0.;
    gpz = 0.;

    lpx = 0.;
    lpy = 0.;
    lpz = 0.;

    pmipx = 0;
    pmipy = 0;
    
    amipx = 0;
    amipy = 0;
    amipq = 0;
    amipn = 0;
    amipmax = 0;
    
    radx = 0;
    rady = 0;
    
    lastx = 0;
    lasty = 0;
    lastz = 0;

    firstx = 0;
    firsty = 0;
    firstz = 0;

    fitpoints = 0;
    indexin = 0;
    indexout = 0;
    
    picons1sig = 0;
    picons2sig = 0;
    piconsang = 0;
    piconsazi = 0;
    piconsarea = 0;
    pitotazi = 0;

    kacons1sig = 0;
    kacons2sig = 0;
    kaconsang = 0;
    kaconsazi = 0;
    kaconsarea = 0;
    katotazi = 0;

    prcons1sig = 0;
    prcons2sig = 0;
    prconsang = 0;
    prconsazi = 0;
    prconsarea = 0;
    prtotazi = 0;

    hitCounter = 0;
    return;
}

void TrackEntry::copyTrack(const TrackEntry& t)
{
  //m_p = t.getP();
  // m_HitCounter = t.hitCounter();
  copyHits(t);
    
    return;
}

void TrackEntry::copyHits(const TrackEntry& t)
{
    TClonesArray* pArray = const_cast<TClonesArray*>(t.getHitArray());
    TClonesArray& oldArray = *pArray;
    TClonesArray& rHitArray = *hitArray;
    for (int i=0; i<hitCounter; i++) {
	HitEntry* temp = static_cast<HitEntry*>(oldArray[i]);
	HitEntry& oldEntry = *temp;
	new(rHitArray[i]) HitEntry(oldEntry);
    }
    return;
}

void TrackEntry::addHit(HitEntry entry)
{
    TClonesArray& cArr = *hitArray;
    new(cArr[hitCounter]) HitEntry(entry);
    hitCounter++;
    return;
}

void TrackEntry::print() const {
  return;
}

Float_t TrackEntry::getP() const {return gpx;}
void TrackEntry::setP(Float_t pp) {gpx = pp;}

void TrackEntry::addTrackInfo(StRichTrack* richTrack, StEvent* event, StRichMaterialsDb* mMaterialDb, Int_t ntracks, Int_t nnt) {

  StPionMinus*  pion = StPionMinus::instance();
  StKaonMinus*  kaon = StKaonMinus::instance(); 
  StAntiProton* proton = StAntiProton::instance();



  // event-wise
  vertexz = -999;
  if (event->primaryVertex()) vertexz = event->primaryVertex()->position().z();
  eventrun = event->runId();
  eventn = event->id();

  refit = richTrack->getRefit();
  nrichtracks = ntracks;
  nnegtracks = nnt;

  // global, primary 3d dca's
  pdca = -999;
  gdca = -999;
  if (event->primaryVertex()) {
    pdca = richTrack->getStTrack()->geometry()->helix().distance(event->primaryVertex()->position());
    if (richTrack->getStTrack()->node()->track(global)) {
      gdca = richTrack->getStTrack()->node()->track(global)->geometry()->helix().distance(event->primaryVertex()->position());
    }
  }

 

  // charge
  q = richTrack->getStTrack()->geometry()->charge();

  // curvature
  curvature = richTrack->getStTrack()->geometry()->curvature();

  // global momentum
  gpx = richTrack->getStTrack()->geometry()->momentum().x();
  gpy = richTrack->getStTrack()->geometry()->momentum().y();
  gpz = richTrack->getStTrack()->geometry()->momentum().z();
  
  // local momentum
  lpx = richTrack->getMomentum().x();
  lpy = richTrack->getMomentum().y();
  lpz = richTrack->getMomentum().z();
  
  // residual at padplane
  pmipx = richTrack->getProjectedMIP().x();
  pmipy = richTrack->getProjectedMIP().y();
  
  amipx = -999;
  amipy = -999;
  amipq = -999;
  amipn = -999;
  amipmax = -999;
  if (richTrack->getAssociatedMIP()) {
    amipx = richTrack->getAssociatedMIP()->local().x();
    amipy = richTrack->getAssociatedMIP()->local().y();
    amipq = richTrack->getAssociatedMIP()->charge();
    amipn = richTrack->getAssociatedMIP()->numberOfPads();
    amipmax = richTrack->getAssociatedMIP()->maxAmplitude();
  }

  // impact point on radiator
  radx = richTrack->getImpactPoint().x();
  rady = richTrack->getImpactPoint().y();
  
  // last hit
  lastx = richTrack->getLastHit().x();
  lasty = richTrack->getLastHit().y();
  lastz = richTrack->getLastHit().z();

  if ( richTrack->getStTrack() &&  richTrack->getStTrack()->detectorInfo()) {
    firstx = richTrack->getStTrack()->detectorInfo()->firstPoint().x();
    firsty = richTrack->getStTrack()->detectorInfo()->firstPoint().y();
    firstz = richTrack->getStTrack()->detectorInfo()->firstPoint().z();
  } 


  // track points
  fitpoints = richTrack->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
  
  // index of refraction
  indexin = mMaterialDb->indexOfRefractionOfC6F14At(mMaterialDb->innerWavelength());
  indexout = mMaterialDb->indexOfRefractionOfC6F14At(mMaterialDb->outerWavelength());

  // get pid traits
  const StPtrVecTrackPidTraits& theRichPidTraits = richTrack->getStTrack()->pidTraits(kRichId);
  StTrackPidTraits* theMostRecentTrait = theRichPidTraits[theRichPidTraits.size()-1];
  StRichPidTraits *richPidTraits = dynamic_cast<StRichPidTraits*>(theMostRecentTrait);
  const StSPtrVecRichPid& theRichPids = richPidTraits->getAllPids();
  
  picons1sig = -999;
  picons2sig = -999;
  piconsang  = -999;
  piconsazi  = -999;
  piconsarea = -999;
  pitotazi = -999;

  kacons1sig = -999;
  kacons2sig = -999;
  kaconsang  = -999;
  kaconsazi  = -999;
  kaconsarea = -999;
  katotazi = -999;

  prcons1sig = -999;
  prcons2sig = -999;
  prconsang  = -999;
  prconsazi  = -999;
  prconsarea = -999;
  prtotazi = -999;


  for (int j=0;j<(int)theRichPids.size();j++) {
    
    int consAreaOneSigmaHits = 0; 
    int consAreaTwoSigmaHits = 0;
    
    const StSPtrVecRichPhotonInfo& infos = theRichPids[j]->getPhotonInfo();    
    for(size_t kk=0; kk<infos.size(); kk++) { // info loop

      float d       = infos[kk]->d();
      float sigma   = infos[kk]->sigma();
      float azimuth = infos[kk]->azimuth();
	    
      if( fabs(azimuth) > theRichPids[j]->getConstantAreaCut() && ((d>0) && (d<1)) ) {
	if( fabs(sigma)<= 1 ) consAreaOneSigmaHits++;
	if( fabs(sigma)<= 2 ) consAreaTwoSigmaHits++;
      } 
    } // info loop
    
    
    // pion
    if (theRichPids[j]->getParticleNumber()==pion->pdgEncoding()) {
      piconsang = theRichPids[j]->getConstantAreaCut()/degree; 
      piconsazi = theRichPids[j]->getTruncatedAzimuth()/degree;
      piconsarea = theRichPids[j]->getTruncatedArea();
      picons1sig = consAreaOneSigmaHits;
      picons2sig = consAreaTwoSigmaHits;
      pitotazi =  theRichPids[j]->getTotalAzimuth()/degree;
    }

    // kaon
    if (theRichPids[j]->getParticleNumber()==kaon->pdgEncoding()) {
      kaconsang = theRichPids[j]->getConstantAreaCut()/degree; 
      kaconsazi = theRichPids[j]->getTruncatedAzimuth()/degree;
      kaconsarea = theRichPids[j]->getTruncatedArea();
      kacons1sig = consAreaOneSigmaHits;
      kacons2sig = consAreaTwoSigmaHits;
      katotazi =  theRichPids[j]->getTotalAzimuth()/degree;
    }

    // proton
    if (theRichPids[j]->getParticleNumber()==proton->pdgEncoding()) {
      prconsang = theRichPids[j]->getConstantAreaCut()/degree; 
      prconsazi = theRichPids[j]->getTruncatedAzimuth()/degree;
      prconsarea = theRichPids[j]->getTruncatedArea();
      prcons1sig = consAreaOneSigmaHits;
      prcons2sig = consAreaTwoSigmaHits;
      prtotazi =  theRichPids[j]->getTotalAzimuth()/degree;  
    }
  }
}


int TrackEntry::getHitCounter() const {return hitCounter;}

const TClonesArray* TrackEntry::getHitArray() const {return hitArray;}

