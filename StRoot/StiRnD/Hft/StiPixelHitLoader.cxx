#include "Stiostream.h"
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiMcTrack.h"
#include "Sti/StiTrackContainer.h"
#include "StiPixelHitLoader.h"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"

StiPixelHitLoader::StiPixelHitLoader()
    : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("TpcHitLoader")
{}
    
StiPixelHitLoader::StiPixelHitLoader(StiHitContainer* hitContainer,
				     StiHitContainer* mcHitContainer,
				     Factory<StiHit>*hitFactory,
				     StiDetectorBuilder * detector)
    : StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("PixelHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiPixelHitLoader::~StiPixelHitLoader()
{}

void StiPixelHitLoader::loadHits(StEvent* source,
				 Filter<StiTrack> * trackFilter, 
				 Filter<StiHit> * hitFilter)
{
    cout << "StiPixelHitLoader::loadHits(StEvent*) -I- Started" << endl;
    if (!_detector)
	throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
    if(!_hitContainer)
	throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");

    /*
      StiDetector * detector;
      StiHit* stiHit;
      const StTpcHitCollection* tpcHits = source->tpcHitCollection();
      unsigned int stiSector;
      for (unsigned int sector=0; sector<24; sector++) 
      {
      if (sector<12)
      stiSector = sector;
      else
      stiSector = 11 - (sector-11)%12;
      const StTpcSectorHitCollection* secHits = tpcHits->sector(sector);
      if (!secHits) 
      {
      _messenger << "StiPixelHitLoader::loadHits(StEvent* source) -W- no hits for sector:"<<sector<<endl;
      break;
      }
      for (unsigned int row=0; row<45; row++) 
      {
      //_messenger << "StiPixelHitLoader:loadHits() -I- Loading row:"<<row<<" sector:"<<sector<<endl;
      const StTpcPadrowHitCollection* padrowHits = secHits->padrow(row);
      if (!padrowHits)
      {
      //_messenger << "StiPixelHitLoader:loadHits() - row=="<<row<<" has padrowHits==0"<<endl;
      break;
      }
      const StSPtrVecTpcHit& hitvec = padrowHits->hits();
      detector = _detector->getDetector(row,stiSector);
	  
      if (!detector)
      {
      cout << "StiPixelHitLoader::loadHits(StEvent*) -E- Detector not found for pad row:" << row
      << " sector:"<<sector<<endl;
      throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- Detector element not found");
      }
      for (vector<StTpcHit*>::const_iterator iter = hitvec.begin();
      iter != hitvec.end(); 
      iter++) 
      {
      StTpcHit*hit=*iter;
      if(!_hitFactory)
      throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- _hitFactory==0");
      stiHit = _hitFactory->getInstance();
      if(!stiHit)
      throw runtime_error("StiPixelHitLoader::loadHits(StEvent*) -E- stiHit==0");
      stiHit->reset();
      stiHit->setGlobal(detector, 
      hit, 
      hit->position().x(),
      hit->position().y(),
      hit->position().z(),
      hit->charge());
      //cout << "Adding HIT:"<<*stiHit<<endl;
      _hitContainer->add( stiHit );
      }
      }
      }
    */
    cout << "StiPixelHitLoader::loadHits(StEvent*) -I- Done" << endl;
}

void StiPixelHitLoader::loadMcHits(StMcEvent* source,
				   bool useMcAsRec,
				   Filter<StiTrack> * trackFilter, 
				   Filter<StiHit> * hitFilter)
{
    cout << "StiPixelHitLoader::loadMcHits(StEvent*) -I- Started" << endl;
    if (!_detector)
	throw runtime_error("StiPixelHitLoader::loadMcHits(StEvent*) -F- _detector==0");
    if(!_mcHitContainer)
	throw runtime_error("StiPixelHitLoader::loadMcHits(StEvent*) -F- _mcHitContainer==0");
  
    if(!_mcTrackFactory)
	throw runtime_error("StiPixelHitLoader::loadMcHits() -F- _mcTrackFactory==0");
    if (!_mcTrackContainer)
	throw runtime_error("StiPixelHitLoader::loadMcHitss() -F- _mcTrackContainer==0");
    if(!_hitFactory)
	throw runtime_error("StiPixelHitLoader::loadMcHits(StMcEvent*) -F- _hitFactory==0");
    cout << "StiPixelHitLoader::loadMcHits() -I- Loading"<<endl;
    /*
    // 
    StSPtrVecMcTrack & mcTracks = source->tracks();
    StiMcTrack * mcTrack;
    StMcTrack  * stMcTrack;
    StMcTrackConstIterator iter;
    int nTracks      = 0;
    int nPlusTracks  = 0;
    int nMinusTracks = 0;
    for (iter=mcTracks.begin();iter!=mcTracks.end();iter++)
    {
    //_messenger << "Loading StMcTrack into _mcTrackContainer" << endl;
    stMcTrack = *iter;
    double eta = stMcTrack->pseudoRapidity();
    double pt  = stMcTrack->pt();
    const StPtrVecMcTpcHit& hits = stMcTrack->tpcHits();
    int nPts = hits.size();
    mcTrack = _mcTrackFactory->getInstance();
    mcTrack->reset();
    mcTrack->setStMcTrack( (*iter) );
    double charge = mcTrack->getCharge();
    if (!trackFilter || trackFilter->filter(mcTrack) ) //fabs(eta)<1.5 && pt<20. && pt>0.3 && nPts>30 && charge>0)
    {      
    _mcTrackContainer->add(mcTrack);
    nTracks++;
    if (charge>0) 
    ++nPlusTracks;
    else 
    ++nMinusTracks;
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- nHits:" << hits.size()<<endl; 
    for (vector<StMcTpcHit*>::const_iterator iterHit = hits.begin();
    iterHit != hits.end(); 
    iterHit++) 
    {
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- Hit:"<<endl;
    StMcTpcHit*hit=*iterHit; 
    if (!hit)
    throw runtime_error("StiKalmanTrackFinder::loadMcHits(StMcEvent*) -E- hit==0");
    //cout << *hit<<endl;
    unsigned int row = hit->padrow()-1;
    unsigned int sector = hit->sector()-1;
    unsigned int stiSector;
    if (sector<12)
    stiSector = sector;
    else
    stiSector = 11 - (sector-11)%12;
    //cout << "sector = " << sector <<" stiSector = "<<stiSector<<endl;
    StiDetector * detector = _detector->getDetector(row,stiSector);
    if (!detector)
    {
    //cout << "StiKalmanTrackFinder::loadMcHits(StMcEvent*) -E- Detector element not found"<<endl;
    throw runtime_error("StiKalmanTrackFinder::loadMcHits(StMcEvent*) -E- Detector element not found");
    }
    StiHit * stiHit = _hitFactory->getInstance();
    if(!stiHit)
    {
    //cout << "StiKalmanTrackFinder::loadMcHits(StMcEvent*) -E- stiHit==0"<<endl;
    throw runtime_error("StiKalmanTrackFinder::loadMcHits(StMcEvent*) -E- stiHit==0");
    }
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- copy hit info to StiHit"<<endl;
    stiHit->reset();
    stiHit->setGlobal(detector, 
    0, 
    hit->position().x(),
    hit->position().y(),
    hit->position().z(),
    hit->dE());
    //_mcHitContainer->add( stiHit );
    if (useMcAsRec) 
    {
    _hitContainer->add( stiHit );
    }
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- Hit Done."<<endl;
    }
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- Hit Loop Done."<<endl;
    }
    //cout << "StiKalmanTrackFinder::loadMcHits() -I- Track done"<<endl;
    }

    cout << "StiPixelHitLoader::loadMcHits() -I- Event Loaded"<< endl
    << "============================================================================="<< endl
    << "      hitContainer size = " << _hitContainer->size()<<endl
    << "  mc  hitContainer size = " << _mcHitContainer->size()<<endl
    << " mc track Container size= " << _mcTrackContainer->size()<<endl
    << "               nTracks  = " << nTracks << endl
    << "           nPlusTracks  = " << nPlusTracks << endl
    << "          nMinusTracks  = " << nMinusTracks << endl;
    */

    cout << "StiPixelHitLoader::loadMcHits(StEvent*) -I- Done" << endl;
}

