#include "Stiostream.h"
#include <cmath>
#include <stdio.h>

#include "StEventTypes.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "Sti/Base/Factory.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiTrack.h"
#include "Sti/StiMcTrack.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiDetector.h"

StiSvtHitLoader::StiSvtHitLoader()
: StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader")
{}

StiSvtHitLoader::StiSvtHitLoader(StiHitContainer* hitContainer,
                                 StiHitContainer* mcHitContainer,
                                 Factory<StiHit>*hitFactory,
                                 StiDetectorBuilder*detector)
: StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("SvtHitLoader",hitContainer,mcHitContainer,hitFactory,detector)
{}

StiSvtHitLoader::~StiSvtHitLoader()
{}

void StiSvtHitLoader::loadHits(StEvent* source,
                               Filter<StiTrack> * trackFilter,
                               Filter<StiHit> * hitFilter)
{
  cout <<"StiSvtHitLoader::loadHits() - Started"<<endl;
  if (!source) throw runtime_error("StiSvtHitLoader::loadHits() -F- source==0 ");
  StSvtHitCollection* svthits = source->svtHitCollection();
  if (!svthits)
    {
    cout << "StiSvtHitLoader::loadHits(StEvent* source) -W- NO SVT hits"<<endl;
    return;
    }
  StSvtHit* hit;
  StiHit* stiHit;
  StiDetector* detector;
  if (!_hitContainer) throw runtime_error("StiSvtHitLoader::loadHits() -F- _hitContainer==0 ");
  for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel)
    {
    StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
    if (!barrelhits) break;
    for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder)
      {
      StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
      if (!ladderhits) break;
      for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer)
        {
        StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
        if (!waferhits) break;
        const StSPtrVecSvtHit& hits = waferhits->hits();
        for (const_StSvtHitIterator it=hits.begin(); it!=hits.end(); ++it)
          {
          if (!*it) throw runtime_error("StiSvtHitLoader::loadHits() -W- *it==0!");
          hit = static_cast<StSvtHit*>(*it);
          if (!hit) throw runtime_error("StiSvtHitLoader::loadHits() -W- hit==0!");
          int svtLayer = hit->layer();
          int svtLadder = hit->ladder();
          int layer = getLayer(svtLayer);
          int ladder = getLadder(svtLayer,svtLadder);
          detector = _detector->getDetector(layer,ladder);
          if (!detector) throw runtime_error("StiSvtHitLoader::loadHits() -W- detector==0!");
          if (hit && detector && hit->flag()<4)
            {
            stiHit = _hitFactory->getInstance();
            stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(),hit->position().z(),hit->charge() );
            _hitContainer->add( stiHit );
            }
          }
        }
      }
    }
  cout <<"StiSvtHitLoader::loadHits() -I- Done"<<endl;
}


void StiSvtHitLoader::loadMcHits(StMcEvent* source,
                                 bool useMcAsRec,
                                 Filter<StiTrack> * trackFilter,
                                 Filter<StiHit> * hitFilter,
                                 StMcTrack & stMcTrack,
                                 StiMcTrack & stiMcTrack)
{
  const StPtrVecMcSvtHit& hits = stMcTrack.svtHits();
  //cout<< "StiSvtHitLoader::loadMcHits() -I- size():"<< hits.size() << endl;
  for (vector<StMcSvtHit*>::const_iterator iterHit = hits.begin();
       iterHit != hits.end();
       iterHit++)
    {
    StMcSvtHit*hit=*iterHit;
    if (!hit)	throw runtime_error("StiSvtHitLoader::loadMcHits(...) -E- hit==0");
    int svtLayer = hit->layer();
    int svtLadder = hit->ladder();
    int layer = getLayer(svtLayer);
    int ladder = getLadder(svtLayer,svtLadder);
    StiDetector * detector = _detector->getDetector(layer,ladder);
    if (!detector) throw runtime_error("StiSvtHitLoader::loadMcHits(...) -E- Detector element not found");
    StiHit * stiHit = _hitFactory->getInstance();
    if(!stiHit) throw runtime_error("StiSvtHitLoader::loadMcHits(...) -E- stiHit==0");
    stiHit->reset();
    stiHit->setGlobal(detector,0,hit->position().x(),hit->position().y(),hit->position().z(),hit->dE());
    _mcHitContainer->add( stiHit );
    //cout << "StiSvtHitLoader::loadMcHits() -I- Adding " << *stiHit << endl;
    stiMcTrack.addHit(stiHit);
    if (useMcAsRec)  _hitContainer->add( stiHit );
    }
}


int StiSvtHitLoader::getLayer(int svtLayer) const
{
  int layer;
  switch (svtLayer)
    {
    case 1: layer = 0; break;
    case 2: layer = 1; break;
    case 3: layer = 2; break;
    case 4: layer = 3; break;
    case 5: layer = 4; break;
    case 6: layer = 5; break;
    default: throw runtime_error("StiSvtHitLoader:getLayer() -E- invalid argument");
    }
  return layer;
}

int StiSvtHitLoader::getLadder(int svtLayer,int svtLadder) const
{
  int layer;
  int ladder;
  switch (svtLayer)
    {
    case 1:
      switch (svtLadder)
        {
        case 2: layer = 0; ladder = 0;break;
        case 4: layer = 0; ladder = 1;break;
        case 6: layer = 0; ladder = 2;break;
        case 8: layer = 0; ladder = 3;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 1");
        }
      break;
    case 2:
      switch (svtLadder)
        {
        case 1: layer = 1; ladder = 0;break;
        case 3: layer = 1; ladder = 1;break;
        case 5: layer = 1; ladder = 2;break;
        case 7: layer = 1; ladder = 3;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 2");
        }
      break;
    case 3:
      switch (svtLadder)
        {
        case  2: layer = 2; ladder = 0;break;
        case  4: layer = 2; ladder = 1;break;
        case  6: layer = 2; ladder = 2;break;
        case  8: layer = 2; ladder = 3;break;
        case 10: layer = 2; ladder = 4;break;
        case 12: layer = 2; ladder = 5;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 3");
        }
      break;
    case 4:
      switch (svtLadder)
        {
        case  1: layer = 3; ladder = 0;break;
        case  3: layer = 3; ladder = 1;break;
        case  5: layer = 3; ladder = 2;break;
        case  7: layer = 3; ladder = 3;break;
        case  9: layer = 3; ladder = 4;break;
        case 11: layer = 3; ladder = 5;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 4");
        }
      break;
    case 5:
      switch (svtLadder)
        {
        case  2: layer = 4; ladder = 0;break;
        case  4: layer = 4; ladder = 1;break;
        case  6: layer = 4; ladder = 2;break;
        case  8: layer = 4; ladder = 3;break;
        case 10: layer = 4; ladder = 4;break;
        case 12: layer = 4; ladder = 5;break;
        case 14: layer = 4; ladder = 6;break;
        case 16: layer = 4; ladder = 7;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 5");
        }
      break;
    case 6:
      switch (svtLadder)
        {
        case  1: layer = 5; ladder = 0;break;
        case  3: layer = 5; ladder = 1;break;
        case  5: layer = 5; ladder = 2;break;
        case  7: layer = 5; ladder = 3;break;
        case  9: layer = 5; ladder = 4;break;
        case 11: layer = 5; ladder = 5;break;
        case 13: layer = 5; ladder = 6;break;
        case 15: layer = 5; ladder = 7;break;
        default: throw runtime_error("StiSvtHitLoader:loadHits() -E- 6");
        }
      break;
    }
  return ladder;
}
