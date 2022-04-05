#include "Stiostream.h"
#include <cmath>
#include <stdio.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "Sti/Base/Factory.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "Sti/StiHit.h"
#include "Sti/StiTrack.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDetector.h"
#include "Sti/StiHitTest.h"

StiSvtHitLoader::StiSvtHitLoader()
: StiHitLoader<StEvent,StiDetectorBuilder>("SvtHitLoader")
{}

StiSvtHitLoader::StiSvtHitLoader(StiHitContainer* hitContainer,
                                 Factory<StiHit>*hitFactory,
                                 StiDetectorBuilder*detector)
: StiHitLoader<StEvent,StiDetectorBuilder>("SvtHitLoader",hitContainer,hitFactory,detector)
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
  StSvtHit* hit=0;
  StiHit* stiHit=0;
  StiDetector* detector=0;
  if (!_hitContainer) throw runtime_error("StiSvtHitLoader::loadHits() -F- _hitContainer==0 ");
  int hitCounter = 0;
  for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel)
  {
    StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
    if (!barrelhits) continue;
    for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder)
    {
      StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
      if (!ladderhits) continue;
      for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer)
      {
        StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
        if (!waferhits) continue;
        const StSPtrVecSvtHit& hits = waferhits->hits();
        StiHitTest hitTest;
        for (const_StSvtHitIterator it=hits.begin(); it!=hits.end(); ++it)
        {
          if (!*it) throw runtime_error("StiSvtHitLoader::loadHits() -W- *it==0!");
          hit = static_cast<StSvtHit*>(*it);
          if (!hit) throw runtime_error("StiSvtHitLoader::loadHits() -W- hit==0!");
          int svtLayer = hit->layer();
          int svtLadder = hit->ladder();
          int stilayer = getLayer(svtLayer);
          int stiladder = getLadder(svtLayer,svtLadder);
          detector = _detector->getDetector(stilayer,stiladder);
          if (!detector) throw runtime_error("StiSvtHitLoader::loadHits() -W- detector==0!");
          if (hit->flag()>=4) continue;
          if (hit->flag()< 0) continue;
          stiHit = _hitFactory->getInstance();
          stiHit->setGlobal(detector,hit,hit->position().x(),hit->position().y(),hit->position().z(),hit->charge() );
          hitTest.add(stiHit->x(),stiHit->y(),stiHit->z());
          _hitContainer->add( stiHit );
	  hitCounter++;
        }
        if (hitTest.getN()< 10) continue;
        double w=hitTest.width();
        double dx = detector->getPlacement()->getNormalRadius()-hitTest.center()[0];;
        double ay = hitTest.yAngle()*180/3.1415;
        double az = hitTest.zAngle()*180/3.1415;

        if (w< 0.1 && fabs(dx)<1 && fabs(ay)<1 && fabs(az)<1) continue;
	printf("**** SVT geom problem: barrel=%d ladder%d wafer%d\n",barrel,ladder,wafer);
        printf("**** SVT dX=%g aY=%g aZ=%g\n\n",dx,ay,az);
      }
    }
  }
  cout <<"StiSvtHitLoader::loadHits() -I- SVT Hits added:"<<hitCounter<<endl;
  cout <<"StiSvtHitLoader::loadHits() -I- Hit Container size:"<<_hitContainer->size()<<endl;
  cout <<"StiSvtHitLoader::loadHits() -I- Done"<<endl;
}

#if 0
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
#endif
