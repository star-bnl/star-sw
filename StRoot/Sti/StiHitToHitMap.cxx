#include "StiHitToHitMap.h"
#include "StiHitContainer.h"
#include "Sti/Base/AssociationFilter.h"

StiHitToHitMap::StiHitToHitMap()
{}

StiHitToHitMap::~StiHitToHitMap()
{}

void StiHitToHitMap::build(StiHitContainer * firstContainer,
			   StiHitContainer * secondContainer,
			   AssociationFilter<StiHit> * associationFilter)
{
  StiHit * bestHit;
  StiHit * hit;
  double quality;
  double bestQuality;
  StiHit * firstHit;
  const HitVectorType & hits  = firstContainer->getAllHits();
  for (HitVectorType::const_iterator iter=hits.begin();iter!=hits.end();iter++)
    {
      firstHit = *iter;
      bestHit     = 0;
      bestQuality = 0;
      secondContainer->setRefPoint(firstHit->position(),
				   firstHit->refangle(),
				   firstHit->y(),
				   firstHit->z(),
				   true);
      
      secondContainer->setDeltaD(10.);
      secondContainer->setDeltaZ(10.);
      while (secondContainer->hasMore())
	{
	  hit = secondContainer->getHit();
	  if (associationFilter->filter(firstHit,hit))
	    {
	      quality = associationFilter->getQuality();
	      if (quality>bestQuality)
		{
		  bestHit     = hit;
		  bestQuality = quality; 
		}
	    }
	}
      this->operator[](firstHit) = bestHit;
    }
}

// determine the fraction of hits that associated
void StiHitToHitMap::analyze()
{ 
  double analyzed = 0;
  double matched  = 0;
  for(HitToHitMap::iterator iter=begin();iter!=end(); ++iter)
    {
      ++analyzed;
      if (iter->second)
	++matched;
    }
  cout << " StiHitToHitMap::analyze()"<<endl
       << "   analyzed:" << analyzed << endl
       << "    matched:" << matched << endl;
  if (analyzed>0)
    cout << " hit matching efficiency:" << matched/analyzed << endl;
}
