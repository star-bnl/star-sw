#include "StiHitToHitMap.h"

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
  int      quality;
  int      bestQuality;
  StiHit * firstHit;
  const HitVectorType & hits  = firstContainer->getAllHits();
  for (HitVectorType::const_iterator iter=hits.begin();iter!=hits.end();iter++)
    {
      firstHit = *iter;
      bestHit     = 0;
      bestQuality = 0;
      secondContainer->setRefPoint(firstHit);
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

