#include "StiHitToHitMap.h"
#include "StiHitContainer.h"
#include "Sti/Base/AssociationFilter.h"

StiHitToHitMap::StiHitToHitMap()
{}

StiHitToHitMap::~StiHitToHitMap()
{}

void StiHitToHitMap::build(StiHitContainer & firstContainer,
			   StiHitContainer & secondContainer,
			   AssociationFilter<StiHit> & associationFilter)
{
  StiHit * bestHit;
  StiHit * hit;
  double quality;
  double bestQuality;
  StiHit * firstHit;
  const vector<StiHit*> & hits1  = firstContainer.getHits();
  for (vector<StiHit*>::const_iterator iter1=hits1.begin();iter1!=hits1.end();iter1++)
    {
      firstHit = *iter1;
      bestHit     = 0;
      bestQuality = 0;
      
      const vector<StiHit*> & hits2  = secondContainer.getHits(firstHit->position(),
							       firstHit->refangle(),
							       firstHit->y(),
							       firstHit->z(),
							       10.,10.,
							       true);
      for (vector<StiHit*>::const_iterator iter2=hits2.begin();iter2!=hits2.end();iter2++)
	{
	  hit = *iter2;
	  if (associationFilter.filter(firstHit,hit))
	    {
	      quality = associationFilter.getQuality();
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
