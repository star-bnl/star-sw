#ifndef StiHitToHitMap_H_INCLUDED
#define StiHitToHitMap_H_INCLUDED
#include <map>
using namespace std;
class StiHit;
class StiHitContainer;
typedef map<StiHit*,StiHit*> HitToHitMap;
template<class FILTERED> class AssociationFilter;


class StiHitToHitMap : public HitToHitMap
{
 public:

  StiHitToHitMap();
  virtual ~StiHitToHitMap();
  void build(StiHitContainer * firstContainer,
	     StiHitContainer * secondContainer,
	     AssociationFilter<StiHit> * associationFilter);
  void analyze();
};


#endif
