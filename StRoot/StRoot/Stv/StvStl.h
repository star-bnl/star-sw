
#ifndef StvStl_HH
#define StvStl_HH
#include <vector>
#include <list>
class StvVoids : public std::vector<void*>
{
public:
StvVoids &operator+=(void *add) {push_back(add);return *this;}
};
class StvHit;
class StvHits : public std::vector<StvHit*>
{
public:
StvHits &operator+=(      StvHit  *add)		{push_back(add);return *this;}
StvHits &operator+=(const StvHits &add);
StvHits &operator+=(const std::vector<void*> &add);
void Print(const char *txt="");
};
class StvConstHits : public std::vector<const StvHit*>
{
public:
StvConstHits &operator+=(const StvHit  *add) {push_back(add);return *this;}
};

class StvPoints : public std::vector<float>{
public:
StvPoints &operator+=(const float  add[3]);
StvPoints &operator+=(const double add[3]);
};

class StvNode;
typedef std::list<StvNode*>::iterator 		StvNodeIter;
typedef std::list<StvNode*>::const_iterator 	StvNodeConstIter;
typedef std::list<StvNode*>::reverse_iterator 	StvBakwNodeIter;
class StvNodes : public std::list<StvNode*>{
public:
};
class StvTrack;
typedef std::list<StvTrack*>::iterator 		StvTrackIter;
typedef std::list<StvTrack*>::const_iterator 	StvTrackConstIter;
class StvTracks : public std::list<StvTrack*>{
public:
};
//_____________________________________________________________________________
inline StvHits &StvHits::operator+=(const StvHits &add)
{ insert(end(),add.begin(),add.end()); return *this;}
//_____________________________________________________________________________
inline StvHits &StvHits::operator+=(const std::vector<void*> &add)
{
  const StvHits &myAdd = (const StvHits &)add;
  (*this)+=myAdd;
  return *this;
}
#endif
