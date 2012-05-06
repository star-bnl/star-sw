
#ifndef StvStl_HH
#define StvStl_HH
#include <vector>
#include <list>
class StvHit;

class StvHits : public std::vector<StvHit*>
{
public:
StvHits &operator+=(      StvHit  *add)		{push_back(add);return *this;}
StvHits &operator+=(const StvHits &add);
void unused();
};
class StvConstHits : public std::vector<const StvHit*>
{
public:
StvConstHits &operator+=(const StvHit  *add) {push_back(add);return *this;}
void unused();
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

#endif
