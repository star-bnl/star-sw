
#ifndef StiStl_HH
#define StiStl_HH
#include <vector>
#include <list>
class StiHit;

class StiHits : public std::vector<StiHit*>
{
public:
StiHits &operator+=(      StiHit  *add)		{push_back(add);return *this;}
StiHits &operator+=(const StiHits &add);
void unused();
};

class StiPoints : public std::vector<float>{
public:
StiPoints &operator+=(const float  add[3]);
StiPoints &operator+=(const double add[3]);
};

class StiNode;
typedef std::list<StiNode*>::iterator 		StiNodeIter;
typedef std::list<StiNode*>::const_iterator 	StiNodeConstIter;
typedef std::list<StiNode*>::reverse_iterator 	StiBakwNodeIter;
class StiNodes : public std::list<StiNode*>{
public:
};
class StiTrack;
typedef std::list<StiTrack*>::iterator 		StiTrackIter;
typedef std::list<StiTrack*>::const_iterator 	StiTrackConstIter;
class StiTracks : public std::list<StiTrack*>{
public:
};

#endif
