#ifndef TTYPES_INC
#define TTYPES_INC
// common types

#include "Common.h"
#include "lists.hpp"

class THit;
class TTrack;

// define type "list of pointers to hits"
typedef slist <THit*> THitList;
// singly linked list of tracks
typedef slist <TTrack*> TTrackList;

#ifdef max
#undef max
#undef min
#endif

#define min(a,b)        ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)        ( ( (a) > (b) ) ? (a) : (b) )

#endif
