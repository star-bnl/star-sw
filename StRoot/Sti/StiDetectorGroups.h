#ifndef StiDetectorGroups_H_INCLUDED
#define StiDetectorGroups_H_INCLUDED
#include <vector>
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/StiDetectorGroup.h"

template<class Event, class McEvent>
class StiDetectorGroups : public Named, public Described, public vector<StiDetectorGroup<Event,McEvent>* >
{
public:
    StiDetectorGroups(const string &name,const string &description);
    virtual ~StiDetectorGroups(){}; 
};

template<class Event, class McEvent>
StiDetectorGroups<Event,McEvent>::StiDetectorGroups(const string &name,const string &description )
  : Named(name),
    Described(description)
{}
#endif 
