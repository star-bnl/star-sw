#ifndef StiDetectorGroups_H_INCLUDED
#define StiDetectorGroups_H_INCLUDED
#include <vector>
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
class StiGenericDetectorGroup;

class StiDetectorGroups : public Named, public Described, public vector<StiGenericDetectorGroup * >
{
public:
    StiDetectorGroups(const string &name,const string &description) : Named(name), Described(description) {}
    virtual ~StiDetectorGroups() {} 
};
#endif 
