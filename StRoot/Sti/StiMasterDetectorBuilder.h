#ifndef StiMasterDetectorBuilder_H
#define StiMasterDetectorBuilder_H

#include <vector>
#include "StiDetectorBuilder.h"

class StiMasterDetectorBuilder : public StiDetectorBuilder
{

public:
    StiMasterDetectorBuilder();
    virtual ~StiMasterDetectorBuilder(); 
    void addBuilder(StiDetectorBuilder *builder);
    virtual bool hasMore() const;
    virtual StiDetector * next();
    virtual void reset();
    virtual void build();

 protected:
    vector<StiDetectorBuilder*> _builders;
};

#endif 
