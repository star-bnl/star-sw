#ifndef StiMasterDetectorBuilder_H
#define StiMasterDetectorBuilder_H

#include "Sti/Base/Vectorized.h"
#include "StiDetectorBuilder.h"

class StiMasterDetectorBuilder : public StiDetectorBuilder, public Vectorized<StiDetectorBuilder>
{
public:
    StiMasterDetectorBuilder();
    virtual ~StiMasterDetectorBuilder(); 
    virtual bool hasMore() const;
    virtual StiDetector * next();
    virtual void reset();
    virtual void build();
    virtual void add(StiDetectorBuilder *builder);
};

#endif 
