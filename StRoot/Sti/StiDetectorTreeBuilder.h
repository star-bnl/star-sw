//StiDetectorTreeBuilder.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiDetectorTreeBuilder_HH
#define StiDetectorTreeBuilder_HH

#include <vector>
using std::vector;
#include "StiCompositeTreeNode.h"
#include "StiFactoryTypedefs.h"

class StiDetectorBuilder;

class StiDetectorTreeBuilder
{
public:
    
    StiDetectorTreeBuilder();
    virtual ~StiDetectorTreeBuilder();

    data_node* build(const char* path, data_node_factory* nodefactory, 
                     detector_factory* detfactory);
    
private:
    void loopOnDetectors(const char* path);
    void buildRoot();
    void addToTree(StiDetector*);
    data_node* hangWhere(data_node* parent, StiOrderKey_t order, 
                         string& keystring);
    
    data_node* mroot;
    data_node_factory* mnodefactory;
    detector_factory* mdetfactory;

    StiDetectorBuilder *mDetectorBuilder;

    data_node* mregion;
    
};

#endif
