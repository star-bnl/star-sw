//StiDetectorTreeBuilder.h
//M.L. Miller (Yale Software)
//07/01

#ifndef StiDetectorTreeBuilder_HH
#define StiDetectorTreeBuilder_HH

#include <vector>
using std::vector;
#include "StiCompositeTreeNode.h"

#include "StiFactoryTypes.h"
#include "StiObjectFactoryInterface.h"

class StiDetectorBuilder;

class StiDetectorTreeBuilder
{
public:
    
    StiDetectorTreeBuilder();
    virtual ~StiDetectorTreeBuilder();

    data_node* build(StiObjectFactoryInterface<StiDetectorNode>* nodefactory, StiObjectFactoryInterface<StiDetector>* detfactory);
    
private:
    void loopOnDetectors();
    void buildRoot();
    void addToTree(StiDetector*);
    data_node* hangWhere(data_node* parent, StiOrderKey_t order, 
                         string& keystring);
    
    data_node* mroot;
    StiObjectFactoryInterface<StiDetectorNode>* mnodefactory;
    StiObjectFactoryInterface<StiDetector>* mdetfactory;

    StiDetectorBuilder *mDetectorBuilder;

    data_node* mregion;
    
};

#endif
