//StiDetectorTreeBuilder_ex.cxx

//This function demonstrates how to use StiDetectorTreeBuilder.
//It is an excerpt from StiDetectorContainer.cxx
int main()
{
    //The StiDetector factory
    //We'll make a drawable represenation, to demonstrate the power of the factory
    //method
    StiObjectFactoryInterface<StiDetector>* detectorfactory;
    mdetectorfactory = new StiRDDetectorFactory("RDDetectorFactory");
    detectorfactory->reset();

    //The DetectorNodeFactory
    StiObjectFactoryInterface<StiDetectorNode>* datanodefactory; 
    datanodefactory = new StiDetectorNodeFactory("DetectorNodeFactory");
    datanodefactory->reset();

    //This is just a useful typedef
    typedef StiCompositeTreeNode<StiDetector> data_node;

    //Instantiate the builder
    StiDetectorTreeBuilder mybuilder;
    //Actually build the 3d representation of the STAR detector in memory
    data_node* root = mybuilder.build(datanodefactory, detectorfactory);

    delete datanodefactory;
    delete detectorfactory;
    
    return 1;
}
