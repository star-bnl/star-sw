//StiDetectorContainer_ex.cxx

//This is an example of how to use StiDetectorContainer
int main()
{
    //The StiDetector factory
    StiObjectFactoryInterface<StiDetector>* detectorfactory;
    //Decide which type of derived factory to create
    if (UseGui==true)
	mdetectorfactory = new StiRDDetectorFactory("RDDetectorFactory");
    else 
	mdetectorfactory = new StiDetectorFactory("DetectorFactory");
    detectorfactory->reset();

    //The DetectorNodeFactory
    StiObjectFactoryInterface<StiDetectorNode>* datanodefactory; 
    datanodefactory = new StiDetectorNodeFactory("DetectorNodeFactory");
    datanodefactory->reset();
    
    //The Detector Tree
    StiDetectorContainer& store = *(StiDetectorContainer::instance());
    store.buildDetectors(datanodefactory, detectorfactory);
    store.reset();

    //Now navigate through the detector:
    store.setToPosition(1000000.); //get the outermost layer
    //Now more in until we can't go any further
    bool go=true;
    //This is how we get a StiDetector layer from an StiDetectorContainer instance
    StiDetector* layer= *store; //Dereference iterator
    while (go) {
	store.moveIn();
	if (layer!=*store) {
	    //That means that we haven't been here yet.
	    layer=*store; //Remember, continue
	}
	else {
	    //That means that there is no place else to move into
	    go=false;
	}
    }

    //Cleanup the objects owned by factories
    delete detectorfactory;
    delete datanodefactory;
    
    return 1;
    //Notice that we didin't call StiDetectorContainer::kill().
    //That is not a memory leak, it will be automatically destroyed when
    //we leave main
}
