// $Id: StDaqTpcClusterInterface.h,v 1.1.1.1 2008/05/27 14:22:40 fisyak Exp $
// $Log: StDaqTpcClusterInterface.h,v $
// Revision 1.1.1.1  2008/05/27 14:22:40  fisyak
// Maker to access TPC DAQ information via EVP_READER
//
// Revision 1.1.1.1  2008/04/03 20:16:40  fisyak
// Initial version
//
#ifndef STAR_StDaqTpcClusterInterface
#define STAR_StDaqTpcClusterInterface

// an abstract interface to get access to the Daq TPC cluster
class StDaqTpcClusterInterface {
  public:
    virtual ~StDaqTpcClusterInterface(){;}
    virtual float pad()           const = 0;
    virtual float time()          const = 0;
    virtual float charge()        const = 0;
    virtual short minPad()        const = 0;
    virtual short maxPad()        const = 0;
    virtual short minTimeBucket() const = 0;
    virtual short maxTimeBucket() const = 0;
};
#endif
