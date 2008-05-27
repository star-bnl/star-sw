// $Id: StEVPTpcCluser.h,v 1.1.1.1 2008/05/27 14:22:41 fisyak Exp $
// $Log: StEVPTpcCluser.h,v $
// Revision 1.1.1.1  2008/05/27 14:22:41  fisyak
// Maker to access TPC DAQ information via EVP_READER
//
// Revision 1.1.1.1  2008/04/03 20:16:41  fisyak
// Initial version
//
#ifndef STAR_StEVPTpcCluser
#define STAR_StEVPTpcCluser

//__________________________________________________
//
//  StEVPTpcCluser is the concrete impplementation 
//  of the StDaqTpcClusterInterface for the clusters 
//  provided via EVP_READER::tpcReader::tpc::tpc_cl
//__________________________________________________

#include "StTpcHitMaker/StDaqTpcClusterInterface.h"

struct tpc_cl;

class StEVPTpcCluser : public StDaqTpcClusterInterface {

      const tpc_cl *fDaqTpcCluster;

  public:
        StEVPTpcCluser(tpc_cl *cl=0) : fDaqTpcCluster(cl){}
        virtual ~StEVPTpcCluser()  {}
        void  setTpcCl(tpc_cl *cl) { fDaqTpcCluster = cl; }
        float pad()           const;
        float time()          const;
        float charge()        const;
        short minPad()        const;
        short maxPad()        const;
        short minTimeBucket() const;
        short maxTimeBucket() const;
};

#endif
