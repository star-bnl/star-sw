// $Id: StEVPTpcCluser.cxx,v 1.1.1.1 2008/05/27 14:22:40 fisyak Exp $
// $Log: StEVPTpcCluser.cxx,v $
// Revision 1.1.1.1  2008/05/27 14:22:40  fisyak
// Maker to access TPC DAQ information via EVP_READER
//
// Revision 1.1.1.1  2008/04/03 20:16:40  fisyak
// Initial version
//
#include "StEVPTpcCluser.h"

//__________________________________________________
//
//  StEVPTpcCluser is the concrete impplementation 
//  of the StDaqTpcClusterInterface for the clusters 
//  provided via EVP_READER::tpcReader::tpc::tpc_cl
//__________________________________________________

// trick to parse the tpcReader.h header file frpom EVP_READER
#ifndef NULL
#  define NULL 0
#endif

#include "RTS/src/EVP_READER/tpcReader.h"

//___________________________________
float StEVPTpcCluser::pad() const 
{ return fDaqTpcCluster->p;          }

//___________________________________
float StEVPTpcCluser::time() const 
{ return fDaqTpcCluster->t;          }

//___________________________________
float StEVPTpcCluser::charge() const
{ return fDaqTpcCluster->charge;     }
//___________________________________
short StEVPTpcCluser::minPad() const
{ return fDaqTpcCluster->p1;         }
//___________________________________
short StEVPTpcCluser::maxPad() const
{ return fDaqTpcCluster->p2;         }
//___________________________________
short StEVPTpcCluser::minTimeBucket() const
{ return fDaqTpcCluster->t1;         }
//___________________________________
short StEVPTpcCluser::maxTimeBucket() const
{ return fDaqTpcCluster->t2;         }
