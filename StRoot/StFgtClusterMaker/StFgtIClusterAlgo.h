// $Id: StFgtIClusterAlgo.h,v 1.2 2011/08/24 14:30:44 avossen Exp $
// $Log: StFgtIClusterAlgo.h,v $
// Revision 1.2  2011/08/24 14:30:44  avossen
// Continued raw maker development
//
// Revision 1.1  2011/08/23 03:05:09  avossen
// *** empty log message ***
//
//
//author Anselm Vossen
//
//abstract base class for cluster algorithm implementation
//
//
class StFgtIClusterAlgo
{
 public:
  //subclasses must implement this function that takes raw hits from StEvent and fills the Cluster collection
  virtual void doClustering()=0;
}
