#include "StEEmcBaseCluster.h"

ClassImp(StEEmcBaseCluster);

StEEmcBaseCluster::StEEmcBaseCluster()
{
  // initialize storage for associated cluster keys
  std::vector<Int_t> temp;
  for ( Int_t i=0;i<6;i++ ) mMatched.push_back(temp);
  mKey=-1;
}
