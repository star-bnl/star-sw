//StxKalmanTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include "Stiostream.h"
#include <algorithm>
using namespace std;

#include "StxKalmanTrack.h"
#include "StxKalmanTrack.h"
#include "StxKalmanTrackContainer.h"
ClassImp(StxKalmanTrackContainer);
StxKalmanTrackContainer::StxKalmanTrackContainer(const Char_t * name, const Char_t * description)
  : TNamed(name,description)
{
  cout <<"StxKalmanTrackContainer::StxKalmanTrackContainer() -I- Started with name:"<< name <<endl;
}

StxKalmanTrackContainer::~StxKalmanTrackContainer()
{
  cout <<"StxKalmanTrackContainer::~StxKalmanTrackContainer() -I- Done"<<endl;
}



bool StxKalmanTrackLessThan::operator()(const StxKalmanTrack* lhs, const StxKalmanTrack* rhs) const
{
  int lN = ((StxKalmanTrack*)lhs)->NNodes(3);
  int rN = ((StxKalmanTrack*)rhs)->NNodes(3);
  return lN >= rN;


}
void StxKalmanTrackContainer::sort() 
{
  std::sort(begin(),end(),StxKalmanTrackLessThan());
}
