//StiKalmanTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include "Stiostream.h"
#include <algorithm>
using namespace std;

#include "StiKalmanTrack.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackContainer.h"
ClassImp(StiKalmanTrackContainer);
StiKalmanTrackContainer::StiKalmanTrackContainer(const Char_t * name, const Char_t * description)
  : TNamed(name,description)
{
  cout <<"StiKalmanTrackContainer::StiKalmanTrackContainer() -I- Started with name:"<< name <<endl;
}

StiKalmanTrackContainer::~StiKalmanTrackContainer()
{
  cout <<"StiKalmanTrackContainer::~StiKalmanTrackContainer() -I- Done"<<endl;
}



bool StiKalmanTrackLessThan::operator()(const StiKalmanTrack* lhs, const StiKalmanTrack* rhs) const
{
  int lN = ((StiKalmanTrack*)lhs)->NNodes(3);
  int rN = ((StiKalmanTrack*)rhs)->NNodes(3);
  return lN >= rN;


}
void StiKalmanTrackContainer::sort() 
{
  std::sort(begin(),end(),StiKalmanTrackLessThan());
}
