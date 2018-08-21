//StxDetectorContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <algorithm>
#include "TList.h"
using std::find_if;
using std::for_each;
using std::binary_search;
#include "StxDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "StxDetectorNodeLeafIterator.h"
#include "StxDetectorContainer.h"
ostream& operator<<(ostream&, const NameMapKey&);
ostream& operator<<(ostream*, const StxDetector&);
ClassImp(StxDetectorContainer);
//________________________________________________________________________________
void StxDetectorContainer::Initialize() {
  cout << "StxDetectorContainer::Initialize() -I- Started" << endl;
  _sortedDetectors.reserve(1000);
  _sortedDetectors.clear();
  _selectedDetectors.clear();
  _detectorsHash = new THashList(512);
  
}
//________________________________________________________________________________
//void StxDetectorContainer::Build(StxDetectorBuilder * Builder) {
void StxDetectorContainer::Build() {
  cout <<"StxDetectorContainer::Build() -I- Starting"<<endl;
  // Build the volumes
  //Builder->Build();
  // pass volumes to TreeBuilder before we make like a tree and leave...
  StxDetectorTreeBuilder treeBuilder;
  mRoot = treeBuilder.Build();
  assert(mRoot);

  StxDetector * detector;
  for (vector<StxDetector*>::const_iterator i=_sortedDetectors.begin();
       i!=_sortedDetectors.end(); ++i) {
    detector = *i;
    treeBuilder.AddToTree(detector);
  }
  treeBuilder.SortTree();
  cout <<"StxDetectorContainer::Build() -I- Done"<<endl;
  return;
}
//________________________________________________________________________________
/*! This is used, e.g., to set the iterators to a certain point in
preparation  for propogation of a new track.  If no StxDetector pointer is
found that is equal to <b>layer</b>, then an error message is streamed to
the screen and Reset() is called.
*/
void StxDetectorContainer::SetToDetector(const StxDetector* layer) {
  assert(layer->TreeNode());
  SetToLeaf( layer->TreeNode() );
}
//________________________________________________________________________________
/// A call to Reset simply sets the pointer to the default StxDetector object.
/// It does not alter the state of the detector model.
void StxDetectorContainer::Reset() {
  SameOrderKey mySameOrderKey;
  StxOrderKey tempOrderKey;
  tempOrderKey.key = static_cast<double>( StxDetector::kMidRapidity );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StxOrderKey&
  StxDetectorNode::StxDetectorNodeVector::iterator where = find_if(mRoot->begin(), mRoot->end(), mySameOrderKey);
  assert(! (where==mRoot->end()));
  mregion = mregion = where;
  //This will seg fault if (*mregion)->begin()==(*mregion)->end() !!!!!!!
  mradial_it = (*mregion)->begin(); //change (MLM)
  mphi_it = (*mradial_it)->begin();
}
//________________________________________________________________________________
StxDetector* StxDetectorContainer::operator*() const {
  assert ((*mphi_it));
  StxDetector * det = (*mphi_it)->Data();
  assert (det);
  return det;
}
//________________________________________________________________________________
StxDetector* StxDetectorContainer::CurrentDetector() const {
  assert ((*mphi_it));
  StxDetector * det = (*mphi_it)->Data();
  assert (det);
  return det;
}
//________________________________________________________________________________
/*! A call to MoveIn() may not always alter the StxDetector to which the
container points.  Notably, if there is nowhere else to 'move in to', then
MoveIn() will have no action.  So, to see if the action succeeded, one must
store a pointer to the StxDetector represented by the current state of the
container, call MoveIn(), and then check that the pointer to the
StxDetector represented by the new state of the container is different than
that of the previous state. <p>
Additionally, when a call to MoveIn() is made, the container 'selects' the
StxDetector object that is closest in phi to the StxDetector object that is
being 'movedIn' from.  Therefore, a call to MoveIn() usually need not be
followed by a call to movePlusPhi() or moveMinusPhi(), except in cases of
extreme assymetry, such as navigation through the Silicon Vertex Tracker.
*/
bool StxDetectorContainer::MoveIn() {
  if (mradial_it == (*mregion)->begin() ) return false;
  //remember where we started:
  const StxDetectorNode* oldPhiNode = *mphi_it;
  --mradial_it;
  mphi_it = (*mradial_it)->begin();
  if ( (*mradial_it)->ChildCount() == oldPhiNode->Parent()->ChildCount()) {
    mphi_it = (*mradial_it)->begin()+oldPhiNode->OrderKey().index;
    return true;
  }
  else return SetPhi( oldPhiNode->OrderKey() );
}
//________________________________________________________________________________
bool StxDetectorContainer::SetPhi(const StxOrderKey& oldOrder) {
  mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
                                 (*mradial_it)->end(), oldOrder);
  if (mphi_it == (*mradial_it)->end()) {
    cout <<"StxDetectorContainer::SetPhiIterator() -E- Find Phi failed"<<endl;
    Reset();
    return false;
  }
  return true;
}
//________________________________________________________________________________
//We assume that the node is a leaf in phi
void StxDetectorContainer::SetToLeaf(StxDetectorNode* leaf) {
  //Now we try the new index-iterator scheme:
  mphi_it = leaf->whereInParent();
  if (mphi_it == leaf->end()) {
    cout <<"StxDetectorContainer::SetToLeaf(StxDetectorNode*). ERROR:\t"
	 <<"Node not found in parent.  Abort"<<endl;
    Reset();
    return;
  }

  StxDetectorNode* parentInRadius = (*mphi_it)->Parent();
  mradial_it = parentInRadius->whereInParent();
  if (mradial_it == parentInRadius->end()) {
    cout <<"StxDetectorContainer::SetToLeaf(StxDetectorNode*) -E- Node not found in parent.  Abort?"<<endl;
    Reset();
    return;
  }
}
//________________________________________________________________________________
void StxDetectorContainer::Add(StxDetector* det) { 
  _sortedDetectors.push_back(det); 
  sort(_sortedDetectors.begin(), _sortedDetectors.end(), RPhiLessThan());
  _detectorsHash->AddLast(det);
}
//________________________________________________________________________________
vector<StxDetector*> & StxDetectorContainer::Detectors() {
  return _sortedDetectors;
}
//________________________________________________________________________________
vector<StxDetector*>::iterator  StxDetectorContainer::begin()
{
  cout << "StxDetectorContainer::begin() -I- size:"<<_sortedDetectors.size()<<endl;
  return _sortedDetectors.begin();
}
//________________________________________________________________________________
vector<StxDetector*>::iterator  StxDetectorContainer::end()
{  
  return _sortedDetectors.end();
}
//________________________________________________________________________________
vector<StxDetector*>::const_iterator  StxDetectorContainer::begin() const
{
  cout << "StxDetectorContainer::begin() const -I- size:"<<_sortedDetectors.size()<<endl;
  return _sortedDetectors.begin();
}
//________________________________________________________________________________
vector<StxDetector*>::const_iterator  StxDetectorContainer::end() const
{  
  return _sortedDetectors.end();
}
//________________________________________________________________________________
//Non members
//________________________________________________________________________________
//sort in descending order in radius, and ascending order in phi
bool RPhiLessThan::operator()(const StxDetector* lhs, const StxDetector* rhs) {
  if (!lhs || !rhs)     {
      cout << "RPhiLessThan::operator() -E- !lhs || !rhs "<<endl;
      return false;
  }
  if      (lhs->Key(1)<rhs->Key(1))    return false;
  else if (lhs->Key(1)>rhs->Key(1))    return true;
  double la = lhs->Key(2);
  double ra = rhs->Key(2);
  if ((la<0) != (ra<0))  { if (la<0) la+=2*M_PI;if (ra<0) ra+=2*M_PI;}
  return (la<ra);
}
//________________________________________________________________________________
void StxDetectorContainer:: Print(Option_t *option) const {
  TListIter next( _detectorsHash );
  TObject *o = 0;
  while ((o = next())) {
    o->Print();
  }
}
