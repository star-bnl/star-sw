//StiDetectorContainer.cxx
//M.L. Miller (Yale Software)
//02/02/01

#include <Stiostream.h>
#include <math.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string>
#include <stdexcept>
#include <algorithm>
using std::find_if;
using std::for_each;
using std::binary_search;
#include "Sti/Base/Filter.h"
#include "Sti/StiDetectorTreeBuilder.h"
#include "Sti/StlUtilities.h"
#include "Sti/StiCompositeLeafIterator.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
ostream& operator<<(ostream&, const NameMapKey&);
ostream& operator<<(ostream*, const StiDetector&);

StiDetectorContainer::StiDetectorContainer(const string & name,
                                           const string & description,
                                           StiMasterDetectorBuilder* masterBuilder)
: Named(name),
  Described(description),
  mroot(0),
  //mregion(0),
  mLeafIt(0),
  _masterDetectorBuilder(masterBuilder)
{
  cout <<"StiDetectorContainer::StiDetectorContainer() -I- Started/Done"<<endl;
}

StiDetectorContainer::~StiDetectorContainer()
{
  cout <<"StiDetectorContainer::~StiDetectorContainer() -I- Started"<<endl;
  if (mLeafIt) {
    delete mLeafIt;
    mLeafIt=0;
  }
  cout <<"StiDetectorContainer::~StiDetectorContainer() -I- Done"<<endl;
}

void StiDetectorContainer::initialize()
{
  cout << "StiDetectorContainer::initialize() -I- Started" << endl;
  cout << "StiDetectorContainer::initialize() -I- Use master builder to build detectors" << endl;
  build(_masterDetectorBuilder);
  cout << "StiDetectorContainer::initialize() -I- Now extract detectors and add them to the _sortedDetectors" << endl;
  _sortedDetectors.reserve(1000);
  _sortedDetectors.clear();
  _selectedDetectors.clear();
  vector<StiDetectorBuilder*>::iterator bIter;
  for (bIter=_masterDetectorBuilder->begin(); bIter!=_masterDetectorBuilder->end(); ++bIter)
    {
    string name = (*bIter)->getName();
    //cout << "Detector:"<< name<<endl;
    int nRows = (*bIter)->getNRows();
    for (int row=0;row<nRows;row++)
      {
      int nSectors = (*bIter)->getNSectors(row);
      for (int sector=0; sector<nSectors; sector++)
        {
        StiDetector* detector = (*bIter)->getDetector(row,sector);
//        if (!detector) throw runtime_error("StiDetectorContainer::build() -F- detector==0 ");
        if (!detector){ 
          Warning("StiDetectorContainer::initialize() %s(%d,%d)=0",name.c_str(),row,sector);
          continue;
	}
        if (detector->isActive()) 
	  add(detector);
//        else 
//	  cout <<"StiDetectorContainer::initialize() -I-  Not Adding detector unit: "<< detector->getName()<<endl;
        }
      }
    }
  cout << "StiDetectorContainer::initialize() -I- Done" << endl;
}


/*! This is used, e.g., to set the iterators to a certain point in
preparation  for propogation of a new track.  If no StiDetector pointer is
found that is equal to <b>layer</b>, then an error message is streamed to
the screen and reset() is called.
*/
void StiDetectorContainer::setToDetector(const StiDetector* layer)
{
  if (!layer->getTreeNode()) 
    throw runtime_error("StiDetectorContainer::setToDetector(StiDetector*) -E- layer->getTreeNode()==0");
  setToLeaf( layer->getTreeNode() );
}

/// A call to reset simply sets the pointer to the default StiDetector object.
/// It does not alter the state of the detector model.
void StiDetectorContainer::reset()
{
  SameOrderKey<StiDetector> mySameOrderKey;
  StiOrderKey tempOrderKey;
  tempOrderKey.key = static_cast<double>( StiPlacement::kMidRapidity );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&
  StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameOrderKey);
  if (where==mroot->end()) 
    throw runtime_error("StiDetectorContainer::setToDetector(StiDetector*) -E-  mid-rapidity region not found - where==0");
  mregion = where;
  //This will seg fault if (*mregion)->begin()==(*mregion)->end() !!!!!!!
  mradial_it = (*mregion)->begin(); //change (MLM)
  mphi_it = (*mradial_it)->begin();
}

StiDetector* StiDetectorContainer::operator*() const
{
  if (!(*mphi_it))
    throw runtime_error("StiDetectorContainer::operator*() const -E- *mphi_it==0");
  StiDetector * det = (*mphi_it)->getData();
  if (!det)
    throw runtime_error("StiDetectorContainer::operator*() const -E- *mphi_it==0");
  return det;
}

StiDetector* StiDetectorContainer::getCurrentDetector() const
{
  if (!(*mphi_it))
    throw runtime_error("StiDetectorContainer::getCurrentDetector() const -E- *mphi_it==0");
  StiDetector * det = (*mphi_it)->getData();
  if (!det)
    throw runtime_error("StiDetectorContainer::getCurrentDetector() const -E- *mphi_it==0");
  return det;
}

#if 0
bool StiDetectorContainer::moveToNextRegion()
{
  //cout <<"StiDetectorContainer::moveToNextRegion()"<<endl;
StiDetectorNodeVector::const_iterator region = mregion;
  if ( ++region == mroot->end() )
    {
      //cout <<"StiDetectorContainer::moveToNextRegion() -I- Nowhere to go."<<endl;
    return false;
    }
  //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK"<<endl;

StiDetectorNodeVector::const_iterator radial_it = (*mregion)->begin();
  //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 2"<<endl;
  if (radial_it==(*mregion)->end())
    return false;
StiDetectorNodeVector::const_iterator phi_it = (*radial_it)->begin();
  //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 3"<<endl;
  if (phi_it==(*radial_it)->end())
    return false;
  //cout <<"StiDetectorContainer::moveToNextRegion() -I- new region OK 4"<<endl;

  mregion    = region;
  mradial_it = radial_it;
  mphi_it    = phi_it;
  //cout <<"StiDetectorContainer::moveToNextRegion() -I- moved to new region OK"<<endl;
  return true;
}

bool StiDetectorContainer::moveToPreviousRegion()
{
  if (mregion == mroot->begin() ) 
    {
      //cout <<"StiDetectorContainer::moveToPreviousRegion():\t Nowhere to go. return false"<<endl;
      return false;
    }

  --mregion;
  //now reset to beginning of region
  mradial_it = (*mregion)->begin();
  mphi_it = (*mradial_it)->begin();
  return true;
}
#endif
/*! A call to moveIn() may not always alter the StiDetector to which the
container points.  Notably, if there is nowhere else to 'move in to', then
moveIn() will have no action.  So, to see if the action succeeded, one must
store a pointer to the StiDetector represented by the current state of the
container, call moveIn(), and then check that the pointer to the
StiDetector represented by the new state of the container is different than
that of the previous state. <p>
Additionally, when a call to moveIn() is made, the container 'selects' the
StiDetector object that is closest in phi to the StiDetector object that is
being 'movedIn' from.  Therefore, a call to moveIn() usually need not be
followed by a call to movePlusPhi() or moveMinusPhi(), except in cases of
extreme assymetry, such as navigation through the Silicon Vertex Tracker.
*/
bool StiDetectorContainer::moveIn()
{
  //if (mradial_it == mregion->begin() ) { //change (MLM)
  if (mradial_it == (*mregion)->begin() ) { //change (MLM)
                                            // cout <<"StiDetectorContainer::moveIn():\t";
                                            // cout <<"Nowhere to go. return false"<<endl;
    return false;
  }

  //remember where we started:
  const StiDetectorNode* oldPhiNode = *mphi_it;

  --mradial_it;
  mphi_it = (*mradial_it)->begin();

  if ( (*mradial_it)->getChildCount() == oldPhiNode->getParent()->getChildCount()) {
    // cout <<"Index into array"<<endl;
    mphi_it = (*mradial_it)->begin()+oldPhiNode->getOrderKey().index;
    return true;
  }
  else {
    // cout <<"Do linear search"<<endl;
    return setPhi( oldPhiNode->getOrderKey() );
  }
  }

bool StiDetectorContainer::setPhi(const StiOrderKey& oldOrder)
{
  mphi_it = gFindClosestOrderKey((*mradial_it)->begin(),
                                 (*mradial_it)->end(), oldOrder);
  if (mphi_it == (*mradial_it)->end()) 
    {
      cout <<"StiDetectorContainer::setPhiIterator() -E- Find Phi failed"<<endl;
      reset();
      return false;
    }
  return true;
}
#if 0
/*! A call to moveOut() may not always alter the StiDetector to which the
container points.  Notably, if there is nowhere else to 'move out to', then
moveOut() will have no action.  So, to see if the action succeeded, one must
store a pointer to the StiDetector represented by the current state of the
container, call moveOut(), and then check that the pointer to the
StiDetector represented by the new state of the container is different than
that of the previous state. <p>
Additionally, when a call to moveOut() is made, the container 'selects' the
StiDetector object that is closest in phi to the StiDetector object that is
being 'movedOut' from.  Therefore, a call to moveIn() usually need not be
followed by a call to movePlusPhi() or moveMinusPhi(), except in cases of
extreme assymetry, such as navigation through the Silicon Vertex Tracker.
*/
bool StiDetectorContainer::moveOut()
{
  //remember where we started:
  const StiDetectorNode* oldPhiNode = *mphi_it;

  //if there's nowher to go, get out before doing work!
  // cout <<"StiDetectorContainer::moveOut()"<<endl;

  //if ( (++mradial_it<mregion->end())==false) { //change (MLM)
  if ( (++mradial_it<(*mregion)->end())==false) { //change (MLM)

    // cout <<"StiDetectorContainer::moveOut():\t";
    // cout <<"Nowhere to go. return false"<<endl;
    --mradial_it;
    return false;
  }

  if ( (*mradial_it)->getChildCount() == oldPhiNode->getParent()->getChildCount()) {
    // cout <<"Index into array"<<endl;
    mphi_it = (*mradial_it)->begin()+oldPhiNode->getOrderKey().index;
    return true;
  }
  else {
    // cout <<"Do linear search"<<endl;
    return setPhi( oldPhiNode->getOrderKey() );
  }
  }

/*! Plus phi is defined as clockwise if viewing sectors 1-12 from the membrane,
increasing phi in STAR TPC global coordinates.  A call to movePlusPhi() will
always have a valid action.  That is, the call will wrap around past 2pi.
*/
void StiDetectorContainer::movePlusPhi()
{
  ++mphi_it;
  if (mphi_it == (*mradial_it)->end()) { //Wrap around 2pi
    mphi_it = (*mradial_it)->begin();
  }
}

/*! Minus phi is defined as counter-clockwise if viewing sectors 1-12 from
the membrane,
decreasing phi in STAR TPC global coordinates.  A call to moveMinusPhi() will
always have a valid action.  That is, the call will wrap around past 2pi.
*/
void StiDetectorContainer::moveMinusPhi()
{
  if (mphi_it == (*mradial_it)->begin()) { //Wrap around 2pi
    mphi_it = (*mradial_it)->end();
  }
  --mphi_it;
}
#endif
/*! Recursively load all detector definition files from the given directory.
There is internal protection to avoid building the detector representation
more than once.
*/
void
StiDetectorContainer::build(StiDetectorBuilder * builder)
{
  cout <<"StiDetectorContainer::build() -I- Starting"<<endl;
  cout <<"StiDetectorContainer::build() -I- Building using builder:"<<builder->getName()<<endl;
  // build the volumes
  //builder->build();
  // pass volumes to TreeBuilder before we make like a tree and leave...
  StiDetectorTreeBuilder treeBuilder;
  mroot = treeBuilder.build(builder);
  if (!mroot)
    throw runtime_error("StiDetectorContainer::build() - ERROR - mroot==0");

  //Don't need any of this!!!!!
  /*
   //Set region to midrapidity, hard-coded for now, update later to allow for other regions
   SameName<StiDetector> mySameName;
   mySameName.mname = "midrapidity";
   StiDetectorNodeVector::iterator where = find_if(mroot->begin(), mroot->end(), mySameName);
   if (where==mroot->end())
   throw runtime_error("StiDetectorContainer::build() - ERROR - mid-rapidity region not found - where==0");
   if (!(*where))
   throw runtime_error("StiDetectorContainer::build() - ERROR - mid-rapidity region not found - *where==0");
   cout <<"StiDetectorContainer::build() -I- Find Leaves and set mregion"<<endl;
   //Sort by name for O(::log(n)) calls to setDetector()
   //sort(mLeafIt->begin(), mLeafIt->end(), DataNameLessThan<StiDetector>() );

   mregion = (*where); //change (MLM)
   */
  mLeafIt = new StiCompositeLeafIterator<StiDetector>(mroot);
  cout <<"StiDetectorContainer::build() -I- Done"<<endl;
  return;
}


//We assume that the node is a leaf in phi
void StiDetectorContainer::setToLeaf(StiDetectorNode* leaf)
{
  //Now we try the new index-iterator scheme:
  mphi_it = leaf->whereInParent();
  if (mphi_it == leaf->end()) {
    cout <<"StiDetectorContainer::setToLeaf(StiDetectorNode*). ERROR:\t"
    <<"Node not found in parent.  Abort"<<endl;
    reset();
    return;
  }

  StiDetectorNode* parentInRadius = (*mphi_it)->getParent();
  mradial_it = parentInRadius->whereInParent();
  if (mradial_it == parentInRadius->end()) {
    cout <<"StiDetectorContainer::setToLeaf(StiDetectorNode*) -E- Node not found in parent.  Abort?"<<endl;
    reset();
    return;
  }
  //cout <<"\nleaf: "<<leaf->getName()<<" "<<leaf->getOrderKey()<<endl;
  //cout <<"*mradial_it: "<<(*mradial_it)->getName()<<" "<<(*mradial_it)->getOrderKey()<<endl;
  //cout <<"*mphi_it: "<<(*mphi_it)->getName()<<" "<<(*mphi_it)->getOrderKey()<<endl;

}

void StiDetectorContainer::add(StiDetector* det)
{
  _sortedDetectors.push_back(det);
  sort(_sortedDetectors.begin(), _sortedDetectors.end(), RPhiLessThan());
}


//Non members

//sort in descending order in radius, and ascending order in phi
bool RPhiLessThan::operator()(const StiDetector* lhs, const StiDetector* rhs)
{
  StiPlacement* lhsp = lhs->getPlacement();
  StiPlacement* rhsp = rhs->getPlacement();
  if (!lhsp || !rhsp) 
    {
      cout << "RPhiLessThan::operator() -E- !lhsp || !rhsp "<<endl;
      return false;
    }
  if (lhsp->getLayerRadius()<rhsp->getLayerRadius())
    return false;
  else if (lhsp->getLayerRadius()>rhsp->getLayerRadius())
    return true;
//VP  else
//VP    return (lhsp->getLayerAngle()<rhsp->getLayerAngle());
  double la = lhsp->getLayerAngle();
  double ra = rhsp->getLayerAngle();
  if ((la<0) != (ra<0))  { if (la<0) la+=2*M_PI;if (ra<0) ra+=2*M_PI;}
  return (la<ra);
}


vector<StiDetector*> & StiDetectorContainer::getDetectors()
{
  return _sortedDetectors;
}

vector<StiDetector*> & StiDetectorContainer::getDetectors(Filter<StiDetector> & filter)
{
  _selectedDetectors.clear();
  StiDetector * detector;
  for (vector<StiDetector*>::const_iterator i=_sortedDetectors.begin();
       i!=_sortedDetectors.end();
       ++i)
    {
    detector = *i;
    if (filter.accept(detector))
      _selectedDetectors.push_back(detector);
    }
  return _selectedDetectors;
}

vector<StiDetector*>::iterator  StiDetectorContainer::begin()
{
  cout << "StiDetectorContainer::begin() -I- size:"<<_sortedDetectors.size()<<endl;
  return _sortedDetectors.begin();
}

vector<StiDetector*>::iterator  StiDetectorContainer::end()
{  
  return _sortedDetectors.end();
}

vector<StiDetector*>::const_iterator  StiDetectorContainer::begin() const
{
  cout << "StiDetectorContainer::begin() const -I- size:"<<_sortedDetectors.size()<<endl;
  return _sortedDetectors.begin();
}

vector<StiDetector*>::const_iterator  StiDetectorContainer::end() const
{  
  return _sortedDetectors.end();
}
