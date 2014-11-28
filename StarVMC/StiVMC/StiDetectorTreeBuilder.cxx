//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

#include "Stiostream.h"
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include "StiDetector.h"
#include "StiDetectorNode.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "TMath.h"
ostream& operator<<(ostream&, const StiDetector&);
StiDetectorNode* StiDetectorTreeBuilder::Build() {
  cout <<"StiDetectorTreeBuilder::Build() - Started"<<endl;
  if (mRoot) {
    cout << "StiDetectorTreeBuilder::Build()\tError!\tRoot tree already built"<<endl;
    assert(! mRoot);
  }
  if (!mnodefactory) {
    cout << "StiDetectorTreeBuilder::Build() - ERROR - no Factory<StiDetectorNode> provided" << endl;
    assert(mnodefactory);
  }		
  BuildRoot();
  return mRoot;
}
//________________________________________________________________________________
void StiDetectorTreeBuilder::SortTree() {
  cout <<"StiDetectorTreeBuilder::SortTree() -I- Sort Tree"<<endl;
  //Now sort the tree:
  SortDaughters mysorter;
  //mysorter(mregion); (old)
  mysorter(mRoot); //new (MLM)
  //Now index the tree to give efficient sibling traversal
  cout <<"StiDetectorTreeBuilder::SortTree() -I- Index Tree"<<endl;
  IndexDaughters myindexer;
  myindexer(mRoot);
  //myindexer(mregion);
  cout <<"StiDetectorTreeBuilder::SortTree() -I- Done"<<endl;
}
//________________________________________________________________________________
void StiDetectorTreeBuilder::BuildRoot() {
  mRoot = mnodefactory->getInstance();
  mRoot->SetName("star");
  //make 3 daughters
  StiDetectorNode* mid = mnodefactory->getInstance();
  StiDetectorNode* fwd = mnodefactory->getInstance();
  StiDetectorNode* bwd = mnodefactory->getInstance();
  
  mid->SetName("midrapidity");
  fwd->SetName("forwardrapidity");
  bwd->SetName("backwardrapidity");
  
  StiOrderKey midKey;
  midKey.key = static_cast<double>(StiDetector::kMidRapidity);
  StiOrderKey fwdKey;
  fwdKey.key = static_cast<double>(StiDetector::kForwardRapidity);
  StiOrderKey bwdKey;
  bwdKey.key = static_cast<double>(StiDetector::kBackwardRapidity);
  
  mid->SetOrderKey(midKey);
  fwd->SetOrderKey(fwdKey);
  bwd->SetOrderKey(bwdKey);
  
  mRoot->Add(mid);
  mRoot->Add(fwd);
  mRoot->Add(bwd);
  
  mregion = mid;
}
//________________________________________________________________________________
void StiDetectorTreeBuilder::AddToTree(StiDetector* layer) {
  //cout << "StiDetectorTreeBuilder::AddToTree(StiDetector*) -I- Started"<<endl;
  //Which region do we hang it on?
  SameOrderKey mySameOrderKey;
  StiOrderKey tempOrderKey;
  tempOrderKey.key = StiDetector::kMidRapidity; // static_cast<double>( layer->getRegion() );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StiOrderKey&
  StiDetectorNode::StiDetectorNodeVector::iterator where = find_if(mRoot->begin(), mRoot->end(), mySameOrderKey); 
  
  if (where==mRoot->end()) {
    //must abort!!! If this happens do not go on!!!
    cout <<"StiDetectorContainer::Build() - ERROR - mid-rapidity region not found - where==0"<<endl;
    abort();
  }
  
  //ok, now we have the region
  mregion = (*where);
  StiOrderKey radius;
  TString radstring;
  
  radius.key = layer->Key(1);
  radstring = "_R";
  StiDetectorNode* radialnode = HangWhere(mregion, radius, radstring);
  
  //Where do we hang in phi?
  StiOrderKey refAngle;
  refAngle.key = layer->Key(2);
  TString phistring = "_P";
  StiDetectorNode* phinode = HangWhere(radialnode, refAngle, phistring);
  
  if (!phinode) cout << "StiDetectorTreeBuilder::AddToTree() -E- phinode==0" << endl; 
  //Maintain the relationship between these two.
  //It's not so elegant to have the Detector know about the node that it's stored on, but
  //it's fast way to get from the detector to the node, and it allows the project to be
  //generally independent of the tree-node, ie, the tracker only has to know about
  //StiDetector objects.
  //So, we put the extra layer of coupling in StiDetector, not Tracke, SeedFinder, etc...
  phinode->SetData(layer);
  layer->SetTreeNode(phinode);
}
//________________________________________________________________________________
// Starting with the given parent, use the ordering key of the given type
// to determine where the new detector should be hung.
StiDetectorNode* StiDetectorTreeBuilder::HangWhere(StiDetectorNode* parent, const StiOrderKey& order,
						   TString& keystring) {
  SameOrderKey mySameOrderKey;
  mySameOrderKey.morderKey = order; //order is of type const StiOrderKey&
  
  StiDetectorNode::StiDetectorNodeVector::iterator where = find_if(parent->begin(), parent->end(), mySameOrderKey);
  
  if (where == parent->end()) {
    //cout <<"HangWhere().  Start new node"<<endl;
    StiDetectorNode* temp = mnodefactory->getInstance();
    keystring += Form("_%i", (Int_t) order.key);
    TString newname = parent->GetName();
    newname += keystring;
    
    temp->SetName(newname);
    temp->SetOrderKey(order);
    parent->Add(temp);
    return temp;
  }
  else {
    return (*where);
  }
}
