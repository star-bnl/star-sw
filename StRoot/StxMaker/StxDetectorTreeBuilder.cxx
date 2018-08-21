//StxDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

#include "Stiostream.h"
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include "StxDetector.h"
#include "StxDetectorNode.h"
#include "StxDetectorTreeBuilder.h"
#include "StlUtilities.h"
#include "TMath.h"
ostream& operator<<(ostream&, const StxDetector&);
StxDetectorNode* StxDetectorTreeBuilder::Build() {
  cout <<"StxDetectorTreeBuilder::Build() - Started"<<endl;
  if (mRoot) {
    cout << "StxDetectorTreeBuilder::Build()\tError!\tRoot tree already built"<<endl;
    assert(! mRoot);
  }
  if (!mnodefactory) {
    cout << "StxDetectorTreeBuilder::Build() - ERROR - no Factory<StxDetectorNode> provided" << endl;
    assert(mnodefactory);
  }		
  BuildRoot();
  return mRoot;
}
//________________________________________________________________________________
void StxDetectorTreeBuilder::SortTree() {
  cout <<"StxDetectorTreeBuilder::SortTree() -I- Sort Tree"<<endl;
  //Now sort the tree:
  SortDaughters mysorter;
  //mysorter(mregion); (old)
  mysorter(mRoot); //new (MLM)
  //Now index the tree to give efficient sibling traversal
  cout <<"StxDetectorTreeBuilder::SortTree() -I- Index Tree"<<endl;
  IndexDaughters myindexer;
  myindexer(mRoot);
  //myindexer(mregion);
  cout <<"StxDetectorTreeBuilder::SortTree() -I- Done"<<endl;
}
//________________________________________________________________________________
void StxDetectorTreeBuilder::BuildRoot() {
  mRoot = mnodefactory->getInstance();
  mRoot->SetName("star");
  //make 3 daughters
  StxDetectorNode* mid = mnodefactory->getInstance();
  StxDetectorNode* fwd = mnodefactory->getInstance();
  StxDetectorNode* bwd = mnodefactory->getInstance();
  
  mid->SetName("midrapidity");
  fwd->SetName("forwardrapidity");
  bwd->SetName("backwardrapidity");
  
  StxOrderKey midKey;
  midKey.key = static_cast<double>(StxDetector::kMidRapidity);
  StxOrderKey fwdKey;
  fwdKey.key = static_cast<double>(StxDetector::kForwardRapidity);
  StxOrderKey bwdKey;
  bwdKey.key = static_cast<double>(StxDetector::kBackwardRapidity);
  
  mid->SetOrderKey(midKey);
  fwd->SetOrderKey(fwdKey);
  bwd->SetOrderKey(bwdKey);
  
  mRoot->Add(mid);
  mRoot->Add(fwd);
  mRoot->Add(bwd);
  
  mregion = mid;
}
//________________________________________________________________________________
void StxDetectorTreeBuilder::AddToTree(StxDetector* layer) {
  //cout << "StxDetectorTreeBuilder::AddToTree(StxDetector*) -I- Started"<<endl;
  //Which region do we hang it on?
  SameOrderKey mySameOrderKey;
  StxOrderKey tempOrderKey;
  tempOrderKey.key = StxDetector::kMidRapidity; // static_cast<double>( layer->getRegion() );
  mySameOrderKey.morderKey = tempOrderKey; //order is of type const StxOrderKey&
  StxDetectorNode::StxDetectorNodeVector::iterator where = find_if(mRoot->begin(), mRoot->end(), mySameOrderKey); 
  
  if (where==mRoot->end()) {
    //must abort!!! If this happens do not go on!!!
    cout <<"StxDetectorContainer::Build() - ERROR - mid-rapidity region not found - where==0"<<endl;
    abort();
  }
  
  //ok, now we have the region
  mregion = (*where);
  StxOrderKey radius;
  TString radstring;
  
  radius.key = layer->Key(1);
  radstring = "_R";
  StxDetectorNode* radialnode = HangWhere(mregion, radius, radstring);
  
  //Where do we hang in phi?
  StxOrderKey refAngle;
  refAngle.key = layer->Key(2);
  TString phistring = "_P";
  StxDetectorNode* phinode = HangWhere(radialnode, refAngle, phistring);
  
  if (!phinode) cout << "StxDetectorTreeBuilder::AddToTree() -E- phinode==0" << endl; 
  //Maintain the relationship between these two.
  //It's not so elegant to have the Detector know about the node that it's stored on, but
  //it's fast way to get from the detector to the node, and it allows the project to be
  //generally independent of the tree-node, ie, the tracker only has to know about
  //StxDetector objects.
  //So, we put the extra layer of coupling in StxDetector, not Tracke, SeedFinder, etc...
  phinode->SetData(layer);
  layer->SetTreeNode(phinode);
}
//________________________________________________________________________________
// Starting with the given parent, use the ordering key of the given type
// to determine where the new detector should be hung.
StxDetectorNode* StxDetectorTreeBuilder::HangWhere(StxDetectorNode* parent, const StxOrderKey& order,
						   TString& keystring) {
  SameOrderKey mySameOrderKey;
  mySameOrderKey.morderKey = order; //order is of type const StxOrderKey&
  
  StxDetectorNode::StxDetectorNodeVector::iterator where = find_if(parent->begin(), parent->end(), mySameOrderKey);
  
  if (where == parent->end()) {
    //cout <<"HangWhere().  Start new node"<<endl;
    StxDetectorNode* temp = mnodefactory->getInstance();
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
