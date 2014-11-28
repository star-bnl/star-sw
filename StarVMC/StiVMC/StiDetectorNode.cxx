#include "StiDetectorNode.h"
void StiDetectorNode::Add(StiDetectorNode* newChild) 
{
    if (!newChild) return;
    StiDetectorNode::StiDetectorNodeVector::iterator where = find(mVec.begin(), mVec.end(),
								  newChild);
    //Remove if we see an efficiency penalty
    if (where!=end()) {
	cout <<"StiDetectorNode::Add().  ERROR:\t";
	cout <<"Child Already exists.  Abort"<<endl;
	return;
    }
    //else add to list, set parent
    newChild->SetParent(this);
    mVec.push_back(newChild);
}

/*! This is an internal method.  For more see documentation for the public
  method Add().
 */
void StiDetectorNode::SetParent(StiDetectorNode* val) 
{
    if (mparent) {
	cout <<"StiDetectorNode::SetParent()\tError:\t";
	cout <<"parent already exists"<<endl;
	return;
    }
    mparent=val;
    return;
}


//For now, include some typdefs that will make for easy user includes
//non-members
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StiOrderKey& theKey) {
  return os<<"key: "<<theKey.key<<" index: "<<theKey.index;
}
//________________________________________________________________________________
ostream& operator<<(ostream& os, const StiDetectorNode& node) {
  StiDetectorNode *parent = node.Parent();
  while (parent) {
    os << "\t";
    parent = parent->Parent();
  }
  os << "node: " << node.GetName();
  if (node.Parent()) os << "\tparent: " << node.Parent()->GetName();
  if (node.Data()) os << "\tDetector: " << node.Data()->GetName();
  const StiDetectorNode::vec_type& children = node.getChildren();
  int n = children.size();
  os << "\t" << node.OrderKey() << "\tDaughter: " << n << endl;
  for (int i = 0; i < n; i++) {
    os << *children[i];
  }
  return os;
}
//________________________________________________________________________________
void StiDetectorNode::Print(Option_t *option) const {cout << *this;}
