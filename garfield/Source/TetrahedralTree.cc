/**
TetrahedralTree.cc
This class stores the mesh nodes and elements in an Octree data
structure to optimize the element search operations

Author: Ali Sheharyar
Organization: Texas A&M University at Qatar
*/

#include "TetrahedralTree.hh"
#include <iostream>

namespace Garfield {

TetrahedralTree::TetrahedralTree(const Vec3& origin, const Vec3& halfDimension)
    : m_origin(origin), m_halfDimension(halfDimension) {

  min.x = origin.x - halfDimension.x;
  min.y = origin.y - halfDimension.y;
  min.z = origin.z - halfDimension.z;
  max.x = origin.x + halfDimension.x;
  max.y = origin.y + halfDimension.y;
  max.z = origin.z + halfDimension.z;

  // Initially, there are no children
  for (int i = 0; i < 8; ++i) children[i] = NULL;
}

TetrahedralTree::~TetrahedralTree() {
  // Recursively destroy octants
  for (int i = 0; i < 8; ++i) delete children[i];
}

// Check if a box specified as two min/max points overlap
bool TetrahedralTree::DoesBoxOverlap(const Vec3& b_min,
                                     const Vec3& b_max) const {
  if (max.x < b_min.x) return false;  // this box is left of the input box
  if (max.y < b_min.y) return false;  // this box is below the input box
  if (max.z < b_min.z) return false;  // this box is behind the input box
  if (min.x > b_max.x) return false;  // this box is right of the input
  if (min.y > b_max.y) return false;  // this box is above the input
  if (min.z > b_max.z) return false;  // this in front of the input

  return true;
}

// Determine which octant of the tree would contain 'point'
int TetrahedralTree::GetOctantContainingPoint(const Vec3& point) const {
  int oct = 0;
  if (point.x >= m_origin.x) oct |= 4;
  if (point.y >= m_origin.y) oct |= 2;
  if (point.z >= m_origin.z) oct |= 1;
  return oct;
}

bool TetrahedralTree::IsFull() const {
  // the block size cannot be more the the block capacity
  return iBlockElems.size() == BLOCK_CAPACITY;
}

bool TetrahedralTree::IsEmpty() const { return iBlockElems.size() == 0; }

bool TetrahedralTree::IsLeafNode() const {
  // We are a leaf iff we have no children. Since we either have none, or
  // all eight, it is sufficient to just check the first.
  return children[0] == NULL;  // || (!block && block->isFull());
}

void TetrahedralTree::InsertMeshNode(Vec3 point, int nodeIndex) {
  // check if it is a leaf node
  if (IsLeafNode()) {
    // add the new point if the block is not full
    if (!this->IsFull()) {
      iBlockElems.push_back(OctreeBlockElem(point, nodeIndex));
    } else {
      // block is full, so we need to partition it.
      // Split the current node and create new empty trees for each
      // child octant.
      for (int i = 0; i < 8; ++i) {
        // Compute new bounding box for this child
        Vec3 newOrigin = m_origin;
        newOrigin.x += m_halfDimension.x * (i & 4 ? .5f : -.5f);
        newOrigin.y += m_halfDimension.y * (i & 2 ? .5f : -.5f);
        newOrigin.z += m_halfDimension.z * (i & 1 ? .5f : -.5f);
        children[i] = new TetrahedralTree(newOrigin, m_halfDimension * .5f);
      }

      // move the nodes from the partitioned node (now marked as interior) to
      // its children
      while (!this->IsEmpty()) {
        OctreeBlockElem bElem = iBlockElems.back();
        iBlockElems.pop_back();
        int octant = GetOctantContainingPoint(bElem.point);
        children[octant]->InsertMeshNode(bElem.point, bElem.nodeIndex);
      }

      // insert the new node in the appropriate octant
      children[GetOctantContainingPoint(point)]
          ->InsertMeshNode(point, nodeIndex);
    }
  } else {
    // We are at an interior node. Insert recursively into the
    // appropriate child octant
    int octant = GetOctantContainingPoint(point);
    children[octant]->InsertMeshNode(point, nodeIndex);
  }
}

void TetrahedralTree::InsertTetrahedron(const double elemBoundingBox[6],
                                        const int elemIndex) {

  if (IsLeafNode()) {
    // add the element to the list of this octant
    tetList.push_back(elemIndex);
  } else {
    // check which child overlaps with the element's bounding box
    for (int i = 0; i < 8; i++) {
      Vec3 elem_min(elemBoundingBox[0], elemBoundingBox[1], elemBoundingBox[2]);
      Vec3 elem_max(elemBoundingBox[3], elemBoundingBox[4], elemBoundingBox[5]);

      if (children[i]->DoesBoxOverlap(elem_min, elem_max))
        children[i]->InsertTetrahedron(elemBoundingBox, elemIndex);
    }
  }
}

// It returns the list of tetrahedrons that intersects in a bounding box (Octree
// block) that contains the
// point passed as input.
std::vector<int> TetrahedralTree::GetTetListInBlock(const Vec3& point) {

  const TetrahedralTree* octreeNode = GetBlockFromPoint(point);

  if (octreeNode) {
    return octreeNode->tetList;
  }

  return std::vector<int>();
}

// check if the point is inside the domain.
// This function is only executed at root to ensure that input point is inside
// the mesh's bounding box
// If we don't check this, the case when root is leaf node itself will return
// wrong block
const TetrahedralTree* TetrahedralTree::GetBlockFromPoint(const Vec3& point) {
  if (!(m_origin.x - m_halfDimension.x <= point.x &&
        point.x <= m_origin.x + m_halfDimension.x &&
        m_origin.y - m_halfDimension.y <= point.y &&
        point.y <= m_origin.y + m_halfDimension.y &&
        m_origin.z - m_halfDimension.z <= point.z &&
        point.z <= m_origin.z + m_halfDimension.z))
    return NULL;

  return GetBlockFromPointHelper(point);
}

const TetrahedralTree* TetrahedralTree::GetBlockFromPointHelper(
    const Vec3& point) {
  // If we're at a leaf node, it means, the point is inside this block
  if (IsLeafNode()) return this;
  // We are at the interior node, so check which child octant contains the
  // point
  int octant = GetOctantContainingPoint(point);
  return children[octant]->GetBlockFromPointHelper(point);
}
}
