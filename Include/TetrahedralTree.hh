/**

TetrahedralTree.hh

This class stores the mesh nodes and elements in an Octree data

structure to optimize the element search operations



Author: Ali Sheharyar

Organization: Texas A&M University at Qatar

*/

#ifndef TETRAHEDRAL_TREE_H
#define TETRAHEDRAL_TREE_H

#include <vector>

#define BLOCK_CAPACITY 10  // k_v

namespace Garfield {

// TODO: replace this class with ROOT's TVector3 class

struct Vec3 {
  float x, y, z;

  Vec3() {}
  Vec3(float _x, float _y, float _z) : x(_x), y(_y), z(_z) {}

  Vec3 operator+(const Vec3& r) const {
    return Vec3(x + r.x, y + r.y, z + r.z);
  }

  Vec3 operator-(const Vec3& r) const {
    return Vec3(x - r.x, y - r.y, z - r.z);
  }

  Vec3& operator+=(const Vec3& r) {
    x += r.x;
    y += r.y;
    z += r.z;
    return *this;
  }

  Vec3& operator-=(const Vec3& r) {
    x -= r.x;
    y -= r.y;
    z -= r.z;
    return *this;
  }

  Vec3 operator*(float r) const { return Vec3(x * r, y * r, z * r); }

  Vec3 operator/(float r) const { return Vec3(x / r, y / r, z / r); }
};

class TetrahedralTree {

 private:
  // Physical position/size. This implicitly defines the bounding
  // box of this node
  Vec3 m_origin;         // The physical center of this node
  Vec3 m_halfDimension;  // Half the width/height/depth of this node

  Vec3 min, max;  // storing min and max points for convenience

  // The tree has up to eight children and can additionally store
  // a list of mesh nodes and mesh elements (Tetrahedron)
  TetrahedralTree* children[8];  // Pointers to child octants

  // Children follow a predictable pattern to make accesses simple.
  // Here, - means less than 'origin' in that dimension, + means greater than.
  // child:	0 1 2 3 4 5 6 7
  // x:      - - - - + + + +
  // y:      - - + + - - + +
  // z:      - + - + - + - +

  struct OctreeBlockElem {
    Vec3 point;
    int nodeIndex;

    OctreeBlockElem(const Vec3& _point, const int _ni)
        : point(_point), nodeIndex(_ni) {}
  };

  std::vector<OctreeBlockElem> iBlockElems;
  std::vector<int> tetList;

 public:
  // Constructor
  TetrahedralTree(const Vec3& origin, const Vec3& halfDimension);

  // Destructor
  ~TetrahedralTree();

  // Insert a mesh node (a vertex/point) to the tree
  void InsertMeshNode(Vec3 point, const int nodeIndex);

  // Insert the mesh element (a tetrahedron) to the tree
  void InsertTetrahedron(const double elemBoundingBox[6], const int elemIndex);

  // Get all tetrahedra linked to a block corresponding to the given point
  std::vector<int> GetTetListInBlock(const Vec3& point);

 private:
  // Check if the given box overlaps with the box corresponding to this tree
  // node
  bool DoesBoxOverlap(const Vec3& b_min, const Vec3& b_max) const;

  int GetOctantContainingPoint(const Vec3& point) const;

  // Check if the tree node is full
  bool IsFull() const;

  // Check if the tree node is empty
  bool IsEmpty() const;

  // Check if this tree node is a leaf or intermediate node
  bool IsLeafNode() const;

  // Get a block containing the input point
  const TetrahedralTree* GetBlockFromPoint(const Vec3& point);

  // A helper function used by the function above. 
  // Called recursively on the child nodes.
  const TetrahedralTree* GetBlockFromPointHelper(const Vec3& point);
};
}

#endif
