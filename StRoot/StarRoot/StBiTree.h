#ifndef STAR_STBITREE
#define STAR_STBITREE

#include <vector>
#include <iterator>
#include <cassert>
#include <math.h>
#include <iostream>
#include <iomanip>

#include "StBiTreeIter.h"

namespace std {

template <class Key>
class StBiTree 
{
   public:
       enum  EBranch {kLeft, kRight, kLeaf, kEmpty};
   private:
       StBiTree  *mParent;
       std::vector<StBiTree  *>mBranches;
       Key            mParameters;
       bool           mIsLeaf;
   protected:
         void Insert(StBiTree *node);
         void Insert(const Key &data,bool leaf=false);
         int  Reparent(StBiTree *newParent);
         void SetParent(StBiTree *node);
         void SetLeaf(bool leaf=true) { mIsLeaf = leaf; }
   public:

         ////////////////////////////////////////////////////////////////////////////////////////////////////
         /// \fn  StBiTree()
         ///
         /// \brief  Default constructor.  Create an  empty binary tree
         ///
         /// \author Fine
         /// \date   3/12/2010
         ////////////////////////////////////////////////////////////////////////////////////////////////////

         StBiTree() :mParent(), mBranches(kLeaf),mParameters(), mIsLeaf(false)
         {           mBranches[kLeft] =0;           mBranches[kRight]=0;    }

         ////////////////////////////////////////////////////////////////////////////////////////////////////
         /// \fn  StBiTree(const std::vector<Key> &mParameters, bool isLeaf=true)
         ///
         /// \brief  Constructor. 
         ///
         /// \param  mParameters Options for controlling the operation. 
         /// \param  isLeaf      true if is leaf. 
         ////////////////////////////////////////////////////////////////////////////////////////////////////

         StBiTree(const Key  &mParameters, bool isLeaf=true) 
              :mParent(), mBranches(kLeaf),mParameters(mParameters) , mIsLeaf(isLeaf)
         {           mBranches[kLeft] =0;           mBranches[kRight]=0;          }

         StBiTree(StBiTree *parent, const Key &mParameters, bool isLeaf=false) 
            :mParent(), mBranches(kLeaf), mParameters(mParameters) , mIsLeaf(isLeaf)
         {            
            mBranches[kLeft] =0; mBranches[kRight]=0;
            if (mParent) {Reparent(mParent);}
         }
         StBiTree(const StBiTree &src);
         virtual ~StBiTree();
         StBiTree *Parent() const;
         void Print(int shift=0) const;
         void PrintData(int shift=0) const;
         
         StBiTree *Left() const;
         StBiTree *Left();
         void SetLeft(StBiTree *node);
         
         StBiTree *Right() const;
         StBiTree *Right();
         void SetRight(StBiTree *node);

         unsigned int  Depth() const; 
         const Key &Data() const;
         Key &Data();

         static int Where( const Key &plane, const Key &data);
         static Key Plane( const Key &left, const Key &right);
         bool IsLeaf() const { return mIsLeaf; }
         bool IsEmpty() const { return !mIsLeaf && Left()==0 && Right() ==0; }
         EBranch Where( const Key &data) const
         {
            return Where(Data(),data) ? kLeft : kRight;
         }
         EBranch WhereAmI() const
         {
            EBranch iAmHere = kLeaf;
            if ( Parent() ) {
              if      ( Parent()->Left()  == this) iAmHere = kLeft;
              else if ( Parent()->Right() == this) iAmHere = kRight;
              else assert(0 && "__FUNCTION__ wrong parent");
            }
            return iAmHere;
         }
         const StBiTree *push_back( const Key &data);
         void SetBranch(StBiTree *node,EBranch branch);
         StBiTree *Branch(EBranch branch) const;
         StBiTree *Branch(EBranch branch);
         iterator<input_iterator_tag, class StBiTree<Key> > begin() 
         {
            return iterator<input_iterator_tag, class StBiTree<Key> >(this);
         }
         iterator<input_iterator_tag, class StBiTree<Key> > *CreateIterator()
         {
            return new iterator<input_iterator_tag, class StBiTree<Key> >(this);
         }

         static iterator<input_iterator_tag, class StBiTree<Key> > end() 
         {
            return iterator<input_iterator_tag, class StBiTree<Key> >();
         }
         bool empty() const   { return IsEmpty(); }
         iterator<input_iterator_tag, class StBiTree<Key> >  find(const Key &data); 
         iterator<input_iterator_tag, class StBiTree<Key> >  find(const StBiTree<Key> &node); 
};

//___________________________________________________________________
template <class Key>
StBiTree<Key>::~StBiTree()
{
//  cout << __FUNCTION__ << " my parent=" << mParent << " me=" << this << endl;
//  Print(-1); PrintData(-1);
  if (mParent) Reparent(0);
//  cout << __FUNCTION__ << " my parent=" << mParent << endl;
  for (unsigned int i=0;i<mBranches.size();++i) {
    if (mBranches[i]) {
  //     cout << __FUNCTION__ << " branch=" << i << " is: " << mBranches[i] <<   endl;
       mBranches[i]->SetParent(0);
       delete mBranches[i]; 
       mBranches[i] = 0;
    }
  }
}

//___________________________________________________________________
template <class Key>
void StBiTree<Key>::Insert(const Key &data, bool leaf)
{
   Insert(new StBiTree(this,data,leaf));
}

//___________________________________________________________________
template <class Key>
void StBiTree<Key>::Insert(StBiTree<Key> *node)
{
   
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Branch(StBiTree<Key>::EBranch branch) const
{
   return mBranches[branch];
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Branch(StBiTree<Key>::EBranch branch)
{
   return mBranches[branch];
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Left() const
{
   return Branch(kLeft);
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Left()
{
   return Branch(kLeft);
}


//___________________________________________________________________
template <class Key>
void StBiTree<Key>::SetBranch(StBiTree<Key> *node,StBiTree<Key>::EBranch branch)
{
   if (branch!= kLeaf) mBranches[branch]=node;
   else Reparent(0);
}

//___________________________________________________________________
template <class Key>
void StBiTree<Key>::SetLeft(StBiTree *left)
{
   SetBranch(left,kLeft);
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Right()
{
   return Branch(kRight);
}

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Right() const
{
   return Branch(kRight);
}

//___________________________________________________________________
template <class Key>
void StBiTree<Key>::SetRight(StBiTree<Key> *right)
{
   SetBranch(right,kRight);
}


//___________________________________________________________________
template <class Key>
int      StBiTree<Key>::Reparent(StBiTree<Key> *newParent)
{
   EBranch branch = kLeft;
   StBiTree *oldParent = Parent(); 
   if (newParent != oldParent) {
      if (oldParent) 
         oldParent->SetBranch(newParent,WhereAmI());
      SetParent(newParent);
      if (newParent) {
         newParent->SetParent(oldParent);
        switch (branch = newParent->Where(Data()))  {
        case kLeft:
           newParent->SetLeft(this);
           break;
        case kRight:
           newParent->SetRight(this);
           break;
        case kLeaf: case kEmpty: assert(0 && " Wrong position"); break;
         }
      }
   }
   return branch;
}

//___________________________________________________________________
template <class Key>
void StBiTree<Key>::SetParent(StBiTree<Key> *newParent) 
{ mParent=newParent;  }

//___________________________________________________________________
template <class Key>
StBiTree<Key> *StBiTree<Key>::Parent() const
{ return mParent;  }

//___________________________________________________________________
template <class Key>
const Key &StBiTree<Key>::Data() const
{
    return mParameters;
}

//_____________________________________________________________________________
template <class Key>
Key &StBiTree<Key>::Data()
{
    return mParameters;
}

//_____________________________________________________________________________
template <class Key>
const StBiTree<Key> *StBiTree<Key>::push_back( const Key &data)
{
   const StBiTree<Key> *leafBranch = 0;
   StBiTree<Key> &branch = *this; 
   if (branch.IsEmpty() ) {
      mParameters = data;
      mIsLeaf=true;
      leafBranch = this;
   } else if ( branch.IsLeaf() ) {
      // Add the new plane and right leaf
      StBiTree<Key> *rightLeaf = new StBiTree<Key>(data,true);
      StBiTree<Key> *parent    = branch.Parent();
      if (parent) {
         StBiTree<Key> *newParentPlane = new StBiTree<Key>(Plane(branch.Data(),data),false);

         if (newParentPlane->Where(data) == kRight) {
             newParentPlane->SetRight(rightLeaf);
             newParentPlane->SetLeft(&branch);
         } else {
             newParentPlane->SetLeft(rightLeaf);
             newParentPlane->SetRight(&branch);
         }
         newParentPlane->SetParent(parent);

         parent->SetBranch(newParentPlane,branch.WhereAmI());
         branch.SetParent(newParentPlane);
         rightLeaf->SetParent(newParentPlane);
      } else {
         StBiTree<Key> *leftLeaf    = new StBiTree<Key>(branch.Data(),true);
         branch.Data() = Plane(branch.Data(),data);
         if ( branch.Where(data) == kRight) {
            branch.SetLeft(leftLeaf);
            branch.SetRight(rightLeaf);
         } else {
            branch.SetLeft(rightLeaf);
            branch.SetRight(leftLeaf);
         }
         rightLeaf->SetParent(&branch);
         leftLeaf->SetParent(&branch);
         branch.SetLeaf(false);
      }
      leafBranch = rightLeaf;
   } else {
      // define the direction
      EBranch loc = Where(data);
      StBiTree<Key> *next = Branch( loc ); 
      if(next) {
         leafBranch = next->push_back(data);
      } else {
         assert(0);
         SetBranch(new StBiTree<Key>(branch.Data(),true),loc);
      }
   }
   return leafBranch;
}

//_____________________________________________________________________________
template <class Key>
iterator<input_iterator_tag, class StBiTree<Key> >  StBiTree<Key>::find(const StBiTree<Key> &node)
{
   iterator<input_iterator_tag, class StBiTree<Key> > it = find(node.Data());
   if ( it == node.begin()) return it;
   return  end();
}

//_____________________________________________________________________________
template <class Key>
iterator<input_iterator_tag, class StBiTree<Key> >  StBiTree<Key>::find(const Key &data)
{
   if (empty() || IsLeaf() ) {
      return begin();
   } else {
      EBranch loc = Where(data);
      StBiTree<Key> *next = Branch( loc ); 
      if(next) {
         return next->find(data);
      } else {
         return begin();
      }
   }
}

//_____________________________________________________________________________
template<>
inline int StBiTree< vector<float> >::Where( const vector<float> &plane, const vector<float>  &data)
{
   unsigned int nDim=data.size();
   assert(plane.size() == nDim+1);
   float dist = plane[nDim];
   for (unsigned int i=0;i<nDim;++i)   dist += plane[i]*data[i];
   return dist > 0;
}

//_____________________________________________________________________________
template<>
inline vector<float> StBiTree< vector<float> >::Plane( const vector<float> &left, const vector<float>  &right)
{
   //  Create plane between two points:
   //  A  = (right - left)/|right - left|
   //  X0 = (right+left)/2
   //  D  = - A*X0
   //  PLANE (A+D)/|right -left

   unsigned int nDim = left.size();
   vector<float> plane(nDim+1);
   unsigned int indx = 0;
   float length = 0;
   plane[nDim] = 0;
   for (;indx<nDim;++indx) {
     plane[indx]  = right[indx]-left[indx];
     plane[nDim] += plane[indx]*(right[indx]+left[indx]);
     length      += plane[indx]*plane[indx];
   }
   for (;indx>nDim+1;++indx) plane[indx] /= sqrt(length);
   plane[nDim] = -0.5*plane[nDim];
   cout << endl;
   cout << __FUNCTION__ << "::Left: " << "x="   << left[0] 
                                      << "; y=" << left[1];
   indx = 2;
   if (nDim >= 3)              cout  << "; z=" << left[2];
                                cout  << endl;

   cout << __FUNCTION__ << "::Right: "<< "x="   << right[0] 
                                      << "; y=" << right[1];
   indx = 2;
   if (nDim >= 3)              cout  << "; z=" << right[2];
                                cout  << endl;

   cout << __FUNCTION__ << "::Plane:" << plane[0] << "*x+" 
                                      << plane[1] << "*y+";
   indx = 2;
   if (nDim >= 3)      cout     << plane[indx++] << "*z+";
                       cout      << plane[indx]   << endl;

   cout << endl;
   return plane;
}
//___________________________________________________________________
template <class Key>
unsigned int  StBiTree<Key>::Depth() const
{
   const StBiTree<Key> *currentNode = Parent();
   unsigned int depth = 0;
   while (currentNode) {
      depth++;
      currentNode = currentNode->Parent();
   }
   return depth;
}
//___________________________________________________________________
template <class Key>
void StBiTree<Key>::Print(int shift) const
{
   cout << __FUNCTION__ << setw(shift) << "  "<< " me=" << this
         << " Parent=" <<Parent()
         << " Leaf=" << IsLeaf()
         << " Left branch =" << Left()
         << " Right branch = " << Right() << endl;
   if (shift >= 0) {
      shift++;
      if (Left()) Left()->Print(2*shift);
      if (Right()) Right()->Print(2*shift);
   }
}

//_____________________________________________________________________________
template<>
inline void StBiTree< vector<float> >::PrintData(int shift) const
{
  unsigned int nDim = mParameters.size();
  const char *nodeType = nDim <=3 ? "point: " : "plane";
  cout <<  setw(shift)<< nodeType;
  for (unsigned int i =0; i<nDim;++i) cout << " :  [" << i << "]=" << mParameters[i] ;
  cout << endl << "    " << " Total: " << nDim << endl;
  if (shift >= 0) {
      shift++;
      if (Left()) Left()->PrintData(2*shift);
      if (Right()) Right()->PrintData(2*shift);
   }
}
}
#endif
