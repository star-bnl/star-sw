#ifndef STAR_STBITREE
#define STAR_STBITREE

#include <vector>
#include <iterator>
#include <cassert>
#include <iostream>
#include <iomanip>

#include "StBiTreeIter.h"

namespace std {

template <class PAR>
class StBiTree 
{
   public:
       enum  EBranch {kLeft, kRight, kLeaf, kEmpty};
   private:
       StBiTree  *mParent;
       std::vector<StBiTree  *>mBranches;
       PAR            mParameters;
       bool           mIsLeaf;
   protected:
         void Insert(StBiTree *node);
         void Insert(const PAR &data,bool leaf=false);
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
         /// \fn  StBiTree(const std::vector<PAR> &mParameters, bool isLeaf=true)
         ///
         /// \brief  Constructor. 
         ///
         /// \param  mParameters Options for controlling the operation. 
         /// \param  isLeaf      true if is leaf. 
         ////////////////////////////////////////////////////////////////////////////////////////////////////

         StBiTree(const PAR  &mParameters, bool isLeaf=true) 
              :mParent(), mBranches(kLeaf),mParameters(mParameters) , mIsLeaf(isLeaf)
         {           mBranches[kLeft] =0;           mBranches[kRight]=0;          }

         StBiTree(StBiTree *parent, const PAR &mParameters, bool isLeaf=false) 
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
         const PAR &Data() const;
         PAR &Data();

         static int Where( const PAR &plane, const PAR &data);
         static PAR Plane( const PAR &left, const PAR &right);
         bool IsLeaf() const { return mIsLeaf; }
         bool IsEmpty() const { return !mIsLeaf && Left()==0 && Right() ==0; }
         EBranch Where( const PAR &data) const
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
         void push_back( const PAR &data);
         void SetBranch(StBiTree *node,EBranch branch);
         StBiTree *Branch(EBranch branch) const;
         StBiTree *Branch(EBranch branch);
         iterator<input_iterator_tag, class StBiTree<PAR> > begin() 
         {
            return iterator<input_iterator_tag, class StBiTree<PAR> >(this);
         }
         iterator<input_iterator_tag, class StBiTree<PAR> > *CreateIterator()
         {
            return new iterator<input_iterator_tag, class StBiTree<PAR> >(this);
         }

         static iterator<input_iterator_tag, class StBiTree<PAR> > end() 
         {
            return iterator<input_iterator_tag, class StBiTree<PAR> >();
         }

};

//___________________________________________________________________
template <class PAR>
StBiTree<PAR>::~StBiTree()
{
  cout << __FUNCTION__ << " my parent=" << mParent << " me=" << this << endl;
  Print(-1); PrintData(-1);
  if (mParent) Reparent(0);
  cout << __FUNCTION__ << " my parent=" << mParent << endl;
  for (unsigned int i=0;i<mBranches.size();++i) {
    if (mBranches[i]) {
       cout << __FUNCTION__ << " branch=" << i << " is: " << mBranches[i] <<   endl;
       mBranches[i]->SetParent(0);
       delete mBranches[i]; 
       mBranches[i] = 0;
    }
  }
}

//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::Insert(const PAR &data, bool leaf)
{
   Insert(new StBiTree(this,data,leaf));
}

//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::Insert(StBiTree<PAR> *node)
{
   
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Branch(StBiTree<PAR>::EBranch branch) const
{
   return mBranches[branch];
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Branch(StBiTree<PAR>::EBranch branch)
{
   return mBranches[branch];
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Left() const
{
   return Branch(kLeft);
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Left()
{
   return Branch(kLeft);
}


//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::SetBranch(StBiTree<PAR> *node,StBiTree<PAR>::EBranch branch)
{
   if (branch!= kLeaf) mBranches[branch]=node;
   else Reparent(0);
}

//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::SetLeft(StBiTree *left)
{
   SetBranch(left,kLeft);
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Right()
{
   return Branch(kRight);
}

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Right() const
{
   return Branch(kRight);
}

//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::SetRight(StBiTree<PAR> *right)
{
   SetBranch(right,kRight);
}


//___________________________________________________________________
template <class PAR>
int      StBiTree<PAR>::Reparent(StBiTree<PAR> *newParent)
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
template <class PAR>
void StBiTree<PAR>::SetParent(StBiTree<PAR> *newParent) 
{ mParent=newParent;  }

//___________________________________________________________________
template <class PAR>
StBiTree<PAR> *StBiTree<PAR>::Parent() const
{ return mParent;  }

//___________________________________________________________________
template <class PAR>
const PAR &StBiTree<PAR>::Data() const
{
    return mParameters;
}

//___________________________________________________________________
template <class PAR>
PAR &StBiTree<PAR>::Data()
{
    return mParameters;
}

//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::push_back( const PAR &data)
{
   iterator<input_iterator_tag, class StBiTree<PAR> > it = begin();
   while (it != end())    {
      StBiTree<PAR> &branch = *it; 
      if (branch.IsEmpty() ) {
         mParameters = data;
         mIsLeaf=true;
         break;
      } else if ( branch.IsLeaf() ) {
         // Add the new plane and right leaf
         StBiTree<PAR> *rightLeaf = new StBiTree<PAR>(data,true);
         StBiTree<PAR> *parent    = branch.Parent();
         if (parent) {
            StBiTree<PAR> *newParentPlane = new StBiTree<PAR>(Plane(branch.Data(),data),false);

            newParentPlane->SetRight(rightLeaf);
            newParentPlane->SetLeft(&branch);
            newParentPlane->SetParent(parent);

            parent->SetBranch(newParentPlane,branch.WhereAmI());
            branch.SetParent(newParentPlane);
            rightLeaf->SetParent(newParentPlane);
         } else {
            StBiTree<PAR> *leftLeaf    = new StBiTree<PAR>(branch.Data(),true);
            branch.SetLeft(leftLeaf);
            leftLeaf->SetParent(&branch);

            branch.SetRight(rightLeaf);
            rightLeaf->SetParent(&branch);

            branch.Data() = Plane(branch.Data(),data);
            branch.SetLeaf(false);
         }
         break;
      } else {
         // define the direction
         it.next ( Where(branch.Data(),data) ); 
      }
   }
}

template<>
inline int StBiTree< vector<float> >::Where( const vector<float> &plane, const vector<float>  &data)
{
   float dist = plane[3];
   for (int i=0;i<3;++i)   dist += plane[i]*data[i];
   return dist > 0;
}

template<>
inline vector<float> StBiTree< vector<float> >::Plane( const vector<float> &left, const vector<float>  &right)
{
   //  Create plane between two points:
   //  A  = (right - left)/|right - left|
   //  X0 = (right+left)/2
   //  D  = - A*X0
   //  PLANE (A+D)/|right -left

   int nDim = left.size();
   vector<float> plane(nDim+1);
   int indx = 0;
   float length = 0;
   for (;indx<nDim;++indx) {
     plane[indx]  = right[indx]-left[indx];
     plane[nDim] += 0.5*plane[indx]*(right[indx]+left[indx]);
     length      += plane[indx]*plane[indx];
   }
   for (;indx>nDim+1;++indx) plane[indx] /= length;
   plane[nDim] /= 2;
   cout << __FUNCTION__ << "::Left: " << "x="   << left[0] 
                                    << "; y=" << left[1]
                                    << "; z=" << left[2]
                                    << endl;

   cout << __FUNCTION__ << "::Right: "<< "x="   << right[0] 
                                    << "; y=" << right[1]
                                    << "; z=" << right[2]
                                    << endl;

   cout << __FUNCTION__ << "::Plane:" << plane[0] << "*x+" 
                                   << plane[1] << "*y+"
                                   << plane[2] << "*z+"
                                   << plane[3] << endl;

   return plane;
}
//___________________________________________________________________
template <class PAR>
unsigned int  StBiTree<PAR>::Depth() const
{
   const StBiTree<PAR> *currentNode = Parent();
   unsigned int depth = 0;
   while (currentNode) {
      depth++;
      currentNode = currentNode->Parent();
   }
   return depth;
}
//___________________________________________________________________
template <class PAR>
void StBiTree<PAR>::Print(int shift) const
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
