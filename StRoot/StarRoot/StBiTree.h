#ifndef STAR_STBITREE
#define STAR_STBITREE

#include <vector>
#include <iterator>
#include <cassert>

namespace std {

template <class PAR>
class StBiTree 
{
   public:
       enum  EBranch {kLeft, kRight, kLeaf};
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
   public:

         ////////////////////////////////////////////////////////////////////////////////////////////////////
         /// \fn  StBiTree()
         ///
         /// \brief  Default constructor.  Create an  empty binary tree
         ///
         /// \author Fine
         /// \date   3/12/2010
         ////////////////////////////////////////////////////////////////////////////////////////////////////

         StBiTree() :mParent(), mBranches(kLeaf),mParameters(), mIsLeaf(true)
         {           mBranches[kLeft] =0;           mBranches[kRight]=0;    }

         StBiTree(const std::vector<PAR>   &mParameters, bool isLeaf=true) 
              :mParent(), mBranches(kLeaf),mParameters(mParameters) , mIsLeaf(isLeaf)
         {           mBranches[kLeft] =0;           mBranches[kRight]=0;          }

         StBiTree(StBiTree *parent, const std::vector<PAR> &mParameters, bool isLeaf=false) 
            :mParent(), mBranches(kLeaf), mParameters(mParameters) , mIsLeaf(isLeaf)
         {            
            mBranches[kLeft] =0; mBranches[kRight]=0;
            if (mParent) {Reparent(mParent);}
         }
         StBiTree(const StBiTree &src);
         virtual ~StBiTree();
         StBiTree *Parent() const;
         
         StBiTree *Left() const;
         void SetLeft(StBiTree *node);
         
         StBiTree *Right() const;
         void SetRight(StBiTree *node);
         
         const PAR &Data() const;
         PAR &Data();

         EBranch Where( const PAR &data) const
         {
            return data < Data() ? kLeft : kRight;
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
         iterator<input_iterator_tag, class StBiTree<PAR> > find( const PAR &data);
         void SetBranch(StBiTree *node,EBranch branch);
         const StBiTree *Branch(EBranch branch) const;
         StBiTree *Branch(EBranch branch);
         iterator<input_iterator_tag, class StBiTree<PAR> > begin() 
         {
            return iterator<input_iterator_tag, class StBiTree<PAR> >(this);
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
  if (mParent) Reparent(0);

  for (int i=0;i<mBranches.size();++i) {
    if (mBranches[i]) {
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
const StBiTree<PAR> *StBiTree<PAR>::Branch(StBiTree<PAR>::EBranch branch) const
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
iterator<input_iterator_tag, class StBiTree<PAR> > StBiTree<PAR>::find( const PAR &data)
{
   iterator<input_iterator_tag, class StBiTree<PAR> > it = begin();
   while (it != end()) {
      if (it.IsLeaf() ) {
         *it;
         break;
      } 
   }
}

}
#endif
