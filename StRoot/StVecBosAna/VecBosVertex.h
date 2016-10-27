#ifndef VecBosVertex_h
#define VecBosVertex_h

#include <vector>

#include "TObject.h"

#include "StThreeVectorF.hh"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "VecBosTrack.h"


class VecBosEvent;


// Info about vertex
class VecBosVertex : public TObject
{
public:

   enum EVertexType {kUNKNOWN=0x0000, kBAD=0x1000, kGOOD=0x0001};

   static const float  sMaxVertexAbsZ;  //!

   VecBosEvent        *mEvent;       //!
   EVertexType         mType;
   Short_t             mId;          // Unique vertex id. Updated when all vertices are set
   Short_t             mIdMuDst;     // as store do muDst list
   float               z;            //! cm
   float               mRank;
   float               mRankLog;
   int                 nEEMCMatch;   // # of matched endcap towers
   TVector3            mPosition;
   VecBosTrackPtrSet   mTracks;      //! these tracks are owned by the event
   VecBosTrackVec      eleTrack;     //!
   vector<StMuTrack*>  prTrList;     //!

   VecBosVertex();
   VecBosVertex(StMuPrimaryVertex &stMuVertex);
   ~VecBosVertex();

   Short_t  GetId() const { return mId; }
   void     SetPosition(const StThreeVectorF &vec);
   bool     IsGood() const { return (mType & kGOOD) == kGOOD ? true : false; }
   void     Process();
   void     clear();
   virtual void Print(const Option_t* opt="") const;

   ClassDef(VecBosVertex, 2);
};


typedef std::vector<VecBosVertex>         VecBosVertexVec;
typedef VecBosVertexVec::iterator         VecBosVertexVecIter;
typedef VecBosVertexVec::const_iterator   VecBosVertexVecConstIter;


inline bool operator==(const VecBosVertex& lhs, const VecBosVertex& rhs) { return (TVector3) lhs.mPosition == (TVector3) lhs.mPosition; }
inline bool operator!=(const VecBosVertex& lhs, const VecBosVertex& rhs) { return !operator==(lhs,rhs); }
inline bool operator< (const VecBosVertex& lhs, const VecBosVertex& rhs) { return lhs.mRank < rhs.mRank; }
inline bool operator> (const VecBosVertex& lhs, const VecBosVertex& rhs) { return  operator< (rhs,lhs); }
inline bool operator<=(const VecBosVertex& lhs, const VecBosVertex& rhs) { return !operator> (lhs,rhs); }
inline bool operator>=(const VecBosVertex& lhs, const VecBosVertex& rhs) { return !operator< (lhs,rhs); }


struct CompareVecBosVertex
{
   bool operator()(const VecBosVertex& lhs, const VecBosVertex& rhs) const { return lhs > rhs; }
};


struct CompareVecBosVertexPtr
{
   bool operator()(const VecBosVertex* lhs, const VecBosVertex* rhs) const { return (*lhs) > (*rhs); }
};


typedef std::set<VecBosVertex, CompareVecBosVertex>     VecBosVertexSet;
typedef VecBosVertexSet::iterator                       VecBosVertexSetIter;
typedef VecBosVertexSet::const_iterator                 VecBosVertexSetConstIter;

typedef std::set<VecBosVertex*, CompareVecBosVertexPtr> VecBosVertexPtrSet;
typedef VecBosVertexPtrSet::iterator                    VecBosVertexPtrSetIter;
typedef VecBosVertexPtrSet::const_iterator              VecBosVertexPtrSetConstIter;

#endif

