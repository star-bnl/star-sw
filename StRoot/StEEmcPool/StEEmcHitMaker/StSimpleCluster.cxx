/*!
 *
 * \class StSimpleCluster_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See header file.
 */

//#include <iostream>
#include "StSimpleCluster.h"

StSimpleCluster_t& StSimpleCluster_t::operator+=( const StSimpleCluster_t& other ){
//    std::cout << "Merging" << std::endl;
//    std::cout << "\t" << *this << std::endl;
//    std::cout << "\t" << other << std::endl;
//    std::cout << "Old seeds " << mSeedIdx << ' ' << other.mSeedIdx << " old sizes " << mMember.GetSize() << ' ' << other.mMember.GetSize() << std::endl;

   if( other.mE > mE ){
      mID = other.mID;
      mSeedIdx = other.mSeedIdx + mMember.GetSize();
   };
   mMeanX = other.mE*other.mMeanX + mE*mMeanX;
   mMeanY = other.mE*other.mMeanY + mE*mMeanY;

   mE += other.mE;
   if( mE ){
      mMeanX /= mE;
      mMeanY /= mE;
   };
   Int_t oldSize = mMember.GetSize();
   Int_t newSize = oldSize + other.mMember.GetSize();
   mMember.Set( newSize );
   for( Int_t i = oldSize, j = 0; i < newSize; ++i, ++j )
      mMember[i] = other.mMember[j];

   oldSize = mWeight.GetSize();
   newSize = oldSize + other.mWeight.GetSize();
   mWeight.Set( newSize );
   for( Int_t i = oldSize, j = 0; i < newSize; ++i, ++j )
      mWeight[i] = other.mWeight[j];

//    std::cout << "Merged: " << *this << std::endl;

   return *this;
};

std::ostream &operator<<( std::ostream &out, const StSimpleCluster_t &clus ){
   out << "clus " << clus.mID << ", E = " << clus.mE << ", nMember = " << clus.mMember.GetSize() << ", " << clus.mWeight.GetSize();
   out << ", seed at " << clus.mSeedIdx << ", with idx " << clus.getSeedMember() << " X = " << clus.mMeanX;
   return out;
};

ClassImp(StSimpleCluster_t);

/*
 * $Id: StSimpleCluster.cxx,v 1.1 2012/11/26 19:05:56 sgliske Exp $ 
 * $Log: StSimpleCluster.cxx,v $
 * Revision 1.1  2012/11/26 19:05:56  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
