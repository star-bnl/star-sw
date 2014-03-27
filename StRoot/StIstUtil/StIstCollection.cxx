/***************************************************************************
*
* $Id: StIstCollection.cxx,v 1.6 2014/03/27 22:46:47 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* See header file.
****************************************************************************/

#include "StIstConsts.h"
#include "StIstCollection.h"

//constructor
StIstCollection::StIstCollection() : StObject()
{
   // set the ladder field for some of the collections
   for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
      mRawHitCollection[i].setLadder( i );
      mClusterCollection[i].setLadder( i );
   }

   mNumTimeBins = kIstNumTimeBins; //reasonable default
};

//deconstructor
StIstCollection::~StIstCollection()
{
   Clear("");
};

size_t StIstCollection::getNumTimeBins() const
{
   return mNumTimeBins;
};

void StIstCollection::setNumTimeBins(size_t nTimeBins)
{
   mNumTimeBins = nTimeBins;
};

StIstRawHitCollection *StIstCollection::getRawHitCollection( unsigned char ladder )
{
   return (ladder < kIstNumLadders ? &mRawHitCollection[ladder] : 0 );
};

const StIstRawHitCollection *StIstCollection::getRawHitCollection( unsigned char ladder ) const
{
   return (ladder < kIstNumLadders ? &mRawHitCollection[ladder] : 0  );
};

StIstClusterCollection *StIstCollection::getClusterCollection( unsigned char ladder )
{
   return (ladder < kIstNumLadders ? &mClusterCollection[ladder] : 0 );
};

const StIstClusterCollection *StIstCollection::getClusterCollection( unsigned char ladder ) const
{
   return (ladder < kIstNumLadders ? &mClusterCollection[ladder] : 0 );
};

//sum of all the raw hits over all ladders
size_t StIstCollection::getNumRawHits() const
{
   size_t n = 0;

   for ( const StIstRawHitCollection *ptr = &mRawHitCollection[0]; ptr != &mRawHitCollection[kIstNumLadders]; ++ptr )
      n += ptr->getNumRawHits();

   return n;
};

//number of raw hits on one ladder
size_t StIstCollection::getNumRawHits( unsigned char ladder ) const
{
   return (ladder < kIstNumLadders ? mRawHitCollection[ladder].getNumRawHits() : 0 );
};

//sum of all the clusters over all ladders
size_t StIstCollection::getNumClusters() const
{
   size_t n = 0;

   for ( const StIstClusterCollection *ptr = &mClusterCollection[0]; ptr != &mClusterCollection[kIstNumLadders]; ++ptr )
      n += ptr->getNumClusters();

   return n;
};

//number of clusters on one ladder
size_t StIstCollection::getNumClusters( unsigned char ladder ) const
{
   return (ladder < kIstNumLadders ? mClusterCollection[ladder].getNumClusters() : 0 );
};

//Clear
void StIstCollection::Clear( Option_t *opt )
{
   for ( unsigned char i = 0; i < kIstNumLadders; ++i ) {
      mRawHitCollection[i].Clear( opt );
      mClusterCollection[i].Clear( opt );
   }
};

ClassImp(StIstCollection);


/***************************************************************************
*
* $Log: StIstCollection.cxx,v $
* Revision 1.6  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.5  2014/02/14 14:37:57  ypwang
* remove StMuDstMaker and getNumLadders() member function from StIstCollection
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstCollection.cxx,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/
