/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2009 Matthias Kretz <kretz@kde.org>
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef CLUSTERDATA_H
#define CLUSTERDATA_H

#include "AliHLTTPCCAGBHit.h"
#include <vector>
#include "AliHLTArray.h"

#include "AliHLTTPCCAParameters.h"

class AliHLTTPCSpacePointData;

/**
 * Cluster data which keeps history about changes
 *
 * The algorithm doesn't work on this data. Instead the AliHLTTPCCASliceData is created from this.
 */
class AliHLTTPCCAClusterData
{
  public:

    /**
     * Construct AliHLTTPCCAClusterData object from GBHit array.
     */
    AliHLTTPCCAClusterData( const AliHLTTPCCAGBHit *hits, int *offset, int numberOfClusters, int nRows8 ):
      fSlice(0), fFirstRow(0), fLastRow(0), fNumberOfClusters(0), fRowOffset(0), fData(0)
    {
      readEvent( hits, offset, numberOfClusters, nRows8 );
    }

    AliHLTTPCCAClusterData(): fSlice(0), fFirstRow(0), fLastRow(0), fNumberOfClusters(0), fRowOffset(0), fData(0) {}

    // void readEvent( const AliHLTArray<AliHLTTPCSpacePointData *> &clusters,
    //     int numberOfClusters, double ClusterZCut );
    void readEvent( const AliHLTTPCCAGBHit *hits, int *offset, int numberOfClusters, int nRows8 );

    /**
     * "remove" two clusters and "add" a new one, keeping history.
     */
    void Merge( int index1, int index2 );

    /**
     * "remove" one cluster and "add" two new ones, keeping history.
     */
    //void Split( int index, /* TODO: need some parameters how to split */ );

    // TODO: some access to history of merges and splits

    /**
     * The slice index this data belongs to
     */
    int Slice() const { return fSlice; }

    /**
     * The first row index that contains a cluster.
     */
    int FirstRow() const { return fFirstRow; }

    /**
     * The last row index that contains a cluster.
     */
    int LastRow() const { return fLastRow; }

    /**
     * Return the number of clusters in this slice.
     */
    int NumberOfClusters() const { return fData.size(); }

    /**
     * Return the number of clusters in the given row, for this slice.
     */
    int NumberOfClusters( unsigned int rowIndex ) const { return rowIndex < fNumberOfClusters.size() ? fNumberOfClusters[rowIndex] : 0; }

    /**
     * Return the index of the first cluster in the given row.
     *
     * Supports calls with rowIndex greater than the available number of rows. In that case it
     * returns NumberOfClusters.
     *
     * To iterate over the clusters in one row do:
     * \code
     * AliHLTTPCCAClusterData cd;
     * const int lastClusterIndex = cd.RowOffset( rowIndex + 1 );
     * for ( int hitIndex = cd.RowOffset( rowIndex ); hitIndex < lastClusterIndex; ++hitIndex )
     * \endcode
     */
    int RowOffset( unsigned int rowIndex ) const { return rowIndex < fRowOffset.size() ? fRowOffset[rowIndex] : fData.size(); }

    /**
     * Return the x coordinate of the given cluster.
     */
    float X( int index ) const { assert( index < static_cast<int>( fData.size() ) ); return fData[index].fX; }

    /**
     * Return the y coordinate of the given cluster.
     */
    float Y( int index ) const { assert( index < static_cast<int>( fData.size() ) ); return fData[index].fY; }

    /**
     * Return the z coordinate of the given cluster.
     */
    float Z( int index ) const { assert( index < static_cast<int>( fData.size() ) ); return fData[index].fZ; }

    /**
     * Return the global ID of the given cluster.
     */
    int Id( int index ) const { assert( index < static_cast<int>( fData.size() ) ); return fData[index].fId; }

    /**
     * Return the row number/index of the given cluster.
     */
    int RowNumber( int index ) const { assert( index < static_cast<int>( fData.size() ) ); return fData[index].fRow; }

  private:
    struct Data {
      float fX;
      float fY;
      float fZ;
      int fId;
      int fRow;
    };

    int fSlice;    // the slice index this data belongs to
    int fFirstRow; // see FirstRow()
    int fLastRow;  // see LastRow()
    std::vector<int> fNumberOfClusters; // list of NumberOfClusters per row for NumberOfClusters(int)
    std::vector<int> fRowOffset;        // see RowOffset()
    std::vector<Data> fData; // list of data of clusters
};

typedef AliHLTTPCCAClusterData ClusterData;

#endif // CLUSTERDATA_H
