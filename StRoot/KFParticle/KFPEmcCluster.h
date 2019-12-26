/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef KFPEmcCluster_H
#define KFPEmcCluster_H

#include "KFParticleDef.h"

/** @class KFPEmcCluster
 ** @brief A class to store vectors of input cluster from the electro-magnetic calorimeter.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** A cluster is described with the state vector { X, Y, Z, E }
 ** and the corresponding covariance matrix. Also contains a unique id.
 ** The data model implemented in the class is "Structure Of Arrays":
 ** each parameter is stroed in a separate vector. Such data structure
 ** allows fast vectorised access to the aligned data providing the
 ** maximum possible speed for data reading, and at the same time easy
 ** random access to the data members.
 **/

class KFPEmcCluster
{
 public:
  KFPEmcCluster():fP(), fC(), fId() { }
  virtual ~KFPEmcCluster() { }

  /**Returns size of the vectors. All data vectors have the same size. */
  int Size() const { return fP[0].size(); }
  
  void Resize(const int n);
  void Set(KFPEmcCluster& v, int vSize, int offset);
  void SetTracks(const KFPEmcCluster& track, const kfvector_uint& trackIndex, const int nIndexes);
  
  const kfvector_float& X()  const { return fP[0]; } ///< Returns constant reference to the vector with X coordinates.
  const kfvector_float& Y()  const { return fP[1]; } ///< Returns constant reference to the vector with Y coordinates.
  const kfvector_float& Z()  const { return fP[2]; } ///< Returns constant reference to the vector with Z coordinates.
  const kfvector_float& E() const { return fP[3]; }  ///< Returns constant reference to the vector with energy of the cluster.

  const kfvector_float& Parameter(const int i)  const { return fP[i]; }  ///< Returns constant reference to the parameter vector with index "i".
  const kfvector_float& Covariance(const int i)  const { return fC[i]; } ///< Returns constant reference to the vector of the covariance matrix elements with index "i".
  const kfvector_int& Id()    const { return fId; }                      ///< Returns constant reference to the vector with unique id of the clusters.

  //modifiers 
  void SetParameter (float value, int iP, int iTr) { fP[iP][iTr] = value; } ///< Sets the "value" of the parameter "iP" of the cluster with index "iTr".
  void SetCovariance(float value, int iC, int iTr) { fC[iC][iTr] = value; } ///< Sets the "value" of the element of covariance matrix "iC" of the cluster with index "iTr".

  void SetParameter (const float_v& value, int iP, int iTr);  
  void SetCovariance(const float_v& value, int iC, int iTr);

  void SetId        (int value, int iTr)           { fId[iTr] = value; } ///< Sets the "value" of the id of the cluster with index "iTr".
  
  void PrintTrack(int n);
  void PrintTracks();
  
  KFPEmcCluster(const KFPEmcCluster& clusters): fId()
  {
    /** Copy-constructor. Makes one-to-one copy.*/
    const int localSize = clusters.Size();
    
    for(int i=0; i<4; i++)
    {
      fP[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fP[i][n] = clusters.fP[i][n];
    }

    for(int i=0; i<10; i++)
    {
      fC[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fC[i][n] = clusters.fC[i][n];
    }
    
    fId.resize(localSize);
    for(int n=0; n<localSize; n++)
      fId[n] = clusters.fId[n];    
  }
  
  const KFPEmcCluster& operator = (const KFPEmcCluster& clusters)
  {
    /** Operator to copy one KFPEmcCluster object to another. Makes one-to-one copy.*/
    const int localSize = clusters.Size();
    
    for(int i=0; i<4; i++)
    {
      fP[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fP[i][n] = clusters.fP[i][n];
    }

    for(int i=0; i<10; i++)
    {
      fC[i].resize(localSize);
      for(int n=0; n<localSize; n++)
        fC[i][n] = clusters.fC[i][n];
    }
    
    fId.resize(localSize);
    for(int n=0; n<localSize; n++)
      fId[n] = clusters.fId[n];
    
    return *this;
  }
  
 private:  
  kfvector_float fP[4];  ///< Coordinates of the cluster and energy: X, Y, Z, E.
  kfvector_float fC[10]; ///< Covariance matrix of the parameters of the cluster.

  kfvector_int fId; ///< Vector with unique ids of the clusters.
};

#endif
