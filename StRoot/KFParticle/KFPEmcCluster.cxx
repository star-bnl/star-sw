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

#include "KFPEmcCluster.h"
#include <iostream>

void KFPEmcCluster::SetParameter(const float_v& value, int iP, int iTr)
{ 
  /** Copies the SIMD vector "value" to the parameter vector KFPEmcCluster::fP[iP]
   ** starting at the position "iTr".
   ** \param[in] value - SIMD vector with the values to be stored
   ** \param[in] iP - number of the parameter vector
   ** \param[in] iTr - starting position in the parameter vector where the values should be stored
   **/
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fP[iP][iTr]) = value;
  else
  {
    const uint_v index(uint_v::IndexesFromZero());
    (reinterpret_cast<float_v&>(fP[iP][iTr])).gather(reinterpret_cast<const float*>(&value), index, simd_cast<float_m>(index<(Size() - iTr)));
  }
}
void KFPEmcCluster::SetCovariance(const float_v& value, int iC, int iTr) 
{ 
  /** Copies the SIMD vector "value" to the element of the covariance matrix vector KFPEmcCluster::fC[iC]
   ** starting at the position "iTr".
   ** \param[in] value - SIMD vector with the values to be stored
   ** \param[in] iC - number of the element of the covariance matrix
   ** \param[in] iTr - starting position in the parameter vector where the values should be stored
   **/
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fC[iC][iTr]) = value;
  else
  {
    const uint_v index(uint_v::IndexesFromZero());
    (reinterpret_cast<float_v&>(fC[iC][iTr])).gather(reinterpret_cast<const float*>(&value), index, simd_cast<float_m>(index<(Size() - iTr)));
  }
}

void KFPEmcCluster::Resize(const int n)
{
  /** Resizes all vectors in the class to a given value.
   ** \param[in] n - new size of the vector
   **/
  for(int i=0; i<4; i++)
    fP[i].resize(n);
  for(int i=0; i<10; i++)
    fC[i].resize(n);
  fId.resize(n);
}

void KFPEmcCluster::Set(KFPEmcCluster& v, int vSize, int offset)
{
  /** Copies "vSize" clusters from the KFPEmcCluster "v" to the current object.
   ** Tracks are put starting from the "offset" position.
   ** \param[in] v - external KFPEmcCluster with input clusters to be copied
   ** \param[in] vSize - number of clusters to be copied from "v"
   ** \param[in] offset - offset position in the current object, starting from which input clusters will be stored
   **/
  for(int iV=0; iV<vSize; iV++)
  {
    for(int i=0; i<4; i++)
      fP[i][offset+iV] = v.fP[i][iV];
    for(int i=0; i<10; i++)
      fC[i][offset+iV] = v.fC[i][iV];
    fId[offset+iV] = v.fId[iV];
  }
}

void KFPEmcCluster::SetTracks(const KFPEmcCluster& track, const kfvector_uint& trackIndex, const int nIndexes)
{
  /** The current object is resised to "nIndexes", clusters with indices "trackIndex" are copied to the current object.
   ** \param[in] track - input vector of clusters
   ** \param[in] trackIndex - indices of clusters in a vector "track", which should be stored to the current object
   ** \param[in] nIndexes - number of clusters to be copied, defines the new size of the current object
   **/

  if(nIndexes == 0) return;
  
  Resize(nIndexes);

  for(int iP=0; iP<4; iP++)
  {
    int iElement = 0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
      vec.gather(&(track.fP[iP][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
    vec.gather(&(track.fP[iP][0]), index, simd_cast<float_m>(iElement+uint_v::IndexesFromZero()<nIndexes));
    
  }
  for(int iC=0; iC<10; iC++)
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
      vec.gather(&(track.fC[iC][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
    vec.gather(&(track.fC[iC][0]), index, simd_cast<float_m>(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
      vec.gather(&(track.fId[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
    vec.gather(&(track.fId[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
}

void KFPEmcCluster::PrintTrack(int n)
{
  /** Prints parameters of the cluster with index "n".
   ** \param[in] n - index of cluster to be printed
   **/
  for(int i=0; i<4; i++)
    std::cout << fP[i][n] << " ";
  std::cout << std::endl;
  for(int i=0; i<10; i++)
    std::cout << fC[i][n] << " ";
  std::cout << std::endl;
  
  std::cout << fId[n] << std::endl;
}

void KFPEmcCluster::PrintTracks()
{
  /** Prints all field of the current object. **/
    
  std::cout << "NTracks " << Size() << std::endl;
  if( Size()==0 ) return;
  
  std::cout << "Parameters: " << std::endl;
  for(int iP=0; iP<4; iP++)
  {
    std::cout << "  iP " << iP << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Parameter(iP)[iTr]<< " ";
    std::cout << std::endl;
  }

  std::cout << "Cov matrix: " << std::endl;
  for(int iC=0; iC<10; iC++)
  {
    std::cout << "  iC " << iC << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Covariance(iC)[iTr]<< " ";
    std::cout << std::endl;
  }
  
  std::cout << "Id: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  Id()[iTr] << " ";
  std::cout << std::endl;
}
  
