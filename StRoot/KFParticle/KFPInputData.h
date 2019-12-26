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

#ifndef KFPINPUTDATA_H
#define KFPINPUTDATA_H

#include "KFPTrackVector.h"
#include "KFParticle.h"

#include <vector>
#include <string>
#include <fstream>

/** @class KFPTrackIndex
 ** @brief Helper structure to sort tracks in the KFPTrackVector object.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The structure is used in the KFParticleTopoReconstructor::SortTracks() function.
 ** Tracks are sorted according to their pdg hypothesis: electrons, muons, pions,
 ** tracks without pdg (-1), kaons, protons, deuterons, tritons, He3, He4.
 ** Teh structure contains pdg hypothesis of the track and its index in the 
 ** KFPTrackVector object.
 **/

struct KFPTrackIndex
{
  int fIndex; ///< index of the track in the KFPTrackVector object.
  int fPdg;   ///< PDG hypothesis of the track
  
  static bool Compare(const KFPTrackIndex& a, const KFPTrackIndex& b)
  {
    /** Static sorting function for comparison of the two input objects of class KFPTrackIndex.
     ** Objects are sorted according to the PDG hypothesis: electrons, muons, pions,
     ** tracks without pdg (-1), kaons, protons, deuterons, tritons, He3, He4.
     ** Return "true" if a.fPdg < b.fPdg, otherwise returns "false". 
     ** \param[in] a - first object
     ** \param[in] b - second object
     **/
    int pdg1 = a.fPdg == -1 ? 250 : a.fPdg;
    int pdg2 = b.fPdg == -1 ? 250 : b.fPdg;

    return (abs(pdg1) < abs(pdg2));
  }
};


/** @class KFPInputData
 ** @brief Class with the input data for KF Particle Finder: tracks, primary vertex and magnetic field.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is used to transfer the data between devices: CPU and Intel Xeon Phi. The memory is aligned
 ** with the size of the SIMD vectors.
 **/

class KFPInputData
{
 public:
   
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }      ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}  ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                         ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                       ///< delete operator for the SIMD-alligned dynamic memory release
  
  KFPInputData():fPV(0),fBz(0.f) {};
  virtual ~KFPInputData() {};

  bool ReadDataFromFile( std::string prefix )
  {
    /** Reads the input data from the input file with the name defined by "prefix".
     ** \param[in] prefix - string with the name of the input file
     **/ 
    std::ifstream ifile(prefix.data());
    if ( !ifile.is_open() ) return 0;
    int nSets;
    ifile >> fBz;
    ifile >> nSets;
    for(int iSet=0; iSet<nSets; iSet++)
    {
      int nTracks = 0;
      ifile >> nTracks;
      fTracks[iSet].Resize(nTracks);
      
      for(int iP=0; iP<6; iP++)
      {
        float value;
        for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        {
          ifile >> value;
          fTracks[iSet].SetParameter(value, iP, iTr);
        }
      }

      for(int iC=0; iC<21; iC++)
      {
        float value;
        for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
        {
          ifile >> value;
          fTracks[iSet].SetCovariance(value, iC, iTr);
        }
      }

      int tmpInt;
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetId(tmpInt, iTr);
      }
      
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetPDG(tmpInt, iTr);
      }

      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetQ(tmpInt, iTr);
      }
      
      for(int iTr=0; iTr<fTracks[iSet].Size(); iTr++)
      {
        ifile >> tmpInt;
        fTracks[iSet].SetPVIndex(tmpInt, iTr);
      }
      
      ifile >> tmpInt;
      fTracks[iSet].SetLastElectron(tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastMuon    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastPion    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastKaon    (tmpInt);
      ifile >> tmpInt;
      fTracks[iSet].SetLastProton  (tmpInt);
    }

    int nPV;
    ifile>>nPV;
    fPV.resize(nPV);
    for(unsigned int iPV=0; iPV < fPV.size(); iPV++)
    {
      for(int iP=0; iP<3; iP++)
        ifile >> fPV[iPV].Parameter(iP);
  
      for(int iC=0; iC<6; iC++)
        ifile >> fPV[iPV].Covariance(iC);
    }
    
    ifile.close();
    return 1;
  }
  
  void SetDataToVector(int* data, int& dataSize)
  {
    /** Stores information to the memory under pointer "data".
     ** \param[out] data - memory, where input information will be stored
     ** \param[out] dataSize - size of the stored memory in "int" (or bloks of 4 bytes, or 32 bits)
     **/
    dataSize = NInputSets + 1 + 1; //sizes of the track vectors and pv vector, and field
    for(int iSet=0; iSet<NInputSets; iSet++)
      dataSize += fTracks[iSet].DataSize();
    dataSize += fPV.size() * 9;
        
    for(int iSet=0; iSet<NInputSets; iSet++)
      data[iSet] = fTracks[iSet].Size();
    data[NInputSets] = fPV.size();
    
    float& field = reinterpret_cast<float&>(data[NInputSets+1]);
    field = fBz;
    
    int offset = NInputSets+2;
        
    for(int iSet=0; iSet<NInputSets; iSet++)
      fTracks[iSet].SetDataToVector(data, offset);
    
    for(int iP=0; iP<3; iP++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        tmpFloat = fPV[iPV].Parameter(iP);
      }
      offset += fPV.size();
    }
    
    for(int iC=0; iC<6; iC++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        tmpFloat = fPV[iPV].Covariance(iC);
      }
      offset += fPV.size();
    }    
  }

  void ReadDataFromVector(int* data)
  {
    /** Reads input data from the given memory.
     ** \param[in] data - pointer to the memory with the input data
     **/
    int offset = NInputSets+2;
    for(int iSet=0; iSet<NInputSets; iSet++)
    {
      fTracks[iSet].Resize(data[iSet]);
      fTracks[iSet].ReadDataFromVector(data, offset);
    }
    
    float& field = reinterpret_cast<float&>(data[NInputSets+1]);
    fBz = field;
    
    fPV.resize(data[NInputSets]);
                
    for(int iP=0; iP<3; iP++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        fPV[iPV].Parameter(iP) = tmpFloat;
      }
      offset += fPV.size();
    }
    
    for(int iC=0; iC<6; iC++)
    {
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      {
        float& tmpFloat = reinterpret_cast<float&>(data[offset + iPV]);
        fPV[iPV].Covariance(iC) = tmpFloat;
      }
      offset += fPV.size();
    }        
  }
  
  void Print()
  {
    /**Prints all fields of the current object.*/
    for(int iSet=0; iSet<NInputSets; iSet++)
      fTracks[iSet].Print();
    std::cout << "N PV: " << fPV.size() << std::endl;
    
    std::cout << "X: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].X() <<" ";
    std::cout << std::endl;
        std::cout << "Y: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].Y() <<" ";
    std::cout << std::endl;
    std::cout << "Z: ";
    for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
      std::cout << fPV[iPV].Z() <<" ";
    std::cout << std::endl;

    std::cout << "Cov matrix: " << std::endl;
    for(int iC=0; iC<6; iC++)
    {
      std::cout << "  iC " << iC << ":  ";
      for(unsigned int iPV=0; iPV<fPV.size(); iPV++)
        std::cout << fPV[iPV].Covariance(iC) <<" ";
      std::cout << std::endl;
    }
    
    std::cout << "Field: " << fBz << std::endl;
  }
  
  KFPTrackVector* GetTracks()  { return fTracks; } ///< Returns pointer to the array with track vectors.
  float GetBz() const { return fBz; } ///< Returns value of the constant field Bz.
  const std::vector<KFParticle>& GetPV() const { return fPV; } ///< Returns vector with primary vertices.

  const KFPInputData& operator = (const KFPInputData& data)
  {
    /** Copies input data from object "data" to the current object. Returns the current object. \param[in] data - input data*/
    for(int i=0; i<NInputSets; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fBz = data.fBz;
    
    return *this;
  }
  KFPInputData(const KFPInputData& data):fPV(0),fBz(0.f)
  {
    /** Copies input data from object "data" to the current object. \param[in] data - input data */
    for(int i=0; i<NInputSets; i++)
      fTracks[i] = data.fTracks[i];
    fPV = data.fPV;
    fBz = data.fBz;
  }
  
 protected:
  /** Array of track vectors: \n
   ** 0 - positive secondary tracks stored at the first point; \n
   ** 1 - negative secondary tracks stored at the first point; \n
   ** 2 - positive primary tracks stored at the first point; \n
   ** 3 - positive primary tracks stored at the first point; \n
   ** 4 - positive secondary tracks stored at the last point; \n
   ** 5 - negative secondary tracks stored at the last point; \n
   ** 6 - positive primary tracks stored at the last point; \n
   ** 7 - positive primary tracks stored at the last point.
   ** \see KFPTrackVector for documentation.
   **/
  KFPTrackVector fTracks[NInputSets]__attribute__((aligned(sizeof(float_v)))); 
  std::vector<KFParticle> fPV; ///< Vector with primary vertices.
  float fBz; ///< Constant homogenious one-component magnetic field Bz.
} __attribute__((aligned(sizeof(float_v))));

/** @class KFPInputDataArray
 ** @brief Structure with the set of the input data for KF Particle Finder.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The structure contains pointer to array of KFPInputData objects. Copying of the 
 ** objects of this structure is disabled.
 **/

struct KFPInputDataArray{
  KFPInputDataArray():fInput(0){};
  virtual ~KFPInputDataArray() { if(fInput) delete [] fInput; }

  KFPInputData *fInput; ///< Pointer to the array of the input data objects.
  
 private:
   const KFPInputDataArray& operator = (const KFPInputDataArray&);
   KFPInputDataArray(const KFPInputDataArray&);
};


/** @class KFPLinkedList
 ** @brief Structure to creat a linked list of the input data.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The structure contains pointer to array of KFPInputData objects. Copying of the 
 ** objects of this structure is disabled. The list is used to create a queue for processing
 ** at the device side (Intel Xeon Phi).
 **/

struct KFPLinkedList
{
  void *operator new(size_t size) { return _mm_malloc(size, sizeof(float_v)); }      ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size) { return _mm_malloc(size, sizeof(float_v)); }    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new(size_t size, void *ptr) { return ::operator new(size, ptr);}    ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void *operator new[](size_t size, void *ptr) { return ::operator new(size, ptr);}  ///< new operator for allocation of the SIMD-alligned dynamic memory allocation
  void operator delete(void *ptr, size_t) { _mm_free(ptr); }                         ///< delete operator for the SIMD-alligned dynamic memory release
  void operator delete[](void *ptr, size_t) { _mm_free(ptr); }                       ///< delete operator for the SIMD-alligned dynamic memory release
  
  KFPInputData data __attribute__((aligned(sizeof(float_v)))); ///< Input data for KF Particle Finder \see KFPInputData.
  KFPLinkedList* next; ///< Link to the nex object in the linked list.
} __attribute__((aligned(sizeof(float_v))));

#endif
