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

#ifndef KFPVertex_H
#define KFPVertex_H

/** @class KFPVertex
 ** @brief A scalar class for storage of the vertex in the cartesian parametrisation.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** A vertex is described with the state vector { X, Y, Z }
 ** and the corresponding covariance matrix. Also contains chi2 of the fit,
 ** corresponding number of degrees of freedom,
 ** and number of tracks which were used to construct current vertex.
 ** The class is used to provide an external vertex through the interfaces
 ** to the KF Particle package.
 **/

class KFPVertex
{
 public:
  KFPVertex();
  ~KFPVertex() { }

/*  KFPVertex(const KFPVertex& vVert); 
  KFPVertex& operator=(const KFPVertex& vVert);*/

  float GetX() const { return fP[0]; } ///< Returns X coordinate of the vertex.
  float GetY() const { return fP[1]; } ///< Returns Y coordinate of the vertex.
  float GetZ() const { return fP[2]; } ///< Returns Z coordinate of the vertex.
  
  /** Copies position of the vertex to the output array of floats. \param[out] position - the output array with the position of the vertex **/
  void GetXYZ(float *position) const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  /** Copies position of the vertex to the output array of doubles. \param[out] position - the output array with the position of the vertex **/
  void GetXYZ(double *position) const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void GetCovarianceMatrix(float *covmatrix) const
  {
    /** Copies the covariance matrix of the vertex to the array of floats.
     ** \param[out] covmatrix[6] - the output array, where the covariance matrix is copied
     **/
    for (int i=0; i<6; i++)
      covmatrix[i] = fC[i];
  }
  void GetCovarianceMatrix(double *covmatrix) const
  {
    /** Copies the covariance matrix of the vertex to the array of doubles.
     ** \param[out] covmatrix[6] - the output array, where the covariance matrix is copied
     **/
    for (int i=0; i<6; i++)
      covmatrix[i] = fC[i];
  }

  float GetChi2perNDF() const { return fChi2/fNDF; } ///< Returns Chi2/NDF of the vertex, NDF is a number of degrees of freedom.
  float GetChi2()       const { return fChi2;      } ///< Returns Chi2 of the vertex fit.
  int   GetNDF()        const { return fNDF; }       ///< Returns number of degrees of freedom of the vertex.
  int   GetNContributors() const { return fNContributors; } ///< Returns number of tracks which were used for construction of the vertex
  
  float GetParameter(int i) const { return fP[i]; }  ///< Returns parameter "i" of the vertex. \param[in] i - index of the parameter to be returned
  float GetCovariance(int i) const { return fC[i]; } ///< Returns element of the covariance matrix "i" of the vertex. \param[in] i - index of the element to be returned

  /** Sets position { X, Y, Z } of the vertex from the input array of doubles.
    ** \param[in] position - input array with the vertex parameters
    **/
  void SetXYZ(float *position) { fP[0] = position[0]; fP[1] = position[1]; fP[2] = position[2]; }
  /** Sets position { X, Y, Z } of the vertex.
   ** \param[in] x - X coordinate to be set
   ** \param[in] y - Y coordinate to be set
   ** \param[in] z - Z coordinate to be set
   **/
  void SetXYZ(float x, float y, float z) { fP[0] = x; fP[1] = y; fP[2] = z; }
  void SetX(float x)      { fP[0] = x; } ///< Sets X coordinate of the vertex
  void SetY(float y)      { fP[1] = y; } ///< Sets Y coordinate of the vertex
  void SetZ(float z)      { fP[2] = z; } ///< Sets Z coordinate of the vertex
  void SetChi2(float chi) { fChi2 = chi; } ///< Sets Chi2 of the vertex
  void SetNDF(int ndf)    { fNDF = ndf; } ///< Sets number of degrees of freedom of the vertex
  void SetNContributors(int nc) { fNContributors = nc; } ///< Sets number of tracks which were used for construction of the vertex

  void SetCovarianceMatrix(float *C)
  {
    /** Sets the covariance matrix from the input array of floats.
     ** \param[in] C[6] - array with the input elements of the covariance matrix stored in the lower triangular form
     **/
    for (int i=0; i<6; i++)
      fC[i] = C[i];
  }

  void SetCovarianceMatrix(float C00,float C10,float C11,float C20,float C21,float C22)
  {
    /** Sets the covariance matrix from the input array of floats.
     ** \param[in] C00 - Cxx
     ** \param[in] C10 - Cxy = Cyx
     ** \param[in] C11 - Cyy
     ** \param[in] C20 - Cxz = Czx
     ** \param[in] C21 - Cyz = Czy
     ** \param[in] C22 - Czz
     **/
    fC[0] = C00;
    fC[1] = C10;
    fC[2] = C11;
    fC[3] = C20;
    fC[4] = C21;
    fC[5] = C22;
  }

 private:

  float fP[3];  ///< Coordinates of the vertex.
  float fC[6];  ///< Covariance matrix of the vertex parameters.
  float fChi2;  ///< Chi-square of the vertex fit.
  int fNContributors; ///< Number of tracks, from which the vertex was built.
  int fNDF;  ///< Number of degrees of freedom of the vertex fit.
};

#endif
