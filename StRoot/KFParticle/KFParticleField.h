/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *               2007-2019 Sergey Gorbunov
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

#ifndef KFParticleField_h
#define KFParticleField_h 1

#include <iostream>

/** @class KFParticleFieldValue
 ** @brief A class to store a vector with the magnetic field values {Bx, By, Bz} at the certain point.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is used to represent the vector of the magnetic field at the certain position of the track.
 ** It contains three components of the magnetic field: Bx, By and Bz. It allows to combine the current
 ** field measurement with another measurement using a weight.
 **/

class KFParticleFieldValue
{
 public:
  KFParticleFieldValue():x(0.f),y(0.f),z(0.f){};
      
  float_v x; ///< Bx component of the magnetic field 
  float_v y; ///< By component of the magnetic field 
  float_v z; ///< Bz component of the magnetic field 

  void Combine( KFParticleFieldValue &B, float_v w )
  {
    /** Function allows to combine the current magntic field measurement with another measurement
      ** weighted by "w"
      ** \param[in] B - another field measurement to be combined with the current value
      ** \param[in] w - weight of the measurement being added
      **/
    x+= w*( B.x - x );
    y+= w*( B.y - y );
    z+= w*( B.z - z );
  }

  /** Operator to print components of the magnetic field in order Bx, By, Bz.
    ** \param[in] out - output stream where the values will be printed
    ** \param[in] B - field vecrot to be printed
    **/
  friend std::ostream& operator<<(std::ostream& out, KFParticleFieldValue &B){
    return out << B.x[0] << " | " << B.y[0] << " | " << B.z[0];
  };
};

/** @class KFParticleFieldRegion
 ** @brief A class to store an approximation of the magnetic field along the particle trajectory. Is used for nonhomogeneous field.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is used to store the approximation of the magnetic field along the particle trajectory.
 ** Each component Bx, By, Bz is approximated with a parabola function depending on the z coordinate.
 ** The class is fully vectorised, all parameters are stored in SIMD vectors.
 ** The class is used in case of the CBM-like nonhomogenious magnetic field.
 **/

class KFParticleFieldRegion
{
 public:
  KFParticleFieldRegion() {};
  KFParticleFieldRegion(const float field[10])
  {
    /** Sets current vectorised representation of the magnetic field approximation from the scalar input  array.
     ** \param[in] field[10] - the scalar input array with the magnetic field approximation
     **/
    for(int i=0; i<10; i++)
      fField[i] = field[i];
  }

  KFParticleFieldValue Get(const float_v z)
  {
    /** Returns a magnetic field vector calculated using current parametrisation at the given Z coordinate.
      ** \param[in] z - value of the Z coordinate, where magnetic field should be calculated.
      **/ 
    float_v dz = (z-fField[9]);
    float_v dz2 = dz*dz;
    KFParticleFieldValue B;
    B.x = fField[0] + fField[1]*dz + fField[2]*dz2;
    B.y = fField[3] + fField[4]*dz + fField[5]*dz2;
    B.z = fField[6] + fField[7]*dz + fField[8]*dz2;
    return B;
  }
  
  void Set( const KFParticleFieldValue &B0, const float_v B0z,
            const KFParticleFieldValue &B1, const float_v B1z,
            const KFParticleFieldValue &B2, const float_v B2z )
  {
    /** Approximates the magnetic field with the parabolas using three points along the particle trajectory.
     ** \param[in] B0 - magnetic field vector at the first point
     ** \param[in] B0z - Z position of the first point
     ** \param[in] B1 - magnetic field vector at the second point
     ** \param[in] B1z - Z position of the second point
     ** \param[in] B2 - magnetic field vector at the third point
     ** \param[in] B2z - Z position of the third point
     **/
    fField[9] = B0z;
    float_v dz1 = B1z-B0z, dz2 = B2z-B0z;
    float_v det = 1.f/(float_v(dz1*dz2*(dz2-dz1)));
    float_v w21 = -dz2*det;
    float_v w22 = dz1*det;
    float_v w11 = -dz2*w21;
    float_v w12 = -dz1*w22;
    
    float_v dB1 = B1.x - B0.x;
    float_v dB2 = B2.x - B0.x;
    fField[0] = B0.x;
    fField[1] = dB1*w11 + dB2*w12 ;
    fField[2] = dB1*w21 + dB2*w22 ;
    
    dB1 = B1.y - B0.y;
    dB2 = B2.y - B0.y;
    fField[3] = B0.y;
    fField[4] = dB1*w11 + dB2*w12 ;
    fField[5] = dB1*w21 + dB2*w22  ;

    dB1 = B1.z - B0.z;
    dB2 = B2.z - B0.z;
    fField[6] = B0.z;
    fField[7] = dB1*w11 + dB2*w12 ;
    fField[8] = dB1*w21 + dB2*w22   ;
  }

  void Set( const KFParticleFieldValue &B0, const float_v B0z,
            const KFParticleFieldValue &B1, const float_v B1z )
  {
    /** Approximates the magnetic field with the strainght line using two points.
     ** \param[in] B0 - magnetic field vector at the first point
     ** \param[in] B0z - Z position of the first point
     ** \param[in] B1 - magnetic field vector at the second point
     ** \param[in] B1z - Z position of the second point
     **/
    fField[9] = B0z;
    float_v dzi = 1.f/(float_v( B1z - B0z));
    fField[0] = B0.x;
    fField[3] = B0.y;
    fField[6] = B0.z;
    fField[1] = ( B1.x - B0.x )*dzi;
    fField[4] = ( B1.y - B0.y )*dzi;
    fField[7] = ( B1.z - B0.z )*dzi;
    fField[2] = fField[5] = fField[8] = 0.f;
  }
  
  void SetOneEntry( const float* field, int iEntry=0 )
  {
    /** Sets one element of the SIMD vector with index iEntry.
     ** \param[in] field - a scalar input array with the approximation of the magnetic field
     ** \param[in] iEntry - entry number of the current SIMD vectors to be set with the input approximation
     **/
    for(int i=0; i<10; i++)
      fField[i][iEntry] = field[i];
  }
  
  void SetOneEntry( const int i0, const KFParticleFieldRegion &f1, const int i1 )
  {
    /** Copies the field approximation from the vector f1 with index i1 to the SIMD vector elemets of the current object with index i0.
     ** \param[in] i0 - index of the SIMD vector elements of the current field approximation to be set
     ** \param[in] f1 - input approximation of the magnetic field
     ** \param[in] i1 - index of the SIMD vector elements of the input approximation to be copied to the current object 
     **/
    for(int i=0; i<10; i++)
      fField[i][i0] = f1.fField[i][i1];
  }
    
  /** The coefficients of the field approximation: \n
   ** cx0 = fField[0], cx1 = fField[1], cx2 = fField[2] - coefficients of the Bx approximation; \n
   ** cy0 = fField[3], cy1 = fField[4], cy2 = fField[5] - coefficients of the By approximation; \n
   ** cz0 = fField[6], cz1 = fField[7], cz2 = fField[8] - coefficients of the Bz approximation; \n
   ** z0 = fField[9] - reference Z coordinate. \n
   ** Bx(z) = cx0 + cx1*(z-z0) + cx2*(z-z0)^2 \n
   ** By(z) = cy0 + cy1*(z-z0) + cy2*(z-z0)^2 \n
   ** Bz(z) = cz0 + cz1*(z-z0) + cz2*(z-z0)^2
   **/
  float_v fField[10]; 
};


#endif
