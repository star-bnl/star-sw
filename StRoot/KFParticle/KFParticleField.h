#ifndef KFParticleField_h
#define KFParticleField_h 1

#include "KFParticleDef.h"
#include <iostream>
using std::cout;
using std::ostream;
using std::endl;


class KFParticleFieldValue{


  public:
    KFParticleFieldValue():x(0.f),y(0.f),z(0.f){};
        
    float_v x, y, z;

    void Combine( KFParticleFieldValue &B, float_v w )
    {
      x+= w*( B.x - x );
      y+= w*( B.y - y );
      z+= w*( B.z - z );
    }

    friend ostream& operator<<(ostream& out, KFParticleFieldValue &B){
      return out << B.x[0] << " | " << B.y[0] << " | " << B.z[0];
    };
};

class KFParticleFieldRegion{

  public:
    KFParticleFieldRegion() {};
    KFParticleFieldRegion(const float field[10])
    {
      for(int i=0; i<10; i++)
        fField[i] = field[i];
    }
    
    float_v fField[10]; //cx0, cx1, cx2 ; // Bx(z) = cx0 + cx1*(z-z0) + cx2*(z-z0)^2
 //   float_v cy0, cy1, cy2 ; // By(z) = cy0 + cy1*(z-z0) + cy2*(z-z0)^2
 //   float_v cz0, cz1, cz2 ; // Bz(z) = cz0 + cz1*(z-z0) + cz2*(z-z0)^2

    KFParticleFieldValue Get(const float_v z){
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
      for(int i=0; i<10; i++)
        fField[i][iEntry] = field[i];
    }
    
    void SetOneEntry( const int i0, const KFParticleFieldRegion &f1, const int i1 )
    {
      for(int i=0; i<10; i++)
        fField[i][i0] = f1.fField[i][i1];
    }
};


#endif
