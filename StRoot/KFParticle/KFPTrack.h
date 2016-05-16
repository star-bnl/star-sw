//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifndef KFPTrack_H
#define KFPTrack_H

#include <cmath>
#include "TObject.h"
class KFPTrack 
#ifdef __ROOT__
: public TObject
#endif
{

public:
  KFPTrack():fChi2(-1.f), fQ(0), fNDF(-1), fId(-1) { }
  ~KFPTrack() { }

  int    GetID() const { return fId; }
  
  bool   GetXYZPxPyPz(float *p) const
         {
           for(int i=0; i<6; i++)
             p[i] = fP[i];
           return 1;
         }
  bool   GetCovarianceXYZPxPyPz(float cv[21]) const
         {
           for (int i=0; i<21; i++)
             cv[i] = fC[i];
           return 1;
         }

  bool   GetCovarianceXYZPxPyPz(double cv[21]) const
         {
           for (int i=0; i<21; i++)
             cv[i] = fC[i];
           return 1;
         }

//  void   GetXYZ(float *position) {position[0] = fP[0]; position[1] = fP[1]; position[1] = fP[1];}
  void   GetXYZ(float *position)    const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   GetPxPyPz(float *position) const {position[0] = fP[3]; position[1] = fP[4]; position[2] = fP[5];}

  void   XvYvZv(float *position)    const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   PxPyPz(float *position) const {position[0] = fP[3]; position[1] = fP[4]; position[2] = fP[5];}
  void   XvYvZv(double *position)    const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   PxPyPz(double *position) const {position[0] = fP[3]; position[1] = fP[4]; position[2] = fP[5];}

  float GetX() const { return fP[0]; }
  float GetY() const { return fP[1]; }
  float GetZ() const { return fP[2]; }
  float GetPx() const { return fP[3]; }
  float GetPy() const { return fP[4]; }
  float GetPz() const { return fP[5]; }

  float GetPt() const { return sqrt(fP[3]*fP[3]+fP[4]*fP[4]); }
  float GetP()  const { return sqrt(fP[3]*fP[3]+fP[4]*fP[4]+fP[5]*fP[5]); }

  void   GetCovarianceMatrix(float *covmatrix)
         {
           for (int i=0; i<21; i++)
             covmatrix[i] = fC[i];
         }
  float GetParameter(int i) const { return fP[i]; }
  float GetCovariance(int i) const { return fC[i]; }

  int    Charge()        const { return fQ; }
  float GetChi2perNDF() const { return fChi2/fNDF; }
  float GetChi2()       const { return fChi2;      }
  int    GetNDF()        const { return fNDF; }

  const float * GetTrack() const { return fP; }
  const float * GetCovMatrix() const { return fC; }

  void SetParameters(const float *position) 
       { 
         for(int i=0; i<6; i++)
           fP[i] = position[i];
       }
  void SetParameters(double *position) 
       { 
         for(int i=0; i<6; i++)
           fP[i] = position[i];
       }
  void SetParameters(float x, float y, float z, float px, float py, float pz) 
       { 
         fP[0] = x;  fP[1] = y;  fP[2] = z;
         fP[3] = px; fP[4] = py; fP[5] = pz;
       }
  void SetXYZ(float x, float y, float z) 
       { 
         fP[0] = x;  fP[1] = y;  fP[2] = z;
       }
  void SetPxPyPz(float px, float py, float pz) 
       { 
         fP[3] = px; fP[4] = py; fP[5] = pz;
       }
  void SetID(int id)       {fId = id;}

  void SetX(float x)      { fP[0] = x; }
  void SetY(float y)      { fP[1] = y; }
  void SetZ(float z)      { fP[2] = z; }
  void SetPx(float px)      { fP[3] = px; }
  void SetPy(float py)      { fP[4] = py; }
  void SetPz(float pz)      { fP[5] = pz; }
  void SetCharge(int q) { fQ = q; }
  void SetChi2(float chi) { fChi2 = chi; }
  void SetNDF(int ndf)     { fNDF = ndf; }

  void SetCovarianceMatrix(const float *C)
  {
    for (int i=0; i<21; i++)
      fC[i] = C[i];
  }
  void SetCovarianceMatrix(const double *C)
  {
    for (int i=0; i<21; i++)
      fC[i] = C[i];
  }
  
  void SetCovariance(const int i, const float c) { fC[i]=c; }
  
  
  void RotateXY( float alpha ); // rotate on alpha in XY plane. Should be usefull for CS change

  int Id() const { return fId; };
  void SetId( int id ){ fId = id; };

#ifdef NonhomogeneousField
  const float* GetFieldCoeff() const { return fieldRegion; }
  void SetFieldCoeff(float c, int i) { fieldRegion[i] = c; }
#endif
 private:

  float fP[6];  //coordinates of the vertex
  float fC[21];  //Covariance matrix of the vertex parameters
  float fChi2;  //chi-square of the vertex fitting
  char fQ;     //charge
  short fNDF;  //degree of freedom number

  int fId;
  
#ifdef NonhomogeneousField
  float fieldRegion[10];
#endif
#ifdef __ROOT__
  ClassDef(KFPTrack,1)
#endif
};

#endif
