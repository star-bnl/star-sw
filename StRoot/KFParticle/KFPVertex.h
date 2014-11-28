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

#ifndef KFPVertex_H
#define KFPVertex_H

class KFPVertex
{
 public:
  KFPVertex():fChi2(-1.f), fNContributors(0), fNDF(-1)  { }
  ~KFPVertex() { }

/*  KFPVertex(const KFPVertex& vVert); 
  KFPVertex& operator=(const KFPVertex& vVert);*/

  float GetX() const { return fP[0]; }
  float GetY() const { return fP[1]; }
  float GetZ() const { return fP[2]; }
  
  void   GetXYZ(float *position) const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   GetXYZ(double *position) const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   GetCovarianceMatrix(float *covmatrix) const
         {
           for (int i=0; i<6; i++)
             covmatrix[i] = fC[i];
         }
  void   GetCovarianceMatrix(double *covmatrix) const
         {
           for (int i=0; i<6; i++)
             covmatrix[i] = fC[i];
         }

  float GetChi2perNDF() const { return fChi2/fNDF; }
  float GetChi2()       const { return fChi2;      }
  int    GetNDF()        const { return fNDF; }
  int    GetNContributors() const { return fNContributors; }
  
  float GetParameter(int i) const { return fP[i]; }
  float GetCovariance(int i) const { return fC[i]; }


  void SetXYZ(float *position) { fP[0] = position[0]; fP[1] = position[1]; fP[2] = position[2]; }
  void SetXYZ(float x, float y, float z) { fP[0] = x; fP[1] = y; fP[2] = z; }
  void SetX(float x)      { fP[0] = x; }
  void SetY(float y)      { fP[1] = y; }
  void SetZ(float z)      { fP[2] = z; }
  void SetChi2(float chi) { fChi2 = chi; }
  void SetNDF(int ndf)     { fNDF = ndf; }
  void SetNContributors(int nc)  { fNContributors = nc; }

  void SetCovarianceMatrix(float *C)
       {
         for (int i=0; i<6; i++)
           fC[i] = C[i];
       }

  void SetCovarianceMatrix(float C00,float C10,float C11,float C20,float C21,float C22)
  {
    fC[0] = C00;
    fC[1] = C10;
    fC[2] = C11;
    fC[3] = C20;
    fC[4] = C21;
    fC[5] = C22;
  }

 private:

  float fP[3];  //coordinates of the vertex
  float fC[6];  //Covariance matrix of the vertex parameters
  float fChi2;  //chi-square of the vertex fitting
  int fNContributors; // number of tracks, from which the vertex was builded
  int fNDF;  //degree of freedom number
};

#endif
