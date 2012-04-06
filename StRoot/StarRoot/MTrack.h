#ifndef VTrack_H
#define VTrack_H

class MTrack
{

public:
  MTrack() { }
  ~MTrack() { }
/*MTrack(const MTrack& vTrack); 
  MTrack& operator=(const MTrack& vTrack);*/

  int    GetID() const { return ID; }
  
  bool   GetXYZ(double *p) const
         {
           for(int i=0; i<6; i++)
             p[i] = fP[i];
           return 1;
         }
  bool   GetCovarianceXYZPxPyPz(double cv[21]) const
         {
           for (int i=0; i<21; i++)
             cv[i] = fC[i];
           return 1;
         }

//  void   GetXYZ(double *position) {position[0] = fP[0]; position[1] = fP[1]; position[1] = fP[1];}
  void   XvYvZv(double *position) const {position[0] = fP[0]; position[1] = fP[1]; position[2] = fP[2];}
  void   PxPyPz(double *position) const {position[0] = fP[3]; position[1] = fP[4]; position[2] = fP[5];}

  double GetX() const { return fP[0]; }
  double GetY() const { return fP[1]; }
  double GetZ() const { return fP[2]; }
  double GetPx() const { return fP[3]; }
  double GetPy() const { return fP[4]; }
  double GetPz() const { return fP[5]; }
  void   GetCovarianceMatrix(double *covmatrix)
         {
           for (int i=0; i<21; i++)
             covmatrix[i] = fC[i];
         }
  double GetParameter(int i) const { return fP[i]; }
  double GetCovariance(int i) const { return fC[i]; }

  int    Charge()        const { return fQ; }
  double GetChi2perNDF() const { return fChi2/fNDF; }
  double GetChi2()       const { return fChi2;      }
  int    GetNDF()        const { return fNDF; }

  void SetParameters(double *position) 
       { 
         for(int i=0; i<6; i++)
           fP[i] = position[i];
       }
  void SetParameters(double x, double y, double z, double px, double py, double pz) 
       { 
         fP[0] = x;  fP[1] = y;  fP[2] = z;
         fP[3] = px; fP[4] = py; fP[5] = pz;
       }
  void SetXYZ(double x, double y, double z) 
       { 
         fP[0] = x;  fP[1] = y;  fP[2] = z;
       }
  void SetPxPyPz(double px, double py, double pz) 
       { 
         fP[3] = px; fP[4] = py; fP[5] = pz;
       }
  void SetID(int id)       {ID = id;}

  void SetX(double x)      { fP[0] = x; }
  void SetY(double y)      { fP[1] = y; }
  void SetZ(double z)      { fP[2] = z; }
  void SetCharge(int q) { fQ = q; }
  void SetChi2(double chi) { fChi2 = chi; }
  void SetNDF(int ndf)     { fNDF = ndf; }

  void SetCovarianceMatrix(double *C)
  {
    for (int i=0; i<21; i++)
      fC[i] = C[i];
  }

 private:

  int ID;
  double fP[6];  //coordinates of the vertex
  double fC[21];  //Covariance matrix of the vertex parameters
  double fChi2;  //chi-square of the vertex fitting
  int fQ;     //charge
  int fNDF;  //degree of freedom number
};

#endif
