#include <stdlib.h>
#include "Riostream.h"
#include "TMath.h"
#include "TMatrixD.h"
#include "TVectorD.h"
#include "TDecompLU.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TH2F.h"
#include "TH1F.h"

enum { NB = 3 };

static const int LB[NB] = {8, 12, 16}; // number of ladders in a barrel
static const int WL[NB] = {4,  6,  7}; // number of wafers on a ladder
  
//---- class defined as a "struct" 

struct SvtDriftVel_t {
  enum { Pmax = 7, 
         nv = 10 }; 
  SvtDriftVel_t(const double * array);
  int	 GetHid() const;
  double Coord ( double timeb );
  void   addStat( double timeb, double u, double uP );	
  void 	 AddVal ( double * value, double next);
  int	 Solve (FILE *pFile);
  void   PrintV(void) const;
  void   PrintM(void) const;
  int   type;   // type = 0 average drift_velocity,
  int   idx;    // row index; not used yet
  int   nrows;  // total no. of real rows in the table; 
  int   npar;   // npar = 0;  
  int	  barrel;
  int	  ladder;
  int	  wafer;
  int	  hybrid;
  double t0;    
  double et0;       // t0 fit error
  double tmax;  
  double etmx;      // tmax fit error
  double d_length;  // drift length
  double v[nv]; // v[0] is average drift velocity = length*0.025/(tmax-t0) micron/ns
  int hStat;
  double coeff[Pmax];
  double matrx[Pmax][Pmax];
  double vectr[Pmax];
  TH2F * duU;
  TH2F * duUc;
};

//------------class "hybrid collection" ----------------------------------------

class Xdcor{
public:
enum {  NH = 432, 
	hAlive = 293,
	nArr = 23
     };
	Xdcor ();
	~Xdcor (){}
 	void   CorrPlot ( int barrel, int ladder );
	void   solve ( void );		             
 static double Poly ( const int L, double x );
 	void   InitSptr ( void ); 
	int    GetHid (int barrel, int ladder, int wafer, int hybrid) const;

//private:

  static const double dArr[hAlive][nArr];
  static SvtDriftVel_t* Sptr[NH];
  FILE * pFile; // = fopen (foName,"wt");

};
