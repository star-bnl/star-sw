#ifndef __Pythia6_h__
#define __Pythia6_h__

#include "StarCallf77.h"
#include <string>
using namespace std;

#include "TObject.h"
  
/// Generate a pythia event
void PyEvnt();
/// Print end of run statistics
void PyStat( Int_t stat );
/// List particles
void PyList( Int_t list );
/// Setup pythia tune
void PyTune( Int_t tune );
/// Copy particles into the HEPEVT common block
void PyHepc( Int_t mode );
/// Initialize pythia
void PyInit( string frame, string blue, string yellow, Double_t energy );

void Py1Ent( int ip, int kf, double energy, double theta, double phi );
int  PyComp( int kf );

void PyCloseDecays( int id );
void PyOpenDecay( int id, int idcy, int value ); 


//
// Interface to the PYJETS common block
//

#define address_of_pyjets F77_NAME( address_of_pyjets, ADDRESS_OF_PYJETS )
struct PyJets_t {
  /* Layout of the memory. */
  Int_t    n;
  Int_t    npad;
  Int_t    _k[5][4000];
  Double_t _p[5][4000];
  Double_t _v[5][4000]; 
  /* Add access methods which mimic fortran arrays */
  Int_t    &k( Int_t i, Int_t j ){ return _k[j-1][i-1]; }
  Double_t &p( Int_t i, Int_t j ){ return _p[j-1][i-1]; }
  Double_t &v( Int_t i, Int_t j ){ return _v[j-1][i-1]; }
};
extern "C" PyJets_t *address_of_pyjets();

//
// Interface to the PYSUBS common block
//       COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
//
#define address_of_pysubs F77_NAME( address_of_pysubs, ADDRESS_OF_PYSUBS )
struct PySubs_t {
  Int_t    msel; // Process selection switch
  Int_t    padding; // 
  Int_t    _msub[500]; // Individual processes (indexed from zero)
  Int_t    _kfin[81][2]; //
  Double_t _ckin[200]; //
  Int_t    &msub( Int_t i ){ return _msub[i-1]; }
  Int_t    &kfin( Int_t i, Int_t j ){ return _kfin[j-40][i-1]; }
  Double_t &ckin( Int_t i ){ return _ckin[i-1]; }
};
extern "C" PySubs_t *address_of_pysubs();

//
// Interface to the PYDAT3 common block
//      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
//
#define address_of_pydat3 F77_NAME( address_of_pydat3, ADDRESS_OF_PYDAT3 )
struct PyDat3_t {
  Int_t    _mdcy[3][500];
  Int_t    _mdme[2][8000];
  Double_t _brat[8000];
  Int_t    _kfdp[8000];
  Int_t    &mdcy(Int_t i, Int_t j){ return _mdcy[j-1][i-1]; }
  Int_t    &mdme(Int_t i, Int_t j){ return _mdme[j-1][i-1]; }
  Double_t &brat(Int_t i){ return _brat[i-1]; }
  Int_t    &kfdp(Int_t i){ return _kfdp[i-1]; }
};
extern "C" PyDat3_t *address_of_pydat3();

//
// Interface to the PYPARS common block
//      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
//
#define address_of_pypars F77_NAME( address_of_pypars, ADDRESS_OF_PYPARS )
struct PyPars_t {
  Int_t    _mstp[200];
  Double_t _parp[200];
  Int_t    _msti[200];
  Double_t _pari[200];
  Int_t    &mstp( Int_t i ){ return _mstp[i-1]; }
  Double_t &parp( Int_t i ){ return _parp[i-1]; }
  Int_t    &msti( Int_t i ){ return _msti[i-1]; }
  Double_t &pari( Int_t i ){ return _pari[i-1]; }
};
extern "C" PyPars_t *address_of_pypars();

//
// Interface to the PYINT5 common block
//      COMMON/PYINT5/NGENPD,NGEN(0:500,3),XSEC(0:500,3)
//
#define address_of_pyint5 F77_NAME( address_of_pyint5, ADDRESS_OF_PYINT5 )
struct PyInt5_t {
  Int_t    _ngen[3][501];
  Double_t _xsec[3][501];
  Int_t    &ngen( Int_t isub, Int_t i){ return _ngen[i-1][isub]; }
  Double_t &xsec( Int_t isub, Int_t i){ return _xsec[i-1][isub]; }
};
extern "C" PyInt5_t *address_of_pyint5();

#endif
