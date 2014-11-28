#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "TRandom.h"
#include "TMath.h"
extern "C" {
#ifdef  __GNUC__
#define C_RAD_PER_DEG  0.017453292519943295769237
#define C_DEG_PER_RAD 57.295779513082320876798155
  Double_t dsind_(Double_t &degree) {return TMath::Sin (C_RAD_PER_DEG*degree);}
  Double_t dcosd_(Double_t &degree) {return TMath::Cos (C_RAD_PER_DEG*degree);}
  Double_t dtand_(Double_t &degree) {return TMath::Tan (C_RAD_PER_DEG*degree);}
  Double_t dasind_(Double_t &arg)   {return C_DEG_PER_RAD*TMath::ASin (arg);}
  Double_t dacosd_(Double_t &arg)   {return C_DEG_PER_RAD*TMath::ACos (arg);}
  Double_t datand(Double_t &arg)    {return C_DEG_PER_RAD*TMath::ATan (arg);}
  Double_t datan2d_(Double_t &arg1,Double_t &arg2) {return  C_DEG_PER_RAD*TMath::ATan2 (arg1,arg2);}
  Float_t sind_(Float_t &degree) {return TMath::Sin (C_RAD_PER_DEG*degree);}
  Float_t cosd_(Float_t &degree) {return TMath::Cos (C_RAD_PER_DEG*degree);}
  Float_t tand_(Float_t &degree) {return TMath::Tan (C_RAD_PER_DEG*degree);}
  Float_t asind_(Float_t &arg)   {return C_DEG_PER_RAD*TMath::ASin (arg);}
  Float_t acosd_(Float_t &arg)   {return C_DEG_PER_RAD*TMath::ACos (arg);}
#if ! defined( __ICC ) ||  __ICC < 910
  Float_t atand(Float_t &arg)    {return C_DEG_PER_RAD*TMath::ATan (arg);}
#endif
  Float_t atan2d_(Float_t &arg1,Float_t &arg2) {return  C_DEG_PER_RAD*TMath::ATan2 (arg1,arg2);}
#undef C_RAD_PER_DEG
#undef C_DEG_PER_RAD
#endif
  Float_t rndm_(Int_t &i)    {    return gRandom->Rndm();  }
  Float_t ranf_()            {    Int_t i = 0; return rndm_(i);  }
  Float_t freq_ (Float_t &x) { Double_t y = x;   return TMath::Freq(y); }
  Float_t prob_ (Float_t &chi2, Int_t &ndf) {Double_t y = chi2; return TMath::Prob(y,ndf);}
  Float_t chisin_ (Float_t &prob, Int_t &ndf) {Double_t y = prob; return TMath::ChisquareQuantile(y,ndf);}
  Float_t gausin_ (Float_t &p) {Double_t y = p; return 1 - TMath::Erfc(y);}
  void    rdmin_  (UInt_t &seed) {gRandom->SetSeed(seed);}
  void    rdmout_ (UInt_t &seed) {seed = gRandom->GetSeed();}
  Float_t ran_(Int_t &i) {return rndm_(i);}
#if 0
  void    rannor_(Float_t &a, Float_t &b) {Double_t x, y; gRandom->Rannor(x,y); a = x; b = y;}
#endif
  void ucopy_(const Float_t  *a, Float_t  *b, Int_t &n)                   {TCL::ucopy(a, b, n);}
  void dcopy_(const Double_t *a, Double_t *b, Int_t &n) 		    {TCL::ucopy(a, b, n);}

  void vzero_(Float_t *a,  Int_t &n2)                                     {TCL::vzero(a, n2);}
  void dzero_(Double_t *a, Int_t &n2) 				    {TCL::vzero(a, n2);}

  void vadd_(const Float_t *b,  const Float_t  *c,  Float_t *a, Int_t &n) {TCL::vadd(b, c, a, n);}
  void dadd_(const Double_t *b, const Double_t *c, Double_t *a, Int_t &n) {TCL::vadd(b, c, a, n);}

  Float_t  vdot_(const Float_t  *b, const Float_t  *a, Int_t &n)          {return TCL::vdot(b, a, n);}
  Double_t ddot_(const Double_t *b, const Double_t *a, Int_t &n) 	  {return TCL::vdot(b, a, n);}

  void vsub_(const Float_t  *a, const Float_t  *b, Float_t  *x, Int_t &n) {TCL::vsub(a, b, x, n);}
  void dsub_(const Double_t *a, const Double_t *b, Double_t *x, Int_t &n) {TCL::vsub(a, b, x, n);}

  void vcopyn_(const Float_t *a,  Float_t *x, Int_t &n)                   {TCL::vcopyn(a, x, n);}
  void dcopyn_(const Double_t *a, Double_t *x, Int_t &n) 		    {TCL::vcopyn(a, x, n);}

  void vscale_(const Float_t  *a, Float_t  &scale, Float_t  *b, Int_t &n) {TCL::vscale(a, scale, b, n);}
  void dscale_(const Double_t *a, Double_t &scale, Double_t *b, Int_t &n) {TCL::vscale(a, scale, b, n);}

  void vlinco_(const Float_t  *a, Float_t  &fa, const Float_t  *b, Float_t  &fb,Float_t  *x, Int_t &n) {TCL::vlinco(a, fa, b, fb,x, n);}
  void dlinco_(const Double_t *a, Double_t &fa, const Double_t *b, Double_t &fb,Double_t *x, Int_t &n) {TCL::vlinco(a, fa, b, fb,x, n);}

  void vmatl_(const Float_t  *g, const Float_t  *c, Float_t  *x, Int_t &n,Int_t &m) {TCL::vmatl(g, c, x, n,m);}
  void dmatl_(const Double_t *g, const Double_t *c, Double_t *x, Int_t &n,Int_t &m) {TCL::vmatl(g, c, x, n,m);}

  void vmatr_(const Float_t  *c, const Float_t  *g, Float_t  *x, Int_t &n,Int_t &m) {TCL::vmatr(c, g, x, n,m);}
  void dmatr_(const Double_t *c, const Double_t *g, Double_t *x, Int_t &n,Int_t &m) {TCL::vmatr(c, g, x, n,m);}

  void mxmad_( const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad( a, b, c, i, j, k);}
  void mxmad1_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad1(a, b, c, i, j, k);}
  void mxmad2_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad2(a, b, c, i, j, k);}
  void mxmad3_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad3(a, b, c, i, j, k);}
  void mxmpy_( const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy( a, b, c, i, j, k);}
  void mxmpy1_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy1(a, b, c, i, j, k);}
  void mxmpy2_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy2(a, b, c, i, j, k);}
  void mxmpy3_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy3(a, b, c, i, j, k);}
  void mxmub_( const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub( a, b, c, i, j, k);}
  void mxmub1_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub1(a, b, c, i, j, k);}
  void mxmub2_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub2(a, b, c, i, j, k);}
  void mxmub3_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub3(a, b, c, i, j, k);}

  void mxmlrt_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &ni, Int_t &nj) {TCL::mxmlrt(a, b, c, ni, nj);}
  void mxmltr_(const Float_t *a, const Float_t *b, Float_t *c, Int_t &ni, Int_t &nj) {TCL::mxmltr(a, b, c, ni, nj);}
  void mxtrp_(const Float_t *a, Float_t *b, Int_t &i, Int_t &j)                      {TCL::mxtrp(a, b, i, j);}

  void dxmad_ (const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad (a, b, c, i, j, k);}
  void dxmad1_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad1(a, b, c, i, j, k);}
  void dxmad2_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad2(a, b, c, i, j, k);}
  void dxmad3_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmad3(a, b, c, i, j, k);}
  void dxmpy_ (const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy (a, b, c, i, j, k);}
  void dxmpy1_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy1(a, b, c, i, j, k);}
  void dxmpy2_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy2(a, b, c, i, j, k);}
  void dxmpy3_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmpy3(a, b, c, i, j, k);}
  void dxmub_ (const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub (a, b, c, i, j, k);}
  void dxmub1_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub1(a, b, c, i, j, k);}
  void dxmub2_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub2(a, b, c, i, j, k);}
  void dxmub3_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &i, Int_t &j, Int_t &k) {TCL::mxmub3(a, b, c, i, j, k);}

  void dxmlrt_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &ni, Int_t &nj) {TCL::mxmlrt(a, b, c, ni, nj);}
  void dxmltr_(const Double_t *a, const Double_t *b, Double_t *c, Int_t &ni, Int_t &nj) {TCL::mxmltr(a, b, c, ni, nj);}
  void dxtrp_(const Double_t *a, Double_t *b, Int_t &i, Int_t &j) {TCL::mxtrp(a, b,  i,  j);}

// * TR pack

  void traat_(const Float_t *a, Float_t *s, Int_t &m, Int_t &n)                      {TCL::traat(a, s,  m,  n);}
  void tral_(const Float_t *a, const Float_t *u, Float_t *b, Int_t &m, Int_t &n)     {TCL::tral(a, u, b, m, n);}
  void tralt_(const Float_t *a, const Float_t *u, Float_t *b, Int_t &m, Int_t &n)    {TCL::tralt(a, u, b, m, n);}
  void tras_(const Float_t *a, const Float_t *s, Float_t *b, Int_t &m, Int_t &n)     {TCL::tras(a, s, b, m, n);}
  void trasat_(const Float_t *a, const Float_t *s, Float_t *r, Int_t &m, Int_t &n)   {TCL::trasat(a, s, r, m, n);}
  void trata_(const Float_t *a, Float_t *r, Int_t &m, Int_t &n)                      {TCL::trata(a, r, m, n);}
  void trats_(const Float_t *a, const Float_t *s, Float_t *b, Int_t &m, Int_t &n)    {TCL::trats(a, s, b, m, n);}
  void tratsa_(const Float_t *a, const Float_t *s, Float_t *r, Int_t &m, Int_t &n)   {TCL::tratsa(a, s, r, m, n);}
  void trchlu_(const Float_t *a, Float_t *b, Int_t &n)                              {TCL::trchlu(a, b, n);}
  void trchul_(const Float_t *a, Float_t *b, Int_t &n) 			           {TCL::trchul(a, b, n);}
  void trinv_(const Float_t *t, Float_t *s, Int_t &n)  			       	   {TCL::trinv(t, s,  n);}
  void trla_(const Float_t *u, const Float_t *a, Float_t *b, Int_t &m, Int_t &n)     {TCL::trla(u, a, b, m, n);}
  void trlta_(const Float_t *u, const Float_t *a, Float_t *b, Int_t &m, Int_t &n)    {TCL::trlta(u, a, b, m, n);}
  void trpck_(const Float_t *s, Float_t *u, Int_t &n)                               {TCL::trpck(s, u, n);}
  void trqsq_(const Float_t *q, const Float_t *s, Float_t *r, Int_t &m)             {TCL::trqsq(q, s, r, m);}
  void trsa_(const Float_t *s, const Float_t *a, Float_t *b, Int_t &m, Int_t &n)     {TCL::trsa(s, a, b, m, n);}
  void trsinv_(const Float_t *g, Float_t *gi, Int_t &n)                             {TCL::trsinv(g, gi, n);}
  void trsmlu_(const Float_t *u, Float_t *s, Int_t &n)  			           {TCL::trsmlu(u, s, n);}   
  void trsmul_(const Float_t *g, Float_t *gi, Int_t &n) 			      	   {TCL::trsmul(g, gi, n);}
  void trupck_(const Float_t *u, Float_t *s, Int_t &m)  			      	   {TCL::trupck(u, s, m);} 
  void trsat_(const Float_t *s, const Float_t *a, Float_t *b, Int_t &m, Int_t &n)    {TCL::trsat(s, a, b, m, n);}
  void trsequ_(Float_t *smx, Int_t &m, Float_t *b, Int_t &n)                   {TCL::trsequ(smx, m, b, n);}
// ---   Double_t version
  void draat_ (const Double_t *a, Double_t *s, Int_t &m, Int_t &n)                    {TCL::traat (a, s, m, n);}
  void dral_  (const Double_t *a, const Double_t *u, Double_t *b, Int_t &m, Int_t &n) {TCL::tral  (a, u, b, m, n);}
  void dralt_ (const Double_t *a, const Double_t *u, Double_t *b, Int_t &m, Int_t &n) {TCL::tralt (a, u, b, m, n);}
  void dras_  (const Double_t *a, const Double_t *s, Double_t *b, Int_t &m, Int_t &n) {TCL::tras  (a, s, b, m, n);}
  void drasat_(const Double_t *a, const Double_t *s, Double_t *r, Int_t &m, Int_t &n) {TCL::trasat(a, s, r, m, n);}
  void drata_ (const Double_t *a, Double_t *r, Int_t &m, Int_t &n)                    {TCL::trata (a, r, m, n);}
  void drats_ (const Double_t *a, const Double_t *s, Double_t *b, Int_t &m, Int_t &n) {TCL::trats (a, s, b, m, n);}
  void dratsa_(const Double_t *a, const Double_t *s, Double_t *r, Int_t &m, Int_t &n) {TCL::tratsa(a, s, r, m, n);}
  void drchlu_(const Double_t *a, Double_t *b, Int_t &n)                             {TCL::trchlu(a, b, n);}
  void drchul_(const Double_t *a, Double_t *b, Int_t &n)                             {TCL::trchul(a, b, n);}	
  void drinv_ (const Double_t *t, Double_t *s, Int_t &n) 			    {TCL::trinv (t, s, n);}
  void drla_  (const Double_t *u, const Double_t *a, Double_t *b, Int_t &m, Int_t &n) {TCL::trla  (u, a, b, m, n);}
  void drlta_ (const Double_t *u, const Double_t *a, Double_t *b, Int_t &m, Int_t &n) {TCL::trlta (u, a, b, m, n);}
  void drpck_ (const Double_t *s, Double_t *u, Int_t &n)                             {TCL::trpck (s, u, n);}
  void drqsq_ (const Double_t *q, const Double_t *s, Double_t *r, Int_t &m)          {TCL::trqsq (q, s, r, m);}
  void drsa_  (const Double_t *s, const Double_t *a, Double_t *b, Int_t &m, Int_t &n) {TCL::trsa  (s, a, b, m, n);}
  void drsinv_(const Double_t *g, Double_t *gi, Int_t &n)                            {TCL::trsinv(g, gi, n);} 
  void drsmlu_(const Double_t *u, Double_t *s, Int_t &n)  			    {TCL::trsmlu(u,  s, n);}  
  void drsmul_(const Double_t *g, Double_t *gi, Int_t &n) 			    {TCL::trsmul(g, gi, n);}
  void drupck_(const Double_t *u, Double_t *s, Int_t &m)  			    {TCL::trupck(u,  s, m);}
  void drsat_ (const Double_t *s, const Double_t *a, Double_t *b, Int_t &m, Int_t &n) {TCL::trsat (s,  a, b, m, n);}
  void drsequ_(Double_t *smx, Int_t &m, Double_t *b, Int_t &n)                  {TCL::trsequ(smx, m, b, n);}
  
}
