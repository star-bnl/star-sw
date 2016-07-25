#include "Riostream.h"
#include "TF1.h"
#include "TMath.h"
TF1 *shaperF = 0;
TF1 *shaperI = 0;
TF1 *shaperD = 0;
TF1 *shaperT = 0;
TF1 *shaperP = 0;
TF1 *shaperIP = 0;
Double_t TimeBin = 1.06580265598021882e+02;
#define PULSER
#ifdef PULSER
Double_t AP10[3]   = {0.07754,  0.64263,   0.0};
Double_t tauP10[3] = {26.5, 99.5., 2280.};
#else
Double_t AP10[3]   = {26.8,  1.0,   0.1};
Double_t tauP10[3] = {20.9, 287., 2280.};
#endif
//________________________________________________________________________________
Double_t Laplace3(Double_t t, Double_t a, Double_t b, Double_t d) {
  /*
     s + d
   -----------    => A*exp(a*t) + B*exp(b*t);
   (s-a)*(s-b)
       a+d       b+d
   A = ---;  B = ---;
       a-b       b-a
  */
  if (-d == a) {return TMath::Exp(b*t);}
  if (-d == b) {return TMath::Exp(a*t);}
  if ( a == b) {return (1. + (a + d)*t)*TMath::Exp(a*t);}
  Double_t A = (a+d)/(a-b);
  Double_t B = (b+d)/(b-a);
  return A*TMath::Exp(a*t) + B*TMath::Exp(b*t);
}
//________________________________________________________________________________
Double_t Laplace2S(Double_t t, Double_t a, Double_t b) {
  /* 
      1/(s-a)/(s-b) => 
      
}
//________________________________________________________________________________
Double_t Laplace3S(Double_t t, Double_t a, Double_t b, Double_t d) {
  /*
     s + d
   -----------    => A*exp(a*t) + B*exp(b*t) + K;
  s*(s-a)*(s-b)
       a+d       b+d           d
   A = ---;  B = ---;    K = ----
     a*(a-b)    b*(b-a)       a*b
                        1
     0.  d == 0 => ------------
                    (s-a)*(s-b)
                         1
     1. -d == a =>   -----------
                       s*(s-b)

                         1
     2. -d == b =>  ----------
                      s*(s-a)
                        s + d
     3.  a == b =>   ------------- 
                       s*(s-a)**2
  */ 
  if (-d == a) {return TMath::Exp(b*t);}
  if (-d == b) {return TMath::Exp(a*t);}
  if ( a == b) {return (1. + (a + d)*t)*TMath::Exp(a*t);}
  Double_t A = (a+d)/(a*(a-b));
  Double_t B = (b+d)/(b*(b-a));
  Double_t K =  d/(a*b);
  return A*TMath::Exp(a*t) + B*TMath::Exp(b*t) + K;
}
//________________________________________________________________________________
Double_t P10Signale(Double_t *x, Double_t *par) {
  // h(t) = 1/(1. + t/t0) ~ e(t) = Sum_t over i = 0-2 (AP10[i]*exp(-t/tauP10[i])
  if (x[0] <= 0) return 0;
  if (par[0]);
  Double_t t = TimeBin*x[0];
  Double_t val = 0;
  for (Int_t i = 0; i < 3; i++) val += AP10[i]*TMath::Exp(-t/tauP10[i]);
  return val;
}
//________________________________________________________________________________
Double_t LI1(Double_t t, Double_t a, Double_t b, Double_t d) {
  /*                 s + d
        I(s) = -----------------; I(t) = A*exp(a*t) + B*exp(b*t) + K;
               s*(s - a)*(s - b)
                     a + d          b + d         d
	       A = ---------; B = ---------; K = ---
                   a*(a - b)      b*(b - a)      a*b
  */
  if (TMath::Abs(d) < 1.e-7) {
    /*    I(s) = 1/((s-a)*(s-b)) => A*exp(a*t) + B*exp(b*t) = A*(exp(a*t) - exp(b*t))
	  A = 1/(a-b); B = 1/(b-a)
    */
    if (TMath::Abs(a-b) < 1.e-7) {
      /* I(s) = 1/(s-a)**2 => t*exp(a*t) */
      return t*TMath::Exp(a*t);
    }
    Double_t A = 1./(a-b);
    return A*(TMath::Exp(a*t) - TMath::Exp(b*t));
  }
  if (TMath::Abs(a-b) < 1.e-7) {
    if (TMath::Abs(a) < 1.e-7) {
      /* I(s) = (s+d)/s**3 = 1/s**2 + d/s**3 => t + d*(t*t)/2; 
	 1/s**n => t**(n-1)/(n-1)! */
      return t + d*t*t/2.;
    }

    /* I(s) = (s+d)/(s*(s-a)**2) => (A + A1*t)*exp(a*t) + K;
       A = - d/a**2; A1 = (a+d)/a; K = -A; */
    Double_t A = -d/(a*a);
    Double_t A1 = (a + d)/a;
    Double_t K  = -A;
    return (A + A1*t)*exp(a*t) + K;
  }
  if (TMath::Abs(a) < 1.e-7) {
    /* I(s) = (s + d)/(s**2*(s-b)) => 1/(s*(s-b)) + d/(s**2*(s-b))
       1/(s*(s-b)) => A*exp(b*t) + K = 1/a*(exp(b*t) - 1)     A = 1/b; K = -1/b; */
    return (TMath::Exp(b*t) - 1)/b;
  }
  if (TMath::Abs(b) < 1.e-7) {
    /* I(s) = (s + d)/(s**2*(s-a)) => 1/(s*(s-a)) + d/(s**2*(s-a))
       1/(s*(s-a)) => A*exp(a*t) + K = 1/a*(exp(a*t) - 1)     A = 1/a; K = -1/a; */
    return (TMath::Exp(a*t) - 1)/a;
  }
  Double_t A = (a + d)/(a*(a - b));
  Double_t B = (b + d)/(b*(b - a));
  Double_t K = d/(a*b);
  return A*TMath::Exp(a*t) + B*TMath::Exp(b*t) + K;

}
//________________________________________________________________________________
Double_t LI2(Double_t t, Double_t a, Double_t b, Double_t c, Double_t d) {
  /*                    s + d
        I(s) = ------------------------; I(t) = A*exp(a*t) + B*exp(b*t) + C*exp(c*t)
               (s - a)*(s - b)*(s - c)
                        a + d                b + d             c + d
	       A = ---------------; B = --------------; C = --------------;
                   (a - b)*(a - c)      (b - a)*(b - c)     (c - a)*(c - b)
  */
  if (TMath::Abs(a - b) < 1.e-7 && TMath::Abs(a - c) < 1.e-7 && TMath::Abs(b - c) < 1.e-7) {
    /* I(s) = (s + d)/(s-a)**3 = (s - a + d + a)/(s-a)**3 = 1/(s-a)**2 + (d+a)/(s-a)**3 */
    if (TMath::Abs(a) < 1.e-7)  return t + d*t*t/2.;
    return t*TMath::Exp(a*t)*(1. + (d+a)/2.*t);
  }
  if (TMath::Abs(a - b) < 1.e-7) {
  /* I(s) = (s + d)/((s - a)**2 *(s-c)) => (A + A1*t)*exp(a*t) + B*exp(c*t)
  A = - 1/(a - c)**2; A1 = 1/(a - c); B = -A;
    */
    Double_t A1 = 1/(a - c);
    Double_t A = A1*A1;
    Double_t B = A;
    return  (A + A1*t)*TMath::Exp(a*t) + B*TMath::Exp(c*t);
  }
  if (TMath::Abs(a - c) < 1.e-7) {
  /* I(s) = (s + d)/((s-a)**2 *(s-b)) => (A + A1*t)*exp(a*t) + B*exp(b*t)
  A = - 1/(a - b)**2; A1 = 1/(a - b); B = -A;
    */
    Double_t A1 = 1/(a - b);
    Double_t A = A1*A1;
    Double_t B = A;
    return  (A + A1*t)*TMath::Exp(a*t) + B*TMath::Exp(b*t);
  }
  if (TMath::Abs(b - c) < 1.e-7) {
    /* I(s) = (s + d)/((s-a)*(s-b)**2) => (A + A1*t)*exp(b*t) + B*exp(a*t)
     A = - 1/(b - a)**2; A1 = 1/(b - a); B = -A;
    */
    Double_t A1 = 1/(b - a);
    Double_t A = A1*A1;
    Double_t B = A;
    return  (A + A1*t)*TMath::Exp(b*t) + B*TMath::Exp(a*t);
  }
  
  Double_t A = (a + d)/((a - b)*(a - c));
  Double_t B = (b + d)/((b - a)*(b - c));
  Double_t C = (c + d)/((c - a)*(c - b));
  return A*TMath::Exp(a*t) + B*TMath::Exp(b*t) + C*TMath::Exp(c*t);
}
//________________________________________________________________________________
Double_t LI12(Double_t t, Double_t a, Double_t b, Double_t c, Double_t d) {
  return LI1(t,a,b,d) - LI2(t,a,b,c,d);
}
//________________________________________________________________________________
Double_t L(Double_t t, Double_t a) {
  // i(s) = 1/(s - a) => exp(a*t)
  if (TMath::Abs(a) < 1.e-7) return 1;
  return TMath::Exp(a*t);
}
//________________________________________________________________________________
Double_t L2(Double_t t, Double_t a, Double_t b) {
  // i(s) = 1/(s - a)/(s - b) => exp(a*t)
  // a == b == 0 : i(s) = 1/s**2 => t
  // b == 0      : i(s) = 1/s/(s - b) = A*exp(a*t) + K; A = 1/a; K = -1/a
  //                                  = 1/b*(exp(b*t) - 1)
  // a == 0      :                    = 1/b*(exp(b*t) - 1)     
  //             : A*exp(a*t) + B*exp(b*t) = 1/(a-b)*(exp(a*t) - exp(b*t)) 
  
  if (TMath::Abs(a) < 1.e-7 &&  TMath::Abs(b) < 1.e-7) return t;
  if (TMath::Abs(a) < 1.e-7) return 1./b*(TMath::Exp(b*t) - 1.);
  if (TMath::Abs(b) < 1.e-7) return 1./a*(TMath::Exp(a*t) - 1.);
  if (TMath::Abs(a-b) < 1.e-7) return t*TMath::Exp(a*t);
  return 1./(a-b)*(TMath::Exp(a*t) - TMath::Exp(b*t));
}
//________________________________________________________________________________
Double_t LI(Double_t *x, Double_t *par) {
  // i(s) = 1/s - 1/(s - a) = 1/s - 1/(s + 1/tau) => 1 - exp(a*t)
  // i(t) = 1 - exp(-t/tau)
  if (x[0] <= 0) return 0;
  if (par[0]);
  Double_t t = TimeBin*x[0];
  Double_t L0 = L(t,0.);
  Double_t L1 = L(t,-1./par[0]);
  Double_t val = par[1]*L0 - par[2]*L1;
  //  cout << "\tt\t" << t << "\tL0\t" << L0 << "\tL1\t" << L1 << "\tval\t" << val << endl;
  return val;
}
//________________________________________________________________________________
Double_t L3P10(Double_t *x, Double_t *par) {
  // e(s) =  Sum_t over i = 0-2 (AP10[i]/(s+1/tauP10[i]));
  if (x[0] <= 0) return 0;
  if (par[0]);
  Double_t t = TimeBin*x[0];
  Double_t val = 0;
  for (Int_t i = 0; i < 3; i++) val += AP10[i]*L(t,-1./tauP10[i]);
  return val;
}
//________________________________________________________________________________
Double_t L3P10I(Double_t *x, Double_t *par) {
  // f(s) = e(s)*i(s) = Sum_t over i = 0-2 (AP10[i]/(s+1/tauP10[i])) *(1/s - 1/(s + 1/tauI) =>
  // AP10[i]*( 1/(s + 1/tauP10[i])/s + 1/(s + 1/tauP10)/(s + 1/tauI))
  if (x[0] <= 0) return 0;
  Double_t t = TimeBin*x[0];
  Double_t tauI = par[0];
  Double_t val = 0;
  for (Int_t i = 0; i < 3; i++) val += AP10[i]*(L2(t,-1./tauP10[i],0.) - L2(t,-1./tauP10[i],-1./tauI));
  return val;
}
//________________________________________________________________________________
Double_t Laplace(Double_t t, Double_t tau1, Double_t tau2, Double_t tau3) {
  /*    time representation for Laplace form
	V(s) = (1 + s*tau1)/(1 + s*tau2)/(1 + s*tau3);
	V(s) = V1(s) + V2(s);
	V1(s) = 1/(1 + s*tau2)/(1 + s*tau3) = 1/(tau2*tau3)/((s + 1/tau2)*(s + 1/tau3))
	a = -1/tau2; b = -1/tau3;
	V1(s) = a*b/((s-a)*(s-b)) => V1(t) = a*b/(a-b)*(exp(a*t) - exp(b*t))
	V2(s) = tau1*s/(1 + s*tau2)/(1 + s*tau2) = tau1*s*a*b/((s-a)*(s-b)) =>
	V2(t) = tau1*a*b/(a-b)*(a*exp(a*t) - b*exp(b*t))
	Case : tau1 == tau2
	V(s) = 1/(1 + s*tau3) = 1/(tau3*(s + 1/tau3)) = -b/(s - b) => -b *exp(b*t);
	Case : tau1 == tau3
	V(s) = 1/(1 + s*tau2) = 1/(tau2*(s + 1/tau2)) = -a/(s - a) => -a *exp(a*t);
  */
  if (tau1 != tau3 && tau1 != tau2 && tau2 != tau3) {
    Double_t a = -1./tau2;
    Double_t b = -1./tau3;
    return a*b/(a-b)*((TMath::Exp(a*t) - TMath::Exp(b*t)) + tau1*(a*TMath::Exp(a*t) - b*TMath::Exp(b*t)));
  }
  else {// V(s) = 1/(1 + s*tau2) => 1/tau2*/(s - 1/tau2) => 1/tau2*exp(-t/tau2);
    if (tau2 == tau3) return TMath::Exp(-t/tau1)/tau1;
    if (tau1 == tau2) return TMath::Exp(-t/tau3)/tau3;
    if (tau1 == tau3) return TMath::Exp(-t/tau2)/tau2;
  }
  return 0;
}
//________________________________________________________________________________
Double_t Laplace1(Double_t t, Double_t a, Double_t b) {
  /*
        1
   ------------    => A*exp(a*t) + B*exp(b*t) + K;
   s*(s-a)*(s-b)
          1            1            1
   A = -------;  B = -------; K = -----;
       a*(a-b)       b*(b-a)       a*b
  */
  Double_t A = 1./(a*(a-b));
  Double_t B = 1./(b*(b-a));
  Double_t K = 1./(a*b);
  return A*TMath::Exp(a*t) + B*TMath::Exp(b*t) + K;
}
//________________________________________________________________________________
Double_t Laplace2(Double_t t, Double_t a, Double_t d) {
  /*
      s+d
   --------  => A*exp(a*t) + K;
    s*(s-a)

   A = (1 + d/a); K = - d/a;

  */
  if (-d == a) {
    return 1;
  }
  Double_t A = 1. + d/a;
  Double_t K = -d/a;
  return A*TMath::Exp(a*t) + K;
}
//________________________________________________________________________________
Double_t shaper3D(Double_t *x,Double_t *par) {
  /* H(s) = (1/s - 1/(s-a))*(s+d)/(s-b) = H_2 - H_3
     H_2(s) = (s+d)/(s*(s-b));
     H_3(s) = (s+d)/((s-a)*(s-b))
  */
  if (x[0] <= 0) return 0;
  Double_t t = x[0]*TimeBin;
  Double_t tauI = par[0]; Double_t a = -1./tauI; //cout << "tauI\t" << tauI << "\ta\t" << a << endl;
  Double_t tauF = par[1]; Double_t d =  1./tauF; //cout << "tauF\t" << tauF << "\td\t" << d << endl;
  Double_t tauP = par[2]; Double_t b = -1./tauP; //cout << "tauP\t" << tauP << "\tb\t" << b << endl;
  //  Double_t L2 = Laplace2(t,b,d);
  Double_t L3 = Laplace3(t,a,b,d);
  //cout << "L2\t" << L2 << "\tL3\t" << L3 << endl;
  //  Double_t val = par[3]*L2 - par[4]*L3;
  Double_t val = L3;
  //cout << "val\t" << val << endl;
  return val;
}
//________________________________________________________________________________
void Shaper3D() {
  if (! shaperD) shaperD = new TF1("shaperD",shaper3D,-1,20,5);
  shaperD->SetParameters(100,500,2280,1,1);
}
//________________________________________________________________________________
void Shaper3() {
  Shaper3D();
}
//________________________________________________________________________________
Double_t shaper3T(Double_t *x, Double_t *par) {
  if (x[0] <= 0) return 0;
  Double_t t = x[0]*TimeBin;
  Double_t tauI = par[0];
  Double_t tauF = par[1];
  Double_t tauP = par[2];
  Double_t Val = 0;
  /* AP10[i]/(s+1/tauP10[i])) *(1/s - 1/(s + 1/tauI) * (s */
  for (Int_t i = 0; i < 3; i++) Val += AP10[i]*LI12(t,-1/tauP10[i],-1/tauI,-1/tauP,1/tauF);
  return Val;
}
//________________________________________________________________________________
void Shaper3T() {
  if (! shaperT) shaperT = new TF1("shaperT",shaper3T,-1,20,3);
  shaperT->SetParameters(100,500,2280);
}
//________________________________________________________________________________
Double_t shaper3F(Double_t *x,Double_t *par) {
  /* V(s) = (1 + s*tau_p)/(1 + s*tau_f)*(A0/(1 + s*tau0) + A1/(1 + s*tau1) + A2/(1 + s*tau2)) = 
      = A0*(1 + s*tau_p)/(1 + s*tau_f)/(1 + s*tau0) + => A0*L(t,tau_p,tau_f,tau0)
        A1*(1 + s*tau_p)/(1 + s*tau_f)/(1 + s*tau1) + => A1*L(t,tau_p,tau_f,tau1)
	A2*(1 + s*tau_p)/(1 + s*tau_f)/(1 + s*tau2)   => A2*L(t,tau_p,tau_f,tau2)
   */
  if (x[0] <= 0) return 0;
  Double_t t = x[0]*TimeBin;
  Double_t tau_f = par[0];
  Double_t tau_p = tauP10[2];
  return  par[1]*(
    AP10[0]*tauP10[0]*Laplace(t,tau_p,tau_f,tauP10[0]) +
    AP10[1]*tauP10[1]*Laplace(t,tau_p,tau_f,tauP10[1]) +
    AP10[2]*tauP10[2]*Laplace(t,tau_p,tau_f,tauP10[2]));
} 
//________________________________________________________________________________
Double_t Shaper3I(Double_t *x,Double_t *par) {
  // V(s) = (1 + s*tau_p)/(1 + s*tau_f)*(A0/(1 + s*tau0) + A1/(1 + s*tau1) + A2/(1 + s*tau2))
  
  if (! shaperF) shaperF = new TF1("shaperF",shaper3F,-100,200,2);
  shaperF->SetParameters(par);
  return shaperF->Integral(x[0]-0.5,x[0]+0.5);
} 
//________________________________________________________________________________
Double_t shaper3P(Double_t *x,Double_t *par) {// pulser step function with integrator
  /* V(s) = (1 + s*tau_P)/(1 + s*tau_F)*(1/s - tau_I/(1 + s*tau_I)) =
          = (1 + s*tau_P)/(1 + s*tau_F)*1/s  -               => Laplace1(t,-1/tau_P,-1./tau_F) -
	  = (1 + s*tau_P)/(1 + s*tau_F)*tau_I/(1 + s*tau_I)  => tau_I*L(t,tau_P,tau_F,tau_I)
	  par[0] = Norm;
	  par[1] = t0
	  par[2] = tau_I;
	  par[3] = tau_F;
	  par[4] = tau_P;
   */
  Double_t Norm  = par[0];
  Double_t t0    = par[1];
  Double_t tau_I = par[2];
  Double_t tau_F = par[3];
  Double_t tau_P = par[4];
  Double_t t     = x[0] - t0;
  if (t < 0) return 0;
  Double_t t = x[0]*TimeBin;
  return  Norm*(Laplace1(t,-1/tau_P,-1./tau_F) - tau_I*Laplace(t,tau_P,tau_F,tau_I));
} 
//________________________________________________________________________________
Double_t Shaper3IP(Double_t *x,Double_t *par) {
  if (! shaperP) shaperP = new TF1("shaperP",shaper3P,-100,200,5);
  shaperP->SetParameters(par);
  return shaperP->Integral(x[0]-0.5,x[0]+0.5);
} 
//________________________________________________________________________________
void Shaper3() {
#if 1
  if (! shaperI) shaperI = new TF1("shaperI",Shaper3I,-10,20,2);
  shaperI->SetParameters(500,10);
#else
  if (! shaperIP) shaperIP = new TF1("shaperIP",Shaper3IP,-10,20,5);
  shaperIP->SetParameters(1.,0.,10.,500,10);
#endif
}
//________________________________________________________________________________
