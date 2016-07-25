#include "Riostream.h"
//************** EXPONENTIAL INTEGRALS En *****
  // define: E_n(x) = \int_1^infty{exp(-xt)/t^n}dt, x>0, n=0,1,...
double expint(int n, double x) {
  // based on Numerical Recipes in C
  const double euler = 0.57721566; // Euler's constant, gamma
  const int maxit = 100;           // max. no. of iterations allowed
  const double fpmin = 1.0e-30;    // close to smallest floating-point   number
  const double eps = 6.0e-8;       // relative error, or absolute error near
  // the zero of Ei at x=0.3725
  
  int i, ii, nm1;
  double a,b,c,d,del,fact,h,psi,ans;
  
  nm1=n-1;
  if(n<0 || x<0 || (x==0 && (n==0 || n==1))) {
    cout << "Bad argument for expint(n,x)" << endl; return -1;
  }
  else {
    if(n==0) ans=exp(-x)/x;
    else {
      if(x==0) ans=1.0/nm1;
      else {
	if(x>1) {
	  b=x+n;
	  c=1.0/fpmin;
	  d=1.0/b;
	  h=d;
	  for(i=1; i<maxit; i++) {
	    a = -i*(nm1+i);
	    b += 2.0;
	    d=1.0/(a*d+b);
	    c=b+a/c;
	    del=c*d;
	    h *= del;
	    if(fabs(del-1.0)<eps) {
	      ans=h*exp(-x);
	      return ans;
	    }
	  }
	  cout << "***continued fraction failed in expint(n,x)!!!" << endl;
	  return -1;
	} else {
	  ans = (nm1!=0 ? 1.0/nm1 : -log(x)-euler);
	  fact=1;
	  for(i=1; i<=maxit; i++) {
	    fact *= -x/i;
	    if(i!=nm1) del = -fact/(i-nm1);
	    else {
	      psi = -euler;
	      for(ii=1; ii<=nm1; ii++) psi += 1.0/ii;
	      del = fact*(-log(x)+psi);
	    }
	    ans += del;
	    if(fabs(del)<fabs(ans)*eps) return ans;
	  }
	  cout << "***series failed in expint!!!" << endl;
	  return -1;
	}
      }
    }
  }
  
  return ans;
}
//************** EXPONENTIAL INTEGRAL Ei ******
  // define: ei(x) = -\int_{-x}^{\infty}{exp(-t)/t}dt,  for x>0
  // power series: ei(x) = eulerconst + ln(x) + x/(1*1!) + x^2/(2*2!) + ...
double ei(double x)
{ // taken from Numerical Recipes in C
  const double euler = 0.57721566; // Euler's constant, gamma
  const int maxit = 100;           // max. no. of iterations allowed
  const double fpmin = 1.0e-40;    // close to smallest floating-point number
  const double eps = 1.0e-30;       // relative error, or absolute error  near
                                    // the zero of Ei at x=0.3725
  //  I actually changed fpmin and eps into smaller values than in NR
  
  int k;
  double fact, prev, sum, term;
  
  // special case
  if(x < 0) return -expint(1,-x);
  
  if(x == 0.0) { cout << "Bad argument for ei(x)" << endl; return -1; }
  if(x < fpmin) return log(x)+euler;
  if(x <= -log(eps)) {
    sum = 0;
    fact = 1;
    for(k=1; k<=maxit; k++) {
      fact *= x/k;
      term = fact/k;
      sum += term;
      if(term < eps*sum) break;
    }
    if(k>maxit) { cout << "Series failed in ei(x)" << endl; return -1; }
    return sum+log(x)+euler;
  } else {
    sum = 0;
    term = 1;
    for(k=1; k<=maxit; k++) {
      prev = term;
      term *= k/x;
      if(term<eps) break;
      if(term<prev) sum+=term;
      else {
	sum -= prev;
	break;
      }
    }
    return exp(x)*(1.0+sum)/x;
  }
}
//*********************************************
//*********************************************
