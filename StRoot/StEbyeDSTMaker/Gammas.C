//
// a macro containing useful function definitions
//  for doing pt fluctuation analysis
//
// by Jeff Reid and Tom Trainor
//

Double_t errorFix(Double_t *x ,Double_t *par) {

  Float_t q = par[0];
  Float_t e = 10**x[0];
  Float_t N = par[1];

  Float_t valA = (q*q)/(N*(q-1)*(q-1));
  Float_t valB = e;
  Float_t valC = (1-e)*((1/e)-1);
  Float_t valCp = (1/q)-((2*e)/(2*q+1));
  Float_t valD = (1+((1-q)/(1+q))*e)**2;

  return sqrt(valA*valB*(valC+valCp)/valD);

}

Double_t tomGamma(Double_t *x ,Double_t *par) {

  Float_t N0 = par[0];
  Float_t ptbar = par[1];
  Float_t sigmapt = par[2];
  Float_t nbar = par[3];
  Float_t powerO = par[4];

  Float_t power = 1/(1+(1-powerO));

  //  [1/T]  b = pt_bar/sigma_pt^2
  Float_t b = ptbar/(sigmapt*sigmapt);

  //  [n_0]  p = pt_bar^2/sigma_pt^2
  Float_t p = (ptbar/sigmapt)*(ptbar/sigmapt);

  Float_t n = nbar;

  Float_t nb=n*b;
  Float_t np=n*p;
  Float_t np1=(n*p)-1;

  //* pt-bar
  Float_t mu=np/nb;

  //* sigma-pt
  Float_t var=mu/nb;
  Float_t rms=sqrt(var);

  Float_t w = (x[0]*rms)+mu;
  Float_t wnew = w-mu;

  Float_t logf = log(nb)+(np-1)*log(nb*w)-(nb*w)-(np1*log(np1)-np1+log(sqrt(2*np1*3.1416)));
  return N0*exp(logf*power);

}

Double_t tomGauss(Double_t *x ,Double_t *par) {

  Float_t N0 = par[0];
  Float_t ptbar = par[1];
  Float_t sigmapt = par[2];
  Float_t nbar = par[3];
  Float_t power = par[4];

  //  [1/T]  b = pt_bar/sigma_pt^2
  Float_t b = ptbar/(sigmapt*sigmapt);

  //  [n_0]  p = pt_bar^2/sigma_pt^2
  Float_t p = (ptbar/sigmapt)*(ptbar/sigmapt);

  Float_t n = nbar;

  Float_t nb=n*b;
  Float_t np=n*p;
  Float_t np1=(n*p)-1;

  //* pt-bar
  Float_t mu=np/nb;

  //* sigma-pt
  Float_t var=mu/nb;
  Float_t rms=sqrt(var);

  Float_t w = (x[0]*rms)+mu;
  Float_t wnew = w-mu;

  return N0*exp(-(wnew*wnew)/(2*var));

}

Double_t tomPoly(Double_t *x ,Double_t *par) {

  Float_t N0 = par[0];
  Float_t ptbar = par[1];
  Float_t sigmapt = par[2];
  Float_t nbar = par[3];
  Float_t power = par[4];

  //  [1/T]  b = pt_bar/sigma_pt^2
  Float_t b = ptbar/(sigmapt*sigmapt);

  //  [n_0]  p = pt_bar^2/sigma_pt^2
  Float_t p = (ptbar/sigmapt)*(ptbar/sigmapt);

  Float_t n = nbar;

  Float_t nb=n*b;
  Float_t np=n*p;
  Float_t np1=(n*p)-1;

  //* pt-bar
  Float_t mu=np/nb;

  //* sigma-pt
  Float_t var=mu/nb;
  Float_t rms=sqrt(var);

  Float_t w = (x[0]*rms)+mu;
  Float_t wnew = w-mu;

  return N0*pow((1-(nb*wnew/np)*(nb*wnew/np)),(np/2));

}

Double_t deltaRef(Double_t *x ,Double_t *par) {

  Float_t smpt = 0.0150007;

  Float_t No = par[0];
  Float_t r = x[0]*smpt;
  Float_t np = (par[3]/2)*(par[1]/par[2]);

  return No*(pow((1-(r*r)),np));

}

Double_t gammaDist(Double_t *x ,Double_t *par) {

  Float_t smpt = 0.0150007;

  Float_t N0 = par[0];
  Float_t mpt = par[1];
  Float_t spt = par[2];
  Float_t Nbar = par[3];
  Float_t power = par[4];

  Float_t w = (x[0]*smpt)+mpt;

  Float_t b = mpt/(spt*spt);
  Float_t p = mpt*b;

  b *= Nbar;
  p *= Nbar;
  p -= 1;

  Float_t Ca = p-log(sqrt(2*3.1415926*p))-p*log(p)+log(b);

  return N0*exp((Ca+p*log(b*w)-(b*w))*(power));

}

Double_t TwoDgamma(Double_t *x ,Double_t *par) {

  Float_t smptx = 0.0165115;
  Float_t smpty = 0.0165115;

  Float_t N0 = par[0];

  Float_t mptx = par[1];
  Float_t sptx = par[2];
  Float_t Nbarx = par[3];

  Float_t mpty = par[4];
  Float_t spty = par[5];
  Float_t Nbary = par[6];

  Float_t w = x[0]*smptx+mptx;
  Float_t v = x[1]*smpty+mpty;

  Float_t bx = mptx/(sptx*sptx);
  Float_t px = mptx*bx;

  bx *= Nbarx;
  px *= Nbarx;
  px -= 1;

  Float_t by = mpty/(spty*spty);
  Float_t py = mpty*by;

  by *= Nbary;
  py *= Nbary;
  py -= 1;

  Float_t Cax = px-log(sqrt(2*3.1415926*px))-px*log(px)+log(bx);
  Float_t Cay = py-log(sqrt(2*3.1415926*py))-py*log(py)+log(by);

  return N0*exp(Cax+px*log(bx*w)-(bx*w))*exp(Cay+py*log(by*v)-(by*v));

}

