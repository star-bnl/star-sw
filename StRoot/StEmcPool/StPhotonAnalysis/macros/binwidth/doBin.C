Double_t doBin(Double_t x,Double_t d)
{
  Double_t alpha=9.125;
  //right
  x+=d/2.;
  cout<<"r: "<<x<<endl;
  Double_t retR=x*(1./(1.-alpha)) * pow(1.+x,1.-alpha);
  cout<<retR<<endl;
  retR=retR - (1./((1.-alpha)*(2.-alpha)))*pow(1.+x,1.-alpha); 
  cout<<retR<<endl;
  //left
  x-=d;
  cout<<"l: "<<x<<endl;
  Double_t retL=x*(1./(1.-alpha)) * pow(1.+x,2.-alpha);
  cout<<retL<<endl;
  retL=retL - (1./((1.-alpha)*(2.-alpha)))*pow(1.+x,2.-alpha); 
  cout<<retL<<endl;
  //ratio
  x+=d/2.;
  cout<<"c: "<<x<<endl;
  Double_t pqcd=pow(1.+x,-1.0*alpha);
  pqcd/=(retR-retL);
  return pqcd*d*x;
}
