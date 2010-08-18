#include "heed++/code/BGMesh.h"

BGMesh::BGMesh(double fxmin, double fxmax, long fq):
  xmin(fxmin), xmax(fxmax), q(fq)
{
  x = make_log_mesh(fxmin, fxmax, fq);
}

void BGMesh::print(ostream& file, int l) const
{
  if(l <= 0 ) return;
  Ifile<<"BGMesh (l="<<l<<"): \n";
  indn.n+=2;
  Ifile<<"xmin="<<xmin<<" xmax="<<xmax<<
    " quantity of intervals="<<q<<'\n';
  if(l > 1 )
  {
    Iprintn(mcout, x);
  }
  indn.n-=2;
}

ostream& operator<<(ostream& file, const BGMesh& bgm)
{
  Ifile<<"operator<<(ostream& file, const BGMesh& bgm):\n";
  bgm.print(file, 2);
  return file;
}


DynLinArr< double > make_log_mesh(double fxmin, double fxmax, long fq)
{
  mfunname("DynLinArr< double > make_log_mesh(double fxmin, double fxmax, long fq)");

  check_econd11(fq , <= 1 , mcerr); // minimum one interval and two points

  double rk = pow(fxmax/fxmin,(1.0/double(fq-1)));
  
  DynLinArr< double > x(fq);
  x[0] = fxmin;
  x[fq-1] = fxmax;
  
  double xr = fxmin;
  for(long n=1; n<fq-1; n++)
  {
    xr = xr * rk;
    x[n] = xr;
  }
  return x;
}
