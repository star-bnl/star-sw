#include <cmath>
#include <iomanip>
#include "wcpplib/util/FunNameStack.h"
#include "heed++/code/EnergyMesh.h"


EnergyMesh::EnergyMesh(double femin, double femax, long fq):
  q(fq), emin(femin), emax(femax)
{
  mfunname("EnergyMesh::EnergyMesh(double femin, double femax, long fq)");
  check_econd21( q, < 0 || , > pqener - 1 , mcerr);

  double rk = pow(emax/emin,(1.0/double(q)));
  double er=emin;
  e[0]=er;
  for(long n=1; n<q+1; n++)
  {
    e[n] = er * rk;
    ec[n-1] = (e[n-1] + e[n]) * 0.5;
    er=e[n];
  }
  //pcm_e = PointCoorMesh< double, double[pqener] >( q, &e);
  //pcm_ec = PointCoorMesh< double, double[pqener-1] >( q-1, &ec);

  //mcout<<"EnergyMesh is done"<<endl;
}

EnergyMesh::EnergyMesh(DynLinArr< double > fec):
  q(fec.get_qel())
{
  mfunname("DynLinArr< double > fec");
  check_econd21( q, < 0 || , > pqener - 1 , mcerr);
  check_econd11( q, != 1 , mcerr); // otherwise problems with emin/emax 
  if(q > 0)
  {
    emin = fec[0]   - (fec[1]   - fec[0]  ) / 2.0;
    emax = fec[q-1] + (fec[q-1] - fec[q-2]) / 2.0;
    e[0] = emin;
    e[q] = emax;
    
    for(long n=0; n<q; n++)
    {
      ec[n] = fec[n];
    }
    for(long n=1; n < q; n++)
    {
      e[n] = 0.5 * (fec[n-1] + fec[n]);
    }
  }
  else
  {
    emin = 0.0;
    emax = 0.0;
  }
  //pcm_e = PointCoorMesh< double, double[pqener] >( q, &e);
  //pcm_ec = PointCoorMesh< double, double[pqener-1] >( q-1, &ec);
}

/*
EnergyMesh::EnergyMesh(const EnergyMesh& fem)
{
  *this = fem;
}

EnergyMesh& EnergyMesh::operator=(const EnergyMesh& fem)
{
  q = fem.q;
  emin = fem.emin;
  emax = fem.emax;
  long n;
  for(n=0; n<q+1; n++)
  {
    e[n] = fem.e[n];
  }
  for(n=0; n<q; n++)
  {
    ec[n] = fem.ec[n];
  }
  pcm_e = PointCoorMesh< double, double[pqener] >( q, &e);
  pcm_ec = PointCoorMesh< double, double[pqener-1] >( q-1, &ec);
}
*/

long EnergyMesh::get_interval_number(double ener)
{
  if(ener < emin) return -1;
  if(ener > emax) return q;
  
  long n1=0; 
  long n2=q;  // right side of last
  long n3 = n1;
  while( n2-n1 > 1 )
  {
    n3=n1 + (n2-n1)/2;
    if(ener < e[n3])
      n2=n3;
    else
      n1=n3;
  }
  return n1;
}

long EnergyMesh::get_interval_number_between_centers(double ener)
{
  if(ener < ec[0]) return -1;
  if(ener > ec[q-1]) return q;
  
  long n1=0; 
  long n2=q-1;  // right side of last
  long n3 = n1;
  while( n2-n1 > 1 )
  {
    n3=n1 + (n2-n1)/2;
    if(ener < ec[n3])
      n2=n3;
    else
      n1=n3;
  }
  return n1;
}
  

ostream& operator<<(ostream& file, EnergyMesh& f)
{
  Ifile<<"EnergyMesh: \n";
  indn.n+=2;
  Ifile<<"emin="<<f.emin<<" emax="<<f.emax<<
    " quantity of intervals="<<f.q<<'\n'<<
    " maximal possible quantity of intervals="<<pqener<<'\n';
  Ifile<<
" number  left side        center       right side       widht\n";
  for(int n=0; n<f.q; n++)
    Ifile<<setw(5)<<n<<setw(15)<<f.e[n]<<setw(15)<<f.ec[n]<<
      setw(15)<<f.e[n+1]<<setw(15)<<(f.e[n+1] - f.e[n])<<'\n';
  //f.pcm_e.print(mcout);
  //f.pcm_ec.print(mcout);
  indn.n-=2;
  return file;
}

void EnergyMesh::print(ostream& file, int l) const
{
  if(l <= 0 ) return;
  Ifile<<"EnergyMesh (l="<<l<<"): \n";
  indn.n+=2;
  Ifile<<"emin="<<emin<<" emax="<<emax<<
    " quantity of intervals="<<q<<'\n'<<
    " maximal possible quantity of intervals="<<pqener<<'\n';
  if(l > 1 )
  {
    Ifile<<
      " number  left side        center       right side       widht\n";
    for(int n=0; n<q; n++)
      Ifile<<setw(5)<<n<<setw(15)<<e[n]<<setw(15)<<ec[n]<<
	setw(15)<<e[n+1]<<setw(15)<<(e[n+1] - e[n])<<'\n';
  }
  indn.n-=2;
}

DynLinArr< double > make_log_mesh_ec(double emin, double emax, long q)
{
  mfunname("DynLinArr< double > make_log_mesh_ec(double emin, double emax, long q)");

  double rk = pow(emax/emin,(1.0/double(q)));
  double er = emin;
  DynLinArr< double > ec(q);
  double e1 ;
  double e2 = er;
  for(long n=0; n<q; n++)
  {
    e1 = e2;
    e2 = e2 * rk;
    ec[n] = (e1 + e2) * 0.5;
  }
  return ec;
}
  
