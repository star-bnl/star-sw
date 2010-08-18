#include <iomanip>
#include "heed++/code/HeedCluster.h"
//#include "heed++/code/HeedDeltaElectron.h"
//#include "heed++/code/HeedPhoton.h"
//#include "heed++/code/EnTransfCS.h"
//#include "wcpplib/clhep_units/WPhysicalConstants.h"
/*
2003, I. Smirnov
*/

void HeedCluster::print(ostream& file, int l) const
{
  if(l <= 0) return;
  Ifile<<"HeedCluster (l="<<l<<"): transferred_energy="<<transferred_energy
       <<" MeV, estimated_qel="<<estimated_qel<<'\n';
  Ifile<<"pt="<<pt<<"ptloc="<<ptloc<<'\n';
  if(l > 1)
  {
    indn.n+=2;
    absvol* av = tid.G_lavol();  
    if(av != NULL)
    {
      Ifile<<"av="; av->print(mcout, 1);
    }
    else
    {
      Ifile<<"volume is not registered\n";
    }
    Ifile<<"natom="<<natom<<" nshell="<<nshell<<'\n';
    indn.n-=2;
  }
}

