#include "heed++/code/HeedCondElectron.h"
/*
2003, I. Smirnov
*/

void HeedCondElectron::print(std::ostream& file, int l) const
{
  if (l <= 0) return;
  Ifile<<"HeedCondElectron (l=" << l << ")\n";
  //Ifile << "pt=" << pt << "ptloc=" << ptloc << '\n';
  Ifile << "ptloc=" << ptloc << '\n';
  /*
  if (l > 1) {
    indn.n += 2;
    absvol* av = tid.G_lavol();  
    if (av != NULL) {
      Ifile << "av="; av->print(mcout, 1);
    } else {
      Ifile << "volume is not registered\n";
    }
    HeedDeltaElectron* de = parent_de.get();
    if (de != NULL) {
      //Ifile << "parent particle: particle_number="
      //      << de->particle_number << '\n';
      Ifile << "parent particle: ";
      de->print(file, 1);
    } else {
      Ifile << "no parent particle\n";
    } 
    indn.n -= 2;
  }
  */
}
