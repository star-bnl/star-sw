/* 
   foreach d (`ls -alF *.event.root | awk -F_ '{print $3}' | cut -c3-5 | sort -u`) 
     root.exe -q -b Load.C 'Recover.C("st_physics_15'${d}'*.event.root")' >& ${d}.log &
   end

   foreach d (`ls -alF *.MuDst.root | awk -F_ '{print $4}' | cut -c3-5 | sort -u`) 
     root.exe -q -b lMuDst.C 'Recover.C("st_physics_15'${d}'*.MuDst.root")' >& ${d}.log &
   end
   foreach f ( `grep Zombie Recover.log | awk '{print $3}'` )
     set b = `basename ${f} .MuDst.root`; mv ${b}* Zombie/
   end
   foreach f ( `grep Zombie Recover.log | awk '{print $3}'` )
     set b = `basename ${f} .PicoDst.root`; mv ${b}* Zombie/
   end
 */
#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
#include "Riostream.h"
#include "TDirIter.h"
#include "TFile.h"
#endif
void Recover(const Char_t *files ="./*.MuDst.root", Bool_t update = kTRUE) {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TFile *f = 0;
  //  gEnv->SetValue("TFile.Recover", 0); // don't recover files
  while ((file = (Char_t *) Dir.NextFile())) {
    cout << "Try to open " << file << endl;
    if (update)  f = new TFile(file,"update");// 
    else         f = new TFile(file);
    cout << "File\t" << NFiles <<"\t" << file; 
    if (! f)                cout << " Can't be opened"; 
    else if (f->IsZombie()) cout << " is Zombie";
    else                    cout << " is o.k.";
    cout << endl;
    delete f;
    NFiles++;
  }
}
