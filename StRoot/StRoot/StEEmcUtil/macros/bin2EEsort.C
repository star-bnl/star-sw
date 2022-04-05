#include <iostream.h>

class    EEmcMCData;

// ______________________________________________
void bin2EEsort(int Nevents=1, const Char_t *binFile ="data.bin"){
  Int_t  i=0;
  gSystem->Load("EEmc.so"); 

  FILE *fd=0;
  fd=fopen(binFile,"r");
  assert(fd);
  
  for (i=1; i<=Nevents; i++ ) {
    const int mx=1<<14;
    char hitBuf[mx];
    int mxh=sizeof(int);
    int j,len;
    // get buf len
    char *cLen=&len;
    for(j=0;j<sizeof(int);j++) cLen[j]=fgetc(fd);
    cerr << "actual data len=" << len << " bytes " << endl;
    // get data body
    for(j=0;j<len;j++) hitBuf[j]=fgetc(fd);

   // ..............   do sth with data locally
    EEmcMCData   data;
    int nbw = data.read(hitBuf,len);
    data.print();

  }
}



#if 0

  EEmcMCData      *ev     = new EEmcMCData;
  EEmcMCData      *evcopy = new EEmcMCData;

  char *hitBuf = new char[16384];



     int nh  = ev->decode(emc_hit);
      cerr << "actual hits " << nh << endl;
      ev->print();
      int nbw = ev->write(hitBuf,4096);
      cerr << "actual writ " << nbw << " bytes " << endl;
      int nbr = evcopy->read(hitBuf,nbw);
      cerr << "actual read " << nbr << " bytes " << endl;
      evcopy->print();
#endif
