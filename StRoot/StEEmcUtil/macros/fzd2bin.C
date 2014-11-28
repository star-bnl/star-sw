#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;

//#define WRITE_SOCKET
#define WRITE_FILE

TBrowser       *b    = 0;
St_geant_Maker *geant= 0;
// reads .fzd file and writes EEevents to a binary file or socket

// ______________________________________________
void fzd2bin(const Int_t Nevents=5, const Char_t *fzfile ="muon.fzd",int socketID=9093) {
  Int_t  i=0;

  gSystem->Load("EEmc.so"); 
  gROOT->LoadMacro("bfc.C");
  bfc(0,"fzin sim_T gen_T",fzfile);

#ifdef WRITE_SOCKET
  cout<<"opening socket="<<socketID<<"\n";
  TServerSocket *ss = new TServerSocket(socketID, kTRUE);
  cout<<"waits for client1...\n";
  // Accept a connection and return a full-duplex communication socket.
  TSocket *s0 = ss->Accept();
  cout<<"waits 2...\n";
#endif

  for (i=1; i<=Nevents; i++ ) {
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    printf("%2d ====================================\n",i);
    St_g2t_ctf_hit *emc_hit = (St_g2t_ctf_hit *) chain->FindObject("g2t_eem_hit");
    if (emc_hit==0) continue;

#ifdef WRITE_SOCKET//  ...... use socket to transport data
    cout<<"sending this event via socket ...\n";
    // tell the clients to start
    EEmcMCData   data;
    const int mx=1<<14;
    char hitBuf[mx];
    int nh  = data.decode(emc_hit);
    cerr << "actual hits " << nh << endl;
    int nbw = ev->write(hitBuf,mx);
    int ret=s0->SendRaw(hitBuf,nbw);
    cout<<ret<<" char sent\n";
    if(ret!=nbw) {
      cerr<<"socekt Error1 "<<ret<<"  "<<nbw<<endl;
    }
#endif

#ifdef WRITE_FILE    //  ......  write events to bin file
    static FILE *fd=0;
    if(fd==0) {fd=fopen("data.bin","w"); assert(fd);}
    EEmcMCData   data;
    const int mx=1<<14;
    char hitBuf[mx];
    int nh  = data.decode(emc_hit);
    cerr << "actual hits " << nh << endl;
    int nbw = data->write(hitBuf,mx);
    char *cnbw=&nbw;
    int j;
    for(j=0;j<sizeof(int);j++) fputc(cnbw[j],fd);
    for(j=0;j<nbw;j++) fputc(hitBuf[j],fd);
#endif    

    // ..............   do sth with data locally
    EEuse1 jan(emc_hit);
    jan.work();


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
