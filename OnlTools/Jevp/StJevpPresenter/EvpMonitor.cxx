#include "EvpMonitor.h"
#include <stdlib.h>
#include <TSocket.h>
#include <TMessage.h>
#include <Jevp/StJevpPlot/EvpMessage.h>
EvpMonitor *evpMonitor;

int EvpMonitor::main(char *args) 
{
  printf("This main...\n");
  char *argv[50];
  int argc=0;

  argv[0] = strtok(args, " ");
  argc++;

  char *next;
  while((next = strtok(NULL, " "))) {
    argv[argc] = next;
    argc++;
  } 

  return _main(argc, argv);
}


//---- Main program ------------------------------------------------------
int EvpMonitor::_main(int argc, char **argv )
{
  evpMonitor = new EvpMonitor();   // store globals here...
  if(evpMonitor->parseArgs(argc, argv) < 0) return 0;

  TSocket *socket;
  socket = new TSocket(evpMonitor->server, evpMonitor->serverport);
  if(!socket->IsValid()) {
    printf("Can't connect to (%s:%d)\n",evpMonitor->server, evpMonitor->serverport);
    return -1;
  }

  EvpMessage m;
  m.setSource("monitor");
  m.setCmd("monitor");
  m.setArgs(evpMonitor->serverCommand);

  printf("Sending request (%s) to server\n", evpMonitor->serverCommand);

  TMessage mess(kMESS_OBJECT);
  mess.WriteObject(&m);
  socket->Send(mess);
    
  TMessage *rmess;
  int ret = socket->Recv(rmess);
  if(ret == 0) {
    printf("Error recieving from server\n");
    return -1;
  }
  
  EvpMessage *response = (EvpMessage *)rmess->ReadObject(rmess->GetClass());
  
  printf("---------- response ------------------\n");
  printf("%s\n",(response->args) ? response->args : "none");
  printf("--------------------------------------\n");

  socket->Close();  
  return 0;
}

int EvpMonitor::parseArgs(int argc, char *argv[])
{
  if(!argv) return 0;
  if(!argv[0]) return 0;

  for(int i=0;i<argc;i++) {
    if (memcmp(argv[i], "-server", 7) == 0) {
      i++;
      server = argv[i];
    }
    else if (memcmp(argv[i], "-port", 5) == 0) {
      i++;
      serverport = atoi(argv[i]);
    }
    else if (strcmp(argv[i], "-test") == 0) {
      serverport = JEVP_PORT + 10;
    }
    else if (argv[i][0] != '-') {  // append the rest to server args...
      char *s = evpMonitor->serverCommand;
      *s = '\0';
      while(i < argc) {    
	char *a = argv[i];
	while(*a) {
	  *s++ = *a++;
	}
	*s++ = ' ';
	i++;
      }
      s--;
      *s = '\0';
    }
    else {
      printf("%s arguments\n\t-server servername\n\t-port port\n",argv[0]);
      return -1;
    }    
  }
  return 0 ;
}

#if 0
  cout << "command line arguments are: " << endl;
  for ( int i=0; i<argc; i++) {
    cout << i << " :   " << argv[i] << endl;
  }
  cout << endl;



  printf("presenter gui\n");

  PresenterGui* gui = new PresenterGui();
  sleep(2);
  printf("presenter\n");
    

  EvpPresenter* presenter = new EvpPresenter();
  PresenterConnect* con = new PresenterConnect(gui,presenter);
  sleep(2);
  printf("connect\n");

  presenter->Connect();

  printf("resize\n");

  gui->resize(500,500);
  gui->show();
  printf("here\n");
  sleep(2);
  printf("show\n");


  // Everything is ready. It is safe to fire the event loop now !

  printf("single shot\n");
  
//  QTimer::singleShot (0,gui,SLOT(GetNextEvent()));

  for(;;) {
    sleep(1);
  }

  printf("Done\n");
  //    delete presenter;
  //    delete con;
  //    cout << "good bye " << endl;
  return 0;
}

#endif
