#include "EvpPresenter.h"
#include "PresenterGui.h"
#include "PresenterConnect.h"
#include "evpMainPresenter.h"
#include "qapplication.h"


#include "EvpUtil.h"
#include "TSystem.h"

//---- Main program ------------------------------------------------------
int evpMainPresenter::main(int argc, char **argv )
{


  cout << "command line arguments are: " << endl;
  for ( int i=0; i<argc; i++) {
    cout << i << " :   " << argv[i] << endl;
  }
  cout << endl;

  if ( argc==2 ) {
    EvpUtil::mMapFilePath = argv[1];
    cout << " shared memory file set to " << EvpUtil::mMapFilePath << endl;
  }

     TMapFile* mfile = TMapFile::Create(EvpUtil::mMapFilePath,"READ",EvpUtil::mSharedMemorySize);

    PresenterGui* gui = new PresenterGui();

    TSeqCollection* col = gSystem->GetListOfFileHandlers();
    TIter next(col);
    TFileHandler* o=0;
    while ( o=(TFileHandler*) next() ) {
      cout << o->GetFd() << endl;
      if ( o->GetFd()==0 ) {
	o->Remove();
	break;
      }
    }
    
    gui->resize(500,500);
    gui->show();

    

    EvpUtil::ReadCanvasDefinitions();
    

    EvpPresenter* presenter = new EvpPresenter();
    PresenterConnect* con = new PresenterConnect(gui,presenter);
    
    //presenter->run();

//    delete presenter;
//    delete con;
//    cout << "good bye " << endl;
    return 0;
}
