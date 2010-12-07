#include "ExampleMain.h"

#include <qapplication.h>
#include "TGraph.h"
#include "JevpLogic.h"
#include "JevpGui.h"
#include "PresenterConnect.h"
#include "qtimer.h"


//#include "StJevpPool/StJevpUtils/EvpUtil.h"
//#include "TSystem.h"


//---- Main program ------------------------------------------------------
int ExampleMain::main()
{
  //int argc = 1;
  char *argv[2];
  argv[0] = (char *)"exampleMain";
  argv[1] = NULL;


  //QApplication *app = new QApplication(argc, argv);


//   Q3MainWindow *win = new Q3MainWindow();
//   win->show();
//   win->resize(600,500);

//   TQtWidget *MyWidget = new TQtWidget(NULL, "mywidget");


//   MyWidget->show();
//   MyWidget->GetCanvas()->cd();
//   TGraph *mygraph;
//   float x[3] = {1,2,3};
//   float y[3] = {1.5, 3.0, 4.5};
//   mygraph = new TGraph(3,x,y);
//   mygraph->SetMarkerStyle(20);
//   mygraph->Draw("AP");
//   MyWidget->GetCanvas()->Update();
//   MyWidget->resize(500,500);

  //app->exec();

  char name[100];
  int i=0;
  sprintf(name, "meggggggg_%d",i);

  TH1F *hist = new TH1F(name,name,64,0,64);
  TH1F *clone = (TH1F *)hist->Clone();
  for(;;) {

    TH1F *clone1 = (TH1F *)hist->Clone();
    
    printf("orig %s origclone %s clone %s\n",
	   hist->GetName(),
	   clone->GetName(),
	   clone1->GetName());

    delete hist;
   
    i++; 
    sprintf(name, "bad_%d",i);
    TH1F *hist2 = new TH1F(name,name,64,0,64);

    hist = clone1;
    delete hist2;
  }

  return 0;
}
