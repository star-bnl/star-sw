#include "example1.h"
#include "TQGsiRootCanvas.h"
#include "qtroot.h"
#include "TBrowser.h"

//_____________________________________________________
example1::example1()
{
   // Make sure ROOT env is up
   TQtWidget::InitRint();
}
//_____________________________________________________
void example1::execute(int mode)
{
    if ( mode > 1 ) showGuiTest();
    if ( mode > 0 ) showBrowser();
    showApplication();
}
//_____________________________________________________
void example1::showBrowser()
{
    new TBrowser();
}
//_____________________________________________________
void example1::showApplication()
{
   ApplicationWindow * mw = new ApplicationWindow();
   mw->setCaption( "Qt & ROOT" );
   mw->show();
}
