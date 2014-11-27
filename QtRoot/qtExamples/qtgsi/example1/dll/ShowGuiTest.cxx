#include "example1.h"
#include "guitest.h"

//_____________________________________________________
void example1::showGuiTest()
{ 
    new TestMainFrame( gClient->GetRoot(), 400, 220);  
}
