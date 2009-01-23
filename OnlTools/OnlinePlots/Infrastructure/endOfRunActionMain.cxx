#include "endOfRunActionMain.h"
#include "EvpServer.h"
#include "EndOfRunAction.h"

//_____________________________________________________________
int endOfRunActionMain::main(int argc, const char* argv[]) {

  EvpUtil::ReadCanvasDefinitions();

  TCanvas* cc = new TCanvas("cc","cc",EvpUtil::mCanvasWidth, EvpUtil::mCanvasHeight);
  cout << " running endOfRunAction on file : " << argv[1] << endl;
  cout << " running endOfRunAction on file : " << argv[1] << endl;
  cout << " running endOfRunAction on file : " << argv[1] << endl;
  EndOfRunAction* action = new EndOfRunAction(argv[1],EvpUtil::GetOutputPath(),cc);
  action->Action();
  cout << " endOfRunAction on file : " << argv[1] << " done " << endl;

  return 0;
}


/***************************************************************************
 *
 * $Id: endOfRunActionMain.cxx,v 1.1 2009/01/23 16:11:02 jeromel Exp $
 *
 * Author: Valeri Fine , fine@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: endOfRunActionMain.cxx,v $
 * Revision 1.1  2009/01/23 16:11:02  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2008/03/17 22:58:46  fine
 * Add the 3 new classes with the static main method
 *
 *
 ***************************************************************************/

