#include "JevpServerMain.h"
#include "DisplayDefs.h"

void _JevpServerMain(int argc, char *argv[]);

ClassImp(JevpServerMain);

void JevpServerMain::main(int argc, char *argv[])
{
  _JevpServerMain(argc, argv);
}

void JevpServerMain::testDisplayDef(char *args)
{
  DisplayFile::test(args);
}
