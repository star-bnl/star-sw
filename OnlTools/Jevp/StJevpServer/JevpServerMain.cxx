#include "JevpServerMain.h"
#include "DisplayDefs.h"

void _JevpServerMain(int argc, char *argv[]);

ClassImp(JevpServerMain);

void JevpServerMain::main(int argc, char *argv[])
{
    printf("ARGC = %d\n", argc);

    if((argc == 3) && (strcmp(argv[1], "-testdd")==0)) {
	printf("testdd:\n");
	testDisplayDef(argv[2]);
	return;
    }

    _JevpServerMain(argc, argv);
}

void JevpServerMain::testDisplayDef(char *args)
{
  DisplayFile::test(args);
}
