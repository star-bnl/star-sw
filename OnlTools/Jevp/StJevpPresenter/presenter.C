// Just calls EvpMain.main() so why does it exist?
// 
// -->  The STAR cons doesn't make executables.  This
//      executeable is compiled by the secondary Conscript
//      in StJevpPool
//
// -->  The STAR cons is smart enough to to search through
//      the source files for QT extensions, and call moc
//      if needed.   Thus, it's an easier secondary Conscript
//      file if the main program is in a library rather than
//      the executable...

#include "EvpMain.h"

int main(int argc, char *argv[])
{
  return EvpMain::main(argc,argv);
}


