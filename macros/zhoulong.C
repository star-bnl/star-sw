#include "Riostream.h"
#include "TString.h"
#include "string.h"
using namespace std;
void zhoulong(Char_t *FileName="/star/u/zhoulong/data02/DEV14/SST/offline_12_02/testV3.txt") {
  FILE *fp = fopen(FileName,"r");
  char line[121];
  Int_t i = 0, j = 0;
  while (fgets(&line[0],120,fp)) {
    //    cout << "|" << line;
    TString Line(line);
    if (Line.Length() == 1 ) continue;
    Line.Strip(TString::kTrailing,'\n');
    cout << Line.Data();
    if (Line.BeginsWith("#")) {
      if (Line.Contains("Event")) cout << "New Event" << endl;
      else if (Line.Contains("Block")) cout << "New Block" << endl;
      continue;
    }
    sscanf(&line[0] ,"%x",&j);
    cout << "Read " << j << endl;
    i++;
    if (i > 100) break;
  }
  fclose(fp);
}
