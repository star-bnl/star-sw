#ifndef _PPDBLIB_H_
#define _PPDBLIB_H_

#include <RC_Config.h>

struct ctrStruct {
  int starttime;
  int endtime;
  int which;
  int cnt;
  int run;
};
    
void writeCfgDb(PP_CFG *cfg);
void updateCfgDb(int run, int endtime, int nevts);
void writeCounterDb(ctrStruct *ctrs, int n);
void writeFileDb(int run, char *filename, int size);
void writeTestDb(int val);
void *writeTests(void *);

#endif
