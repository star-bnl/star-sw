#ifndef CONFIGLIB_H
#define CONFIGLIB_H

#include "iccp.h"
#include "RC_Config.h"

/* -1 on error, 0 on success */
int getConfigFile(STAR_CFG *c, ic_msg *m);
int getDaqConfigFile(DAQ_CFG *c, ic_msg *m);
int getL3ConfigFile(L3_CFG *c, ic_msg *m);
int getScConfigFile(SC_CFG *c, ic_msg *m);
int getTrgConfigFile(TRG_CFG *c, ic_msg *m);
#endif
