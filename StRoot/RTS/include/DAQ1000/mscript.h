#ifndef _MSCRIPT_H__
#define _MSCRIPT_H_

/* Definitions of the miniscript */

/* Tonko: includes all the command + ALTRO register defines */
#include <DAQ1000/rdo_cmds.h>


/* Registers for nios listprocessor */
#define MINI_RUN_NUMBER 1        /* run tag */
#define MINI_ID		2        /* id tag */



#if defined(linux) || defined(__APPLE__)
/* prototypes for compiler, not seen by nios code */

/* Returns length of opcode buffer */
/* list is the opcode buffer */
/* str is a NULL delimited single command */

#define MAX_MSCRIPT_LEN 1000

struct ParsedScript 
{
  char _fn[256];                 // optional filename
  char *fn;                      // null if no file, or points to _fn[]
  int num;                     // numeric arg  0xffffffff by default

  uint32_t data[MAX_MSCRIPT_LEN];   // compiled script
  uint32_t n;                       // compiled script length
};

int parse(ParsedScript *script, char *str, int batch=0);

#else

/* Nios */
int runList(uint32_t *prog, char *outputbuff, uint32_t sz);
#endif



#endif
