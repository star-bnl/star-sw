/*
 *  This file sets up all global variables that define the Trigger environment
 */

/* ----------------------------- REVISION LOG -----------------------------
 * $Header: /scratch/smirnovd/cvs2git_readonly/cvs/star-sw/StRoot/RTS/trg/include/Attic/trgenv.h,v 1.1 2008/02/27 16:32:48 fine Exp $
 * $Log: trgenv.h,v $
 * Revision 1.1  2008/02/27 16:32:48  fine
 * build RTS repository trg
 *
 * Revision 1.1.1.1  2006/06/02 00:14:21  nelson
 * Top level for new trg_soft_dev respositry
 *
 * Revision 1.2  2003/09/22 19:20:12  nelson
 * Change DEF_TRIGGER_ROOT to point to new location of cfg.  JMN
 *
 * Revision 1.1.1.1  2003/09/18 21:35:40  kopytin
 * Cleaned up version trg_soft_sep1303, no cfg
 *
 *     JMN: 14Feb00:  added startrg to path names
 * ------------------------------------------------------------------------ */



#ifndef _TRGENV_INDLUDED_

/*
 *  Preprocessor constant definitions
 */

#define CFG_DEFAULT_NM          "default"       /* startup configuration nm */
#define CFG_SUBTREE_NM          "cfg"           /* generic config subtree nm */

#define DEF_TRIGGER_ROOT        "/home/startrg/trg"
#define DEF_PRIVATE_ROOT        "."

/* for CTB map file */
#define DEF_MAP_DIR        "/home/startrg/trg/monitor/monfiles"

#define TRGENV_MAIN   /* put in 090699 - zm */
#ifdef TRGENV_MAIN
#define EXT extern
  char *trigger_root = DEF_TRIGGER_ROOT;
  char *private_root = DEF_PRIVATE_ROOT;
#else
#define EXT extern
  extern char *trigger_root;
  extern char *private_root;
#endif

/*#ifndef MAX_PATH_LEN
#define MAX_PATH_LEN            132
#else */
/*#ifeq MAX_PATH_LEN-132
  fix your code: MAX_PATH_LEN should be the same everywhere!!
#endif
#endif*/


/*
 *  variable definitions
 */

EXT char _trigger_root[MAX_PATH_LEN];                   /* /startrg */
EXT char _private_root[MAX_PATH_LEN];                   /* $USER/startrg */


/*
 *  function definitions
 */

int string_id (char *line, char *list[]);
int cnf_open_fd (char *name, char *object, char *path_used);
FILE *cnf_open  (char *name, char *object, char *path_used);


#undef EXT
#define _TRGENV_INCLUDED_
#endif

/* end of file trgenv.h */
