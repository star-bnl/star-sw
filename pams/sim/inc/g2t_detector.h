/* This include file contains the definition of the detector structure.
*/
/* revised 961204, ge */
/* revised 970502, ge: n_detectors to 7, n_tables to 11 */

typedef struct detector
    {
    char        name[32];               /* name of detector */
    int         l_detector;             /* label identifying detector */
    int         nsub;                   /* # of subdetectors */
    struct      detector    *sub_det;   /* pointer to array of subdetectors */
    } detector;

#define         N_DETECTORS     7

#define         TPC             0
#define         EMB             1
#define         CTF             2
#define         VPD             3
#define         SVT             4
#define         FTP             5
#define         ECP             6

#define         NSEC_TPC        24
#define         NPS_TPC         45
#define         NPA_TPC         13

/***************************************************************************/
#include        "dstype.h"

typedef struct hit_table
    {
    char        name[64];               /* name of table */    
    int         is_on;                  /* fill table or not */
    int         idet;                   /* detector number */
    int         nsdet;                  /* number of subdetectors */
    int         isfirst;                /* first subdetector */
    int         n_hits;                 /* total number of hits */
    DS_DATASET_T    *HitDs_p;           /* dataset pointer */
    } hit_table;

#define         N_TABLES       11

#define         TPC_HIT         0
#define         MWC_HIT         1
#define         EMC_HIT         2
#define         SMD_HIT         3
#define         CTB_HIT         4       /* CTF to CTB, 961204 ge */
#define         TOF_HIT         5       /* added     , 961204 ge */
#define         VPD_HIT         6
#define         SVT_HIT         7
#define         FTP_HIT         8
#define         EEM_HIT         9       /* added       970502 ge */
#define         ESM_HIT        10       /* added       970502 ge */
