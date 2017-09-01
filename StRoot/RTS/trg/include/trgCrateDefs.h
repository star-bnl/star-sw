/******
*
*     Layout of DSM Crates
*
*     J.M. Nelson       9 August 2017
*
*****************************************************************************/
#define LAYOUT_VERSION        0x017080901     /* Format: yymmddvv */

typedef struct {
  char name[4];                               /* Contains INF with confNum 20 */
  int length;                                 /* Byte count of data that follows */
  int layout_version;                         /* Layout version */
  int runNumber;                              /* This block will be in a file read by L2 at config time */
  int timeStamp;                              /* It may carry additional information as needed */
  unsigned int data[10];                      /* 40 bytes available for use */
} INFBlock; 

typedef struct  {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} DSMBlock;

typedef struct  {
  char name[4];
  int length;                                 /* Byte count of data that follows */
  int dataLoss;                               /* Byte count of data truncated due to buffer limitations */
  unsigned int data[1];                       /* NB: this definition is generic but would vary depending on actual data */
} QTBlock;
