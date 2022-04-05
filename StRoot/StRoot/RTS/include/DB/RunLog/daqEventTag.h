/*    daqEventTag.h
*
*  Table: daqEventTag
*
*       description: //  Daq input to TagDb
*/ 

#ifndef __daqEventTag__H
#define __daqEventTag__H

struct daqEventTag { 

  unsigned int  run;              /* run number  */
  unsigned int  fileSequence;     /* index portion of filename */
  unsigned short fileStream;      /* new in version 10.0  stream portion of filename */
  unsigned short isCopy;            /* new in version 10.0  is this a event a copy? */
  unsigned int  eventNumber;      /* monotonically increases in every file */
  unsigned short token;
  unsigned short physWord;        /* new in version 10.0  physics word */

  /* unsigned int  offset;           removed in version 10.0 */
  unsigned int  size;             /* in bytes */
  unsigned int  time;             /* Time built by EVB */
  unsigned short  trgWord;        
  unsigned short  trg_cmd;        /* From trgActionWord  */
  unsigned short  daq_cmd;        /* From trgActionWord  */
  unsigned short  detector_bits;  /* From trgActionWord  */
  unsigned int  bx_hi;            /* RHIC bunch crossing (hi word)  */
  unsigned int  bx_lo;            /* RHIC bunch crossing (lo word)  */
  /* unsigned int l3_accept_flags;   removed in version 10.0 */
  /* unsigned int l3_build_flags;    removed in version 10.0 */
  /* unsigned int l3_on_flags;       removed in version 10.0 */

  unsigned int l3_nTracks;
  unsigned int trg_add_bits;      /* was any pileup detected  */
  unsigned int lastDSM;

  unsigned long long int l1_trg_bits;       /* new in version 10.0  triggers satisfied after L1 */
  unsigned long long int l2_trg_bits;       /* new in version 10.0  triggers satisfied after L2 */
  unsigned long long int l3_trg_bits;       /* new in version 10.0  triggers satisfied after L3 */
};  


#endif 
