/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         ami_types.h
*:DESCRIPTION:  Variable types for AMI
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef AMI_TYPES_H
#define AMI_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "dstype.h"
#include "tdmClasses.hh"
#include "ami_macros.h"
#include "table_header.h"

#ifdef CORBA
#include "ami_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/
#ifndef CORBA

#ifdef __cplusplus
typedef struct table_seq_t {
   unsigned long _maximum;
   unsigned long _length;
   tdmTable ** _buffer;
}TABLE_SEQ_T;
#endif /*__cplusplus*/

#endif /*CORBA*/

/*-------------------------------------------- TYPEDEFS             --*/
#ifndef CORBA

#endif /*CORBA*/

typedef long FNC_PTR_T;

typedef enum ami_io_mode_t {
        AMI_INPUT_MODE,
        AMI_OUTPUT_MODE,
        AMI_UPDATE_MODE
}AMI_IO_MODE_T;


/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int ami_init();
extern CC_P int ami_start();
extern CC_P int ami_stop();

/*- Utility functions. -*/
extern CC_P STAFCV_T ami_pamSwitch(long rank, FNC_PTR_T ftn
		, TABLE_HEAD_ST **h, char **d);

#ifndef NOKUIP
extern CC_P void ami_def_();

extern CC_P void kam_ami_call_();
extern CC_P void kam_ami_count_();
extern CC_P void kam_ami_list_();
extern CC_P void kam_amimodule_init_();
extern CC_P void kam_amimodule_rank_();
extern CC_P void kam_amimodule_call_();
extern CC_P void kam_amimodule_show_();
extern CC_P void kam_amimodule_start_();
extern CC_P void kam_amimodule_stop_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T ami_call(char* name,long ntabs,char **tnames);
extern CC_P STAFCV_T ami_count();
extern CC_P STAFCV_T ami_list();
extern CC_P STAFCV_T amimodule_init(char* name);
extern CC_P STAFCV_T amimodule_rank(char* name);
extern CC_P STAFCV_T amimodule_show(char* name);
extern CC_P STAFCV_T amimodule_start(char* name);
extern CC_P STAFCV_T amimodule_stop(char* name);

#endif /* AMI_TYPES_H */

