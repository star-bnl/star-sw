/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dio_types.h
*:DESCRIPTION:  Variable types for DIO
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#ifndef DIO_TYPES_H
#define DIO_TYPES_H

/*-------------------------------------------- INCLUDES             --*/
#include "dio_macros.h"
#include "dstype.h"

#ifdef CORBA
#include "dio_i.hh"
#endif

/*-------------------------------------------- CORBA                --*/

/*-------------------------------------------- TYPEDEFS             --*/
#ifndef CORBA

typedef enum dio_mode_t {
    DIO_READ_MODE,
    DIO_WRITE_MODE,
    DIO_UPDATE_MODE,
    DIO_UNKNOWN_MODE
}DIO_MODE_T;

typedef enum dio_state_t {
    DIO_OPEN_STATE,		/*- stream is open and ready -*/
    DIO_CLOSE_STATE,		/*- stream is closed -*/
    DIO_READ_STATE,		/*- stream is reading -*/
    DIO_WRITE_STATE,		/*- stream is writing -*/
    DIO_CONNECT_STATE,		/*- connection in place (sock only) -*/
    DIO_UNKNOWN_STATE
}DIO_STATE_T;

#endif /*CORBA*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/
extern CC_P int dio_init();
extern CC_P int dio_start();
extern CC_P int dio_stop();
extern CC_P int dio_addHierarchy(DS_DATASET_T *pDS,DS_DATASET_T *pAdd);
extern CC_P int dio_mapHierarchy(DS_DATASET_T *pDS,DS_DATASET_T *pAdd);
extern CC_P int dio_clearDataset(DS_DATASET_T *pDS);
extern CC_P char* dio_mode2text(DIO_MODE_T mode);
extern CC_P DIO_MODE_T dio_text2mode(const char* text);
extern CC_P const char* dio_state2text(DIO_STATE_T state);
extern CC_P DIO_STATE_T dio_text2state(const char* text);

#ifndef NOTCL
extern CC_P int dio_tcltk_browsefile(char** name, char** file
		, DIO_MODE_T *iomode);
#endif /*NOTCL*/

#ifndef NOKUIP
extern CC_P void dio_def_();

extern CC_P void kam_dio_count_();
extern CC_P void kam_dio_list_();
extern CC_P void kam_dio_newfilestream_();
extern CC_P void kam_dio_newsockstream_();
extern CC_P void kam_diostream_open_();
extern CC_P void kam_diostream_close_();
extern CC_P void kam_diostream_getevent_();
extern CC_P void kam_diostream_putevent_();
extern CC_P void kam_diostream_mode_();
extern CC_P void kam_diostream_state_();
extern CC_P void kam_diofilestream_filename_();
extern CC_P void kam_diosockstream_host_();
extern CC_P void kam_diosockstream_port_();
extern CC_P void kam_diosockstream_maxhandshakes_();
#endif /*NOKUIP*/

extern CC_P STAFCV_T dio_count();
extern CC_P STAFCV_T dio_list();
extern CC_P STAFCV_T dio_newfilestream(char* name, char* file
		, char* mode);
extern CC_P STAFCV_T dio_newsockstream(char* name, char* host
		, long port, char* mode);
extern CC_P STAFCV_T diostream_open(char* name, char* mode);
extern CC_P STAFCV_T diostream_close(char* name);
extern CC_P STAFCV_T diostream_getevent(char* name, char* dest);
extern CC_P STAFCV_T diostream_putevent(char* name, char* sour);
extern CC_P STAFCV_T diostream_mode(char* name);
extern CC_P STAFCV_T diostream_state(char* name);
extern CC_P STAFCV_T diofilestream_filename(char* name);
extern CC_P STAFCV_T diosockstream_host(char* name);
extern CC_P STAFCV_T diosockstream_port(char* name);
extern CC_P STAFCV_T diosockstream_maxhandshakes(char* name
		, long count);

#endif /* DIO_TYPES_H */

