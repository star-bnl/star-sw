/*Copyright 1996, Lawrence Berkeley National Laboratory */
/*
** FAKE CORBA.h for use with purely co-located executables that cannot
** link to a real CORBA implementation.
*/
#ifdef							  CORBA

#define NEW(A) new A_i
#define BIND(A) A_i::_bind
#define RELEASE(A) A_i->_release()
#define DELETE(A) A_i->_release()
#define CORBA_TIE(A) "DEF_TIE_" #A "_i(" #A ")"

#else							/*CORBA*/

#define NEW(A) new A
#define BIND(A) new A
#define RELEASE(A) delete A
#define DELETE(A) delete A
#define CORBA_TIE(A)

#endif							/*CORBA*/

                                        /* define CORBA sequences */
#define TYPEDEF_SEQUENCE(AAA,BBB) \
typedef struct _ ## BBB { \
   unsigned long _maximum; \
   unsigned long _length; \
   AAA *_buffer; \
} BBB 

