C hitarray.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    hitarray
C   
#ifndef HITARRAY_INC
#define HITARRAY_INC
#endif
	STRUCTURE /HITARRAY_ST/
	INTEGER*4 data !  4 byte variable 0 
	END STRUCTURE
C
      CHARACTER*(*) HITARRAY_SPEC
      PARAMETER    (HITARRAY_SPEC=
     + ' struct hitarray {'                 //
     + ' long data;'                        //
     + ' };')
C
