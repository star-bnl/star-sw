c table_header.inc
c
#ifndef TABLE_HEADER_INC
#define TABLE_HEADER_INC
#endif 
c
       STRUCTURE  /table_head_st/
           CHARACTER*20 name    ! table name
           CHARACTER*20 type    ! table type
           INTEGER*4 maxlen     ! # rows allocated
           INTEGER*4 nok         ! # rows filled
           INTEGER*4 rbytes     ! number of bytes per row
           INTEGER*4 dummy      ! dummy to align 64bit  pointer  
#ifndef _LP64
           INTEGER*4 dsl_pointer  ! swizzled (DS_DATASET_T*)
           INTEGER*4 data_pointer ! swizzled (char*)
#else
           INTEGER*8 dsl_pointer  ! swizzled (DS_DATASET_T*)
           INTEGER*8 data_pointer ! swizzled (char*)
#endif
       END STRUCTURE  ! table_head_st
c
c
