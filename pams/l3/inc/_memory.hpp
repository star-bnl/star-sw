/*******************************************************************************
+
+  LEDA 3.4.1
+
+  memory.h
+
+  This file is part of the LEDA research version (LEDA-R) that can be 
+  used free of charge in academic research and teaching. Any commercial
+  use of this software requires a license which is distributed by the
+  LEDA Software GmbH, Postfach 151101, 66041 Saarbruecken, FRG
+  (fax +49 681 31104).
+
+  Copyright (c) 1991-1996  by  Max-Planck-Institut fuer Informatik
+  Im Stadtwald, 66123 Saarbruecken, Germany     
+  All rights reserved.
+ 
*******************************************************************************/
#ifndef LEDA_MEMORY_H
#define LEDA_MEMORY_H

#if !defined(LEDA_ROOT_INCL_ID)
#define LEDA_ROOT_INCL_ID 340063
//#include <LEDA/REDEFINE_NAMES.h>
#endif


/*{\Mtext

\section{Memory Management} \label{Memory Management}

LEDA offers an efficient memory management system that is used internally 
for all node, edge and item types. This system can easily be customized for 
user defined classes by the ``LEDA\_MEMORY" macro. You simply have
to add the macro call ``LEDA\_MEMORY($T$)" to the declaration of a class
$T$. This redefines new and delete operators for type $T$, such that
they allocate and deallocate memory using LEDA's internal memory manager. 

{\bf Attention:}
There is a restriction on the size of the type $T$, however. Macro
LEDA\_MEMORY may only be applied to types $T$ with {\bf sizeof(T) \< 256}.
Note that this condition is (for efficiency reasons) not checked. 

We continue the example from section \ref{Overloading}:

\medskip
{\bf struct} $pair$ $\{$\\
\hspace*{.5cm}$double$ $x$;\\
\hspace*{.5cm}$double$ $y$;

\hspace*{.5cm}$pair()$$\{\ x = y = 0;\ \}$\\
\hspace*{.5cm}$pair($const $pair\&\ p)\ \{\ x = p.x;\ y = p.y;\ \}$

\hspace*{.5cm}friend $ostream$\& \ operator\<\<($ostream$\&,const $pair$\&) \hspace{.7cm}$\{$ \dots $\}$\\
\hspace*{.5cm}friend $istream$\& \ operator\>\>($istream$\&,$pair$\&) \hspace{1.85cm}$\{$ \dots $\}$\\
\hspace*{.5cm}friend $int$ \hspace{1.3cm}compare(const $pair\&\ p$, const $pair\&\ q$) $\{$ \dots $\}$

\hspace*{.5cm}LEDA\_MEMORY($pair$)

 $\}$;

\smallskip
dictionary\<$pair$,$int$\> D;

}*/


struct  memory_elem_type { 
 struct memory_elem_type* next; 
};


typedef struct memory_elem_type* memory_elem_ptr;


class memory_manager {

 friend class memory_management;
 friend class memory_manager_init;

 int              max_sz;
 char             name[64];
 long int*        total_count;
 long int*        block_count;
 memory_elem_ptr* block_list;
 memory_manager*  next;

 void init(int,const char* = 0);
 void destroy();

public:

 memory_elem_ptr* free_list;

 memory_manager(int sz=256);
~memory_manager();

 void            allocate_block(int,int);
 memory_elem_ptr allocate_words(int);
 memory_elem_ptr allocate_bytes(int);
 memory_elem_ptr allocate_bytes_with_check(int);
 
 void deallocate_words(void*,int);
 void deallocate_bytes(void*, int);
 void deallocate_bytes_with_check(void*,int);
 void deallocate_list(void*, void*, int);
 
 void clear();
 void kill();
 // kill all the memory blocks but the first and reset the free lists
 void kill_but_first();
 void print_statistics();
 int  max_size() { return max_sz; }
 int  used_memory();
};



inline memory_elem_ptr memory_manager::allocate_bytes(int bytes)
{ memory_elem_ptr* q = free_list+bytes;
  if (*q==0) allocate_block(bytes,-1);
  memory_elem_ptr p = *q;
  *q = p->next;
  return p;
}

inline void memory_manager::deallocate_bytes(void* p, int bytes)
{ memory_elem_ptr* q = free_list+bytes;
  memory_elem_ptr(p)->next = *q;
  *q = memory_elem_ptr(p);
 }

inline void memory_manager::deallocate_list(void* head,void* tail, int bytes)
{ memory_elem_ptr* q = free_list+bytes;
  memory_elem_ptr(tail)->next = *q;
  *q = memory_elem_ptr(head);
 }


// standard memory manager

extern memory_manager std_memory_mgr;



class memory_manager_init {

static unsigned count;

public:
  memory_manager_init();
 ~memory_manager_init();
};

static memory_manager_init memory_mgr_init;





//-----------------------------------------------------------------------------
// multi thread stuff
//-----------------------------------------------------------------------------
#if defined(MULTI_THREAD)

#include <LEDA/thread/mem_mgment.h>

#define std_memory  std_memory_mgment

#else

#define std_memory  std_memory_mgr

#define LEDA_MEMORY(type)\
\
void* operator new(size_t bytes)\
{ memory_elem_ptr* q = std_memory_mgr.free_list+bytes;\
  if (*q==0) std_memory_mgr.allocate_block(bytes,-1);\
  memory_elem_ptr p = *q;\
  *q = p->next;\
  return p;\
 }\
\
void* operator new(size_t,void* p) { return p; }\
void* operator new(size_t,void* p,int) { return p; }\
\
void  operator delete(void* p, size_t bytes)\
{ memory_elem_ptr* q = std_memory_mgr.free_list+bytes;\
  memory_elem_ptr(p)->next = *q;\
  *q = memory_elem_ptr(p);\
 }


#define LEDA_MEMORY_WITH_CHECK(type)\
void* operator new(size_t bytes)\
{ return std_memory_mgr.allocate_bytes_with_check(bytes); }\
\
void* operator new(size_t,void* p) { return p; }\
void* operator new(size_t,void* p,int) { return p; }\
\
void  operator delete(void* p,size_t bytes)\
{ std_memory_mgr.deallocate_bytes_with_check(p,bytes); }



#define USER_MEMORY(type)\
\
void* operator new(size_t bytes)\
{ memory_elem_ptr* q = user_memory_mgr.free_list+bytes;\
  if (*q==0) user_memory_mgr.allocate_block(bytes,-1);\
  memory_elem_ptr p = *q;\
  *q = p->next;\
  return p;\
 }\
\
void* operator new(size_t,void* p) { return p; }\
void* operator new(size_t,void* p,int) { return p; }\
\
void  operator delete(void* p, size_t bytes)\
{ memory_elem_ptr* q = user_memory_mgr.free_list+bytes;\
  memory_elem_ptr(p)->next = *q;\
  *q = memory_elem_ptr(p);\
 }



#endif


// for backward compatibility

inline memory_elem_ptr allocate_words(int n) 
{ return std_memory.allocate_words(n); }

inline memory_elem_ptr allocate_bytes(int n)
{ return std_memory.allocate_bytes(n); }

inline memory_elem_ptr allocate_bytes_with_check(int n)
{ return std_memory.allocate_bytes_with_check(n); }

inline void deallocate_words(void* p, int n)
{ std_memory.deallocate_words(p,n); }

inline void deallocate_bytes(void* p, int n)
{ std_memory.deallocate_bytes(p,n); }

inline void deallocate_bytes_with_check(void* p, int n)
{ std_memory.deallocate_bytes_with_check(p,n); }

inline void memory_clear()     { std_memory.clear(); }
inline void memory_kill()      { std_memory.kill(); }
inline void print_statistics() { std_memory.print_statistics(); }
inline int  used_memory()      { return std_memory.used_memory(); }




#if LEDA_ROOT_INCL_ID == 340063
#undef LEDA_ROOT_INCL_ID
//#include <LEDA/UNDEFINE_NAMES.h>
#endif

#endif
