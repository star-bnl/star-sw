/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_memory.cc (StAF version)
**:
**:     level 3 clusterfinder memory management
**:     dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 4/15/98 ds
**:
**:>-----------------------------------------------------------------*/



/*******************************************************************************
+
+  LEDA-R  3.3.c
+
+  _memory.c
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


#include "l3cl_inc.h"
#include <stdlib.h>


typedef struct  tagmemory_elem_type { 
struct tagmemory_elem_type* next; 
} memory_elem_type;

typedef memory_elem_type* memory_elem_ptr;



/*
//------------------------------------------------------------------------------
// Memory Management
//
// S. Naeher (1990)
//------------------------------------------------------------------------------

// most malloc() implementations increase the size of the block to be allocated
// by one word to store additional information (e.g. the size of the block)
// and then round the size to the next power of two. So it seems to be a good
// idea to choose the block size to be a power of two minus one word.
*/

/* modified for one fixed size and c
   dirk schmischke 1/96
   */

/*
// constants
*/
const int memory_block_bytes = 4096 - sizeof(void*); 

/*
// for external usage
*/
memory_elem_ptr        memory_free_list; 

/*
// static
*/
static long int        memory_total_count; 
static memory_elem_ptr memory_block_list;
static int             memory_block_count;
static int             memory_initialized = 0;
/* fixed size of one clusteruc-node */
static int             chunk_size = sizeof(TClusterUCNode);
static int             allocations = 0;

static void memory_init()
{ 
  memory_free_list   = 0;
  memory_total_count = 0;
  memory_block_list  = 0;
  memory_block_count = 0;
  allocations = 0;
  
  memory_initialized = 1;
 }

void memory_allocate_block()
{ 
  int sz = memory_block_bytes;
  register memory_elem_ptr p;
  register memory_elem_ptr stop;
  register int words = (chunk_size + sizeof(void*) - 1)/sizeof(void*);
  int bytes = words * sizeof(void*);
  int num   = sz/bytes - 1;
  memory_elem_ptr q;

  if (memory_initialized == 0) memory_init();

  /*
  //allocate new block of size sz byte and slice it into chunks of size cunk_size bytes
  */


  /*
  // compute number of chunks (the first one is used for chaining blocks)
  */

  memory_block_count++;
  memory_total_count += num;

  if ((p= (memory_elem_ptr) malloc(sz)) == 0 )
   { printf ("ds memory allocation: out of memory\n");
     print_statistics();
     abort();
    }
  /*
  //insert block into list of used blocks
  */
  p->next = memory_block_list;
  memory_block_list = p;
  p += words;

  q = memory_free_list;

  memory_free_list = p;

  stop = p + (num-1)*words;
  stop->next = q;

  while (p < stop) p = (p->next = p+words);

}

void memory_clear()
{ 
  register memory_elem_ptr p;
  long used;


  p = memory_free_list;
  used = memory_total_count;
  while (p) { used--; p = p->next; }

  if (used==0)
  { 
    while (memory_block_list)
    { 
      p = memory_block_list;
      memory_block_list = p->next;
      memory_block_count--;
      free((char*)p);
    }
    memory_free_list = 0;
    memory_total_count = 0;
  }
}


void memory_kill()
{ 
  register memory_elem_ptr p;

  while (memory_block_list)
  { 
    p = memory_block_list;
    memory_block_list = p->next;
    free((char*)p);
  }
  memory_free_list = 0;
  memory_total_count = 0;
  memory_block_count = 0;
}


void* allocate_bytes_with_check()
{
  memory_elem_ptr p;

  allocations++;
  if (memory_free_list == 0) memory_allocate_block();
  p = memory_free_list;
  memory_free_list = p->next;
  /*
  //printf("allocate(%d):   %x\n",chunk_size,p);
  //fflush(stdout);
  */
  return p;
}

void deallocate_bytes_with_check(void* p)
{ 
/*  memory_elem_ptr q; */
  /*
    //printf("deallocate(%d): %x\n",chunk_size,p);
    //fflush(stdout);
    */
  /*
printf("Dealloc: %ld\n", p);
*/
  /*  if (memory_block_count == 0) 
     printf("no block allocated\n");
  q = memory_free_list;
  while(q && q != (memory_elem_ptr)p) q = q->next;
  if (q) printf("Kraut memory: pointer %d deleted twice\n",p);
  */
  ((memory_elem_ptr)p)->next = memory_free_list;
  memory_free_list = (memory_elem_ptr)p;
}


int used_memory()
{ 
  long int total_bytes=0;

  total_bytes += chunk_size*memory_total_count;

  return total_bytes;
 }



void print_statistics()
{
  long int total,free,used;
  long int total_bytes=0, free_bytes=0, used_bytes=0, b;
  memory_elem_ptr p;
  float kb;

  printf("\n");
  printf("\t+----------------------------------------------------------+\n");
  printf("\t|   size     used     free     blocks     bytes    allocs  |\n"); 
  printf("\t+----------------------------------------------------------+\n");

  if ((total = memory_total_count) > 0 || memory_free_list)
    { printf("\t|   %3d    ",chunk_size);
      fflush(stdout);
      p = memory_free_list;
      free = 0;
      while (p) { free++; p = p->next; }
      b = total*chunk_size; 
      used = total - free;
      free_bytes  += free*chunk_size;
      used_bytes  += used*chunk_size;
      total_bytes += b;
      printf("%6ld   %6ld    %6d   %8ld    %6d  |\n",
              used,free,memory_block_count,b, allocations);
     }

  kb = (float)total_bytes/1024;

  printf("\t+----------------------------------------------------------+\n");
  printf("\t|                                space:%8.2f kb         |\n",kb);
  printf("\t+----------------------------------------------------------+\n");
  printf("\n");

}
