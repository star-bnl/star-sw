/*******************************************************************************
+
+  LEDA 3.4.1
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
//------------------------------------------------------------------------------
// Memory Management
//
// S. N"aher C. Uhrig  (1996)
//------------------------------------------------------------------------------

#ifdef LEDA
#include "_memory.hpp"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*  moved to _global.c:
 *
 *  unsigned memory_manager_init::count;
 *  memory_manager std_memory_mgr;
 */


memory_manager_init::memory_manager_init() 
{ if (count++ == 0) std_memory_mgr.init(256,"STD_MEMORY_MGR"); }

memory_manager_init::~memory_manager_init() 
{ if (--count == 0) std_memory_mgr.destroy(); }


unsigned int  memory_manager_init::count = 0;

memory_manager::memory_manager(int sz) 
{ if (this != &std_memory_mgr) init(sz,"USR_MEMORY_MGR"); }

memory_manager::~memory_manager() 
{ if (this != &std_memory_mgr) destroy(); }



//-----------------------------------------------------------------------------
// most malloc() implementations increase the size of the block to be allocated
// by one word to store additional information (e.g. the size of the block)
// and then round the size to the next power of two. So it seems to be a good
// idea to choose the block size to be a power of two minus one word.
//-----------------------------------------------------------------------------

const int block_bytes = (1<<20) - sizeof(void*);


void memory_manager::init(int sz, const char* n)
{ max_sz = sz-1;
  sprintf(name,"%s",n);
  free_list   = new memory_elem_ptr[sz];
  block_list  = new memory_elem_ptr[sz];
  total_count = new long[sz];
  block_count = new long[sz];
  for(int i = 0; i < sz; i++)
  { free_list[i]   = 0;
    total_count[i] = 0;
    block_list[i]  = 0;
    block_count[i] = 0;
   }
 }


void memory_manager::kill()
{ memory_elem_ptr p;
  for (int i=1; i<=max_sz; i++)
  { while (block_list[i])
    { p = block_list[i];
      block_list[i] = p->next;
      free((char*)p);
     }
    free_list[i] = 0;
    total_count[i] = 0;
    block_count[i] = 0;
   }
}

// kill all the memory blocks but the first and reset the free lists
void memory_manager::kill_but_first()
{	int all_deleted;
	memory_elem_ptr p;
	for (int i=1; i<=max_sz; i++)
	{ 
		all_deleted = 1;
		while (block_list[i])
		{ 
			if (block_list[i]->next != 0)
			{	
				p = block_list[i];
				block_list[i] = p->next;
				free((char*)p);
			}
			else
			{
				memory_elem_ptr p;
				memory_elem_ptr stop;

				int words = (i + sizeof(void*) - 1)/sizeof(void*);

				int bytes = words * sizeof(void*);

				// compute number of chunks (the first one is used for chaining blocks)

				int num   = block_bytes/bytes - 1;

				block_count[i] = 1;
				total_count[i] += num;


				//insert block into list of used blocks
				p = block_list[i];
				p->next = 0;
				p += words;

				memory_elem_ptr q = 0;

				free_list[i] = p;

				stop = p + (num-1)*words;
				stop->next = q;

				while (p < stop) p = (p->next = p+words);
				all_deleted = 0;
				break;
			}
		}
		if (all_deleted == 1)
		{
			free_list[i] = 0;
			total_count[i] = 0;
			block_count[i] = 0;
		}
	}
}



void memory_manager::destroy()
{ if (free_list == 0) return;
  char* leda_init = getenv("LEDA_INIT");
  if (leda_init && strcmp(leda_init,"statistics") == 0) print_statistics(); 
  kill();
  delete[] free_list;
  delete[] block_list;
  delete[] total_count;
  delete[] block_count;
  free_list = 0;
 }

 

void memory_manager::allocate_block(int b, int N)
{ 
  if (N == 0) return;

//  if (b > max_sz) 
//     error_handler(1,string("allocate_block: size (%d bytes) too big",b));

  int sz = (N < 0) ?  block_bytes : b * (N+1);

  //allocate new block of size sz byte and slice it into chunks of size b bytes

  memory_elem_ptr p;
  memory_elem_ptr stop;

  int words = (b + sizeof(void*) - 1)/sizeof(void*);

  int bytes = words * sizeof(void*);

  // compute number of chunks (the first one is used for chaining blocks)

  int num   = sz/bytes - 1;

  block_count[b]++;
  total_count[b] += num;

  if ((p=memory_elem_ptr(malloc(sz))) == 0 )
//   { cout << "LEDA memory allocation: out of memory\n";
//     print_statistics();
     abort();
//    }

  //insert block into list of used blocks
  p->next = block_list[b];
  block_list[b] = p;
  p += words;

  memory_elem_ptr q = free_list[b];

  free_list[b] = p;

  stop = p + (num-1)*words;
  stop->next = q;

  while (p < stop) p = (p->next = p+words);

}



void memory_manager::clear()
{ 
  for (int i=1; i<=max_sz; i++)
  { memory_elem_ptr p = free_list[i];
    long used = total_count[i];

    while (p) { used--; p = p->next; }

    if (used==0)
    { while (block_list[i])
      { p = block_list[i];
        block_list[i] = p->next;
        block_count[i]--;
        free((char*)p);
       }
      free_list[i] = 0;
      total_count[i] = 0;
     }
   }

}


memory_elem_ptr memory_manager::allocate_bytes_with_check(int bytes)
{ 
  if (free_list[bytes] == 0) allocate_block(bytes,-1);
  memory_elem_ptr p = free_list[bytes];
  free_list[bytes] = p->next;
  return p;
}

void memory_manager::deallocate_bytes_with_check(void* p, int bytes)
{ if (block_count[bytes] == 0) 
     exit(-2); //"no block allocated");
  memory_elem_ptr q = free_list[bytes];
  while(q && q != memory_elem_ptr(p)) q = q->next;
  if (q) exit(-3); ("LEDA memory: pointer %d deleted twice");
  memory_elem_ptr(p)->next = free_list[bytes];
  free_list[bytes] = memory_elem_ptr(p);
 }


memory_elem_ptr memory_manager::allocate_words(int words)
{ int bytes = words * sizeof(void*);
  if (free_list[bytes] == 0) allocate_block(bytes,-1);
  memory_elem_ptr p = free_list[bytes];
  free_list[bytes] = p->next;
  return p;
}

void memory_manager::deallocate_words(void* p, int words)
{ int bytes = words * sizeof(void*);
  memory_elem_ptr(p)->next = free_list[bytes];
  free_list[bytes] = memory_elem_ptr(p);
 }


int memory_manager::used_memory()
{ long int total_bytes=0;
  for (int i=1;i<=max_sz;i++)
     total_bytes += i*total_count[i];
  return total_bytes;
 }



void memory_manager::print_statistics()
{ 
  long int total,free,used;
  long int total_bytes=0, free_bytes=0, used_bytes=0, b;


  printf("\n");
  printf("\t %s\n",name); 
  printf("\t+--------------------------------------------------+\n");
  printf("\t|   size     used     free     blocks     bytes    |\n"); 
  printf("\t+--------------------------------------------------+\n");

  for (int i=1;i<=max_sz;i++)
    if ((total = total_count[i]) > 0 || free_list[i])
    { printf("\t|   %3d    ",i);
      fflush(stdout);
      memory_elem_ptr p = free_list[i];
      free = 0;
      while (p) { free++; p = p->next; }
      b = total*i; 
      used = total - free;
      free_bytes  += free*i;
      used_bytes  += used*i;
      total_bytes += b;
      printf("%6ld   %6ld    %6ld   %8ld    |\n",
              used,free,block_count[i],b);
     }

 float kb = float(total_bytes)/1024;
	float sec = 123;

 printf("\t+--------------------------------------------------+\n");
 printf("\t|   time:%6.2f sec              space:%8.2f kb |\n",sec,kb);
 printf("\t+--------------------------------------------------+\n");
 printf("\n");
 fflush(stdout);
}






//------------------------------------------------------------------------------
// Memory Management for Multi-Thread  Applications
//
// c. Uhrig (1996)
//------------------------------------------------------------------------------

#if defined(MULTI_THREAD)

unsigned  memory_management_init::count;

memory_management std_memory_mgment;


void memory_management::init(int n, int sz)
{ num = n;
  tbl_sz =sz;
  head_free_list = 0;
  char name[64];
  for(int i = 1; i <= n; i++)
  { sprintf(name,"THREAD_MEMORY_MGER %d",i);
    memory_manager* p = new memory_manager(sz,name);
    p->next = head_free_list;
    head_free_list = p;
   }
 }


memory_manager& memory_management::acquire()
{ 
  empty_lock.acquire();   // locked if list of memory objects is empty
  list_lock.acquire();    // locks the list 

  memory_manager* mger = head_free_list;  // pop
  head_free_list = head_free_list->next;

  // if list gets empty, it stays locked by empty_lock
  if (head_free_list != nil) empty_lock.release();

  list_lock.release();

  return *mger;
}


void memory_management::release(memory_manager& mgr) 
{ list_lock.acquire();       
  mgr.next = head_free_list;  // push
  head_free_list = &mgr; 
  list_lock.release();
}



memory_elem_ptr memory_management::allocate_bytes(int bytes)
{ memory_manager &manager = acquire();
  memory_elem_ptr help = manager.allocate_bytes(bytes);
  release(manager);
  return help;
}
  
void memory_management::deallocate_bytes(void* p, int bytes)
{ memory_manager &manager = acquire();
  manager.deallocate_bytes(p, bytes);
  release(manager);
}

void memory_management::deallocate_list(void* head,void* tail, int bytes)
{ memory_manager &manager = acquire();
  manager.deallocate_list(head, tail, bytes);
  release(manager);
}


void memory_management::allocate_block(int b, int N)
{ memory_manager &manager = acquire();
  manager.allocate_block(b, N);
  release(manager);
}


void memory_management::clear()
{ memory_manager &manager = acquire();
  manager.clear();
  release(manager);
}


void memory_management::kill()
{ for (memory_manager* p = head_free_list; p; p = p->next) 
     p->kill(); 
 }


void memory_management::print_statistics() 
{ for (memory_manager* p = head_free_list; p; p = p->next) 
     p->print_statistics();
 }


void memory_management::destroy()
{ while (head_free_list)
  { memory_manager* p = head_free_list;
    head_free_list = p->next;
    delete p;
   }
 }


memory_elem_ptr memory_management::allocate_bytes_with_check(int bytes)
{ memory_manager &manager = acquire();
  memory_elem_ptr help = manager.allocate_bytes_with_check(bytes);
  release(manager);
  return help;
}

void memory_management::deallocate_bytes_with_check(void* p, int bytes)
{ memory_manager &manager = acquire();
  manager.deallocate_bytes_with_check(p, bytes);
  release(manager);
}

memory_elem_ptr memory_management::allocate_words(int words)
{ memory_manager &manager = acquire();
  memory_elem_ptr help = manager.allocate_words(words);
  release(manager);
  return help;
}

void memory_management::deallocate_words(void* p, int words)
{ memory_manager &manager = acquire();
  manager.deallocate_words(p, words);
  release(manager);
}


int memory_management::used_memory()
{ memory_manager &manager = acquire();
  int help = manager.used_memory();
  release(manager);
  return help;
}

#endif
#else
void dummy ( ) { }
#endif
