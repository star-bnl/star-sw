// Memory Management For Unpacker
// These objects replace malloc with a safe, no leak allocation
// scheme
#include <stdio.h>
#include "MemMan.hh"

MemoryList::~MemoryList()
{
  for(list<void *>::iterator p = alloc_list.begin();
      p!= alloc_list.end(); p++)
  {
    printf("Destructor:: Freeing 0x%xd\n", *p);
    free(*p);
  }
};


MemoryManager::MemoryManager()
{
  mem_list = new MemoryList();
}

MemoryManager::MemoryManager(MemoryList *l)
{
  mem_list = l;
  mem_list->use_count++;
}

MemoryManager::~MemoryManager()
{
  mem_list->use_count--;
  if(mem_list->use_count == 0)
  {
    delete mem_list;
  }
}

void *MemoryManager::mm_alloc(int size)
{
  void *addr;
  addr = malloc(size);
  mem_list->alloc_list.push_front(addr);
  printf("Allocating 0x%xd\n", addr);
  return addr;
}

void MemoryManager::mm_free(void *addr)
{
  list<void *>::iterator p = find(mem_list->alloc_list.begin(),
				  mem_list->alloc_list.end(),
				  addr);
  if(p == mem_list->alloc_list.end())
  {
    cout << "MemMan::Tried to free invalid address" << endl;
    exit(0);
  }

  printf("Direct:: Freeing 0x%xd\n", *p);
  mem_list->alloc_list.erase(p);
  return;
}

