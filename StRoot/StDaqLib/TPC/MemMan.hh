// Memory Management For Unpacker
// These objects replace malloc with a safe, no leak allocation
// scheme

#include <stdlib.h>
#include <list>
#include <algorithm>

class MemoryList
{
public:
  MemoryList() { use_count = 1; };
  ~MemoryList();
  list<void *> alloc_list;
  
  int use_count;
};
  
class MemoryManager
{
public:
  MemoryManager();
  MemoryManager(MemoryList *);
  ~MemoryManager();

  MemoryList *getList() { return mem_list; };
  void *mm_alloc(int size);
  void mm_free(void *);

private:
  MemoryList *mem_list;
};  
  
