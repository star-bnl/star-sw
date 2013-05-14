#ifndef SHMLIB_HH
#define SHMLIB_HH

/*****************************************************************
 * To use:
 *          1. Mother of all tasks createShmSegment() for each segment
 *         
 *          2. Each tasks calls getShmPtr() to attach
 *
 *          3. After a task has called getShmPtr() for a segment it
 *             may call getShmAttributes()
 *****************************************************************/
#define SHM_KEY_BASE 0xfd000000
#define MAX_SEGMENTS 10

struct ShmAttributes
{
  int key;
  int segment;
  unsigned int size;
  int shmid;
  char *ptr;
};

int createShmSegment(int seg, unsigned int size);
char *getShmPtr(int seg, unsigned int offset);
ShmAttributes *getShmAttributes(int seg);

#endif
