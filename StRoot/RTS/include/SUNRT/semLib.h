#ifndef _SEMLIB_H_
#define _SEMLIB_H_

#include <semaphore.h>
#include <errno.h>
#include "objLib.h"

typedef sem_t* SEM_ID;

// deletes the ctl segment it it exists
void semClearCtl();

// This returns a named process semaphore...
// if the semaphore already exists, its count will be reset to initialCount
SEM_ID semCreateNamed(int initialCount, char *name);

// Attach to an existing semaphore by name...
// returns NULL if semaphore does not exist
SEM_ID semAttachNamed(char *name);

// This returns a thread semaphore...or NULL if error
SEM_ID semCCreate(int options, int initialCount);

// OK, or ERROR if semID invalid
inline STATUS semGive(SEM_ID semId)
{
  return sem_post(semId);
}

// NO_WAIT is properly supported.
// Timeouts are only supported in that if the timeout is WAIT_FOREVER
// the semephore is not interrupted by signals, but if it is any other
// value it is...
// 
// OK, or ERROR 
//
// errno set to 
//    EAGAIN - timeout set to NO_WAIT but would block
//    EINVAL - bad semaphore id
//    EINTR  - interupted
//    EDEADLK - would deadlock
inline STATUS semTake(SEM_ID semId, int timeout=WAIT_FOREVER)
{
  if(timeout == NO_WAIT) return sem_trywait(semId);

  while(sem_wait(semId) == -1)
  {
    if(errno != EINTR) return -1;
    if(timeout != WAIT_FOREVER) return -1;
  }
  return OK;
}

// This works for both named and unnamed semaphores
STATUS semDelete(SEM_ID semId);

#endif
