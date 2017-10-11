// Platform independent calls for 
//
// semephores
// mutex's
// msgQueues
// thread creation
//
#ifndef _PLATFORM_H_
#define _PLATFORM_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <rtsLog.h>
#include <iccp.h>

#ifdef __CINT__
#define unix
#endif


#ifdef sun
//#include <thread.h>
#include <synch.h>
#elif unix
//#include <pthread.h>
#include <semaphore.h>
#endif

#ifdef unix
#include <pthread.h>
#include <sys/mman.h>
#include <SUNRT/msgQLib.h>
#include <SUNRT/ipcQLib.hh>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <UNIX/ThreadsMsgQueue.hh>
#include <sched.h>
#else                                 // VXWORKS 
#include <vxWorks.h>
#include <msgQLib.h>
#include <taskLib.h>
#endif

// just in case...
typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;

#ifdef vxworks
extern "C" {
void *memcpy32(UINT32 *dst, UINT32 *src, int cou);
} ;
#endif

//
// main() function
//
// use for platform independent entry points
// grabArgs() populates argc, argv
//
#ifdef vxworks
#define pMain(a) a(char *args)
#define pGrabArgs() int argc=1; \
char *argv[20]; \
char *__next_ = strtok(args," "); \
argv[0] = "unknown"; \
while (__next_) { \
  argv[argc++] = __next_; \
  __next_ = strtok(NULL, " "); \
}                   
#else
#define pMain(a) main(int argc, char *argv[])
#define pGrabArgs()
#endif

inline void pSleep(int secs)
{
#ifdef vxworks
  taskDelay(secs*100);
#else
  sleep(secs);
#endif
}

// 1/100 second delay....
inline void pTaskDelay(int ticks)
{
#ifdef vxworks
  taskDelay(ticks);
#else
  usleep(ticks * 10000);
#endif
}

//
// Task creation
//
//#ifdef sun
//typedef thread_t pTASKID;
//#elif unix
#ifdef unix
typedef pthread_t pTASKID;
#else
typedef int pTASKID;
#endif

inline pTASKID pTaskSelf()
{
#ifdef unix
  return pthread_self();
#else
  return taskIdSelf();
#endif
}

inline pTASKID pTaskSpawn(char *name, int priority, void *f(void *), void *args)
{
  pTASKID tid;

#ifdef unix
  pthread_attr_t attr;
  pthread_attr_init(&attr);
	
//   struct sched_param sched;
//   sched.sched_priority = priority;
//   if(pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM) ||
//      pthread_attr_setschedpolicy(&attr, SCHED_FIFO) ||
//      pthread_attr_setinheritsched(&attr, PTHREAD_EXPLICIT_SCHED) ||
//      pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) ||
//      pthread_attr_setschedparam(&attr, &sched)) 
//   {
// #ifndef sun
//     LOG(NOTE, "Can't setup custom attributes for task %s, not setting priority",name,0,0,0,0);
// #endif
//     pthread_attr_init(&attr);
//   }
	
  //#ifdef sun
  //  if(thr_create(NULL, NULL, f, args, 0,&tid)) 
  //#else
  if(pthread_create(&tid, &attr, f, args))
    //#endif
  {
    LOG(NOTE, "Can't setup custom attributes for task %s, not setting priority",name,0,0,0,0);
    if(pthread_create(&tid, NULL, f, args)) {
      LOG(CRIT, "Error creating %s thread: (%s)", name, strerror(errno));
      sleep(10);
      exit(0);
    }
  }
#else
  tid = taskSpawn(name,priority,0,4096,(FUNCPTR)f,(int)args,0,0,0,0,0,0,0,0,0);
#endif

  return tid;
}

inline int pChangePriority(int prio)
{
#ifdef sun
  return -1;
#elif linux
//   pTASKID tid = pthread_self();
//   struct sched_param sched;
//   int policy;
//   pthread_getschedparam(tid,&policy,&sched);
//   int old = sched.sched_priority;
//   sched.sched_priority = prio;
//   int ret = pthread_setschedparam(tid,SCHED_FIFO,&sched);
//   if(ret == 0)
//     return old;
//   else
//     return -1;
  return -1;
#else
  pTASKID tid =  taskIdSelf();
  int old=0;
  taskPriorityGet(tid, &old);
  taskPrioritySet(tid, prio);
  return old;
#endif  
}

inline int pGetMaxPriority()
{
#ifdef unix
	return sched_get_priority_max(SCHED_FIFO);
#else
	return 255;	
#endif
}

inline void pLockAllMemory()
{
#ifdef vxworks
  return;
#else
  if(mlockall(MCL_CURRENT | MCL_FUTURE) == -1)
  {
    LOG(DBG, "Error locking all memory",0,0,0,0,0);
  }
#endif
}

inline void pTaskDelete(pTASKID tid)
{
#ifdef sun
  LOG(WARN, "thread cancelation not supported in solaris");
#elif unix
  pthread_cancel(tid);
#else
  taskDelete(tid);
#endif
}

// Semaphores / mutexes
//     solaris : solaris posix-like semaphores
//     linux   : posix
//     vxworks : vxworks semaphores
//



#ifdef sun
typedef sema_t* SEM_ID;
typedef mutex_t* MUX_ID;
#elif unix
typedef sem_t* SEM_ID;
typedef pthread_mutex_t* MUX_ID;
#endif

#ifdef vxworks
typedef SEM_ID MUX_ID;
#endif


#ifdef sun
#define pSEM_PROTO(semName) sema_t rp_##semName; SEM_ID semName = &rp_##semName
#define pMUX_PROTO(muxName) mutex_t rp_##muxName; MUX_ID muxName = &rp_##muxName
#elif unix
#define pSEM_PROTO(semName) sem_t rp_##semName; SEM_ID semName = &rp_##semName
#define pMUX_PROTO(muxName) pthread_mutex_t rp_##muxName; MUX_ID muxName = &rp_##muxName
#else
#define pSEM_PROTO(semName) SEM_ID semName
#define pMUX_PROTO(muxName) pSEM_PROTO(muxName)
#endif



inline void pSemCInit(SEM_ID &sem, int value)
{
#ifdef unix
  int ret;
#ifdef sun
  ret = sema_init(sem, value, USYNC_THREAD, NULL);
#else  // linux
  ret = sem_init(sem,0,value);
#endif
  
  if(ret != 0)
  {
    LOG(CRIT, "Error creating semaphore (%s)",strerror(errno));
  }

#else  // vxworks
  sem = semCCreate(SEM_Q_PRIORITY, value);
  //sem = semCCreate(SEM_Q_FIFO, value);
#endif
}

inline void pSemBInit(SEM_ID &sem, int value)
{
#ifdef unix
  pSemCInit(sem, value);
#else
  sem = semBCreate(SEM_Q_PRIORITY, (value == 0) ?  SEM_EMPTY : SEM_FULL );
  //sem = semBCreate(SEM_Q_FIFO, (value == 0) ?  SEM_EMPTY : SEM_FULL );
#endif
}


inline void pMuxInit(MUX_ID &mux)
{
#ifdef sun
  mutex_init(mux, USYNC_THREAD, NULL);
#elif unix  // linux
  pthread_mutex_init(mux, NULL);
#else // vxworks
  mux = semBCreate(SEM_Q_PRIORITY, SEM_FULL);
  //mux = semBCreate(SEM_Q_FIFO, SEM_FULL);
#endif
}

// Returns true if successfull
inline bool pSemTryTake(SEM_ID sem)
{
  int ret;
#ifdef unix
#ifdef sun
  ret = sema_trywait(sem);
#else
  ret = sem_trywait(sem);
#endif
  if(ret == -1) return false;
  else return true;
#else  // vxworks
  ret = semTake(sem, 0);
  if(ret == ERROR) return false;
  else return true;
#endif  
}

inline bool pSemTake(SEM_ID sem)
{
  int ret=true;
#ifdef unix
  int iret;

  do {
#ifdef sun
    iret = sema_wait(sem);
#else
    iret = sem_wait(sem);
#endif
    if((iret) && (errno != EINTR)) {
      LOG(CRIT, "semTake error %s",strerror(errno));
    }
  } while (iret);

#else  // vxworks
  ret = semTake(sem, 100);
  if(ret == ERROR)
    ret = false;
  else
    ret = true;
#endif  

  return ret;
}

inline void pSemGive(SEM_ID sem)
{
#ifdef sun
  sema_post(sem);
#elif unix
  sem_post(sem);
#else // vxworks
  semGive(sem);
#endif
}

inline void pMuxLock(MUX_ID mux)
{
#ifdef sun
  mutex_lock(mux);
#elif unix
  pthread_mutex_lock(mux);
#else
  semTake(mux,WAIT_FOREVER);
#endif
}

// This is only implemented on linux...
// other platforms block...
#if defined(__linux__) || defined(__APPLE__)

inline bool pMuxTryLock(MUX_ID mux)
{
#ifdef sun
  mutex_lock(mux);
  return true;
#elif unix
  int ret = pthread_mutex_trylock(mux);
  if(ret == 0) return true;
  return false;
#else
  semTake(mux,WAIT_FOREVER);
  return true ;
#endif
}
#endif

inline void pMuxUnlock(MUX_ID mux)
{
#ifdef sun
  mutex_unlock(mux);
  //  usleep(1); // thr_yield();
#elif unix
  pthread_mutex_unlock(mux);
#else
  semGive(mux);
#endif
}


//
// message queues
//
// THREAD_MSG_QUEUE
// IPCQLIB
// SYSVQLIB
// vxworks
//
// vxworks and thread_msg_queues require an array..
// ipcqlib requires nothing (The first Queue creating defines the static variables)
//

// Make sure IPCQLIB, THREAD_MSG_QUEUE, SYSVQLIB, or vxworks is defined...
#if !defined(IPCQLIB) && !defined(THREAD_MSG_QUEUE) && !defined(SYSVQLIB)
#ifdef sun
#define IPCQLIB 
#endif

#if defined(__linux__) || defined(__APPLE__)
#define THREAD_MSG_QUEUE
#endif
#endif

#ifdef vxworks
#undef IPCQLIB
#undef THREAD_MSG_QUEUE
#undef SYSVQLIB
#endif

#ifdef vxworks
#define QUELIB_STRING "vxworks"
#elif defined(IPCQLIB)
#define QUELIB_STRING "ipcqlib"
#elif defined(THREAD_MSG_QUEUE)
#define QUELIB_STRING "thrMsgQ"
#elif defined(SYSVQLIB)
#define QUELIB_STRING "sysvqlib"
#else
#define QUELIB_STRING "noQs"
#endif

#ifdef THREAD_MSG_QUEUE
extern thrMsgQueue<ic_msg> **pMsgQArray;
#define pALLOC_MSGQARRAY(name) thrMsgQueue<ic_msg> *name[256]
#define pMSGQARRAY_PROTO(name) thrMsgQueue<ic_msg> *name[]
inline void pSetupMsgQArray(thrMsgQueue<ic_msg> *qarray[]) {
  pMsgQArray = qarray;
}
#elif defined(IPCQLIB)
#define pALLOC_MSGQARRAY(name) void *name
#define pMSGQARRAY_PROTO(name) void *name
inline void pSetupMsgQArray(void *) {};
#elif defined(SYSVQLIB)
#define pALLOC_MSGQARRAY(name) void *name
#define pMSGQARRAY_PROTO(name) void *name
inline void pSetupMsgQArray(void *) {};
#else
extern MSG_Q_ID *pMsgQArray;
#define pALLOC_MSGQARRAY(name) MSG_Q_ID name[256]
#define pMSGQARRAY_PROTO(name) MSG_Q_ID *name
inline void pSetupMsgQArray(MSG_Q_ID *qarray) {
  pMsgQArray = qarray;

  LOG(DBG, "pMsgQArray = 0x%x",pMsgQArray,0,0,0,0);
}
#endif

inline bool pMsgQValid(int node, int task)
{
#ifdef THREAD_MSG_QUEUE
  if(pMsgQArray[task] == NULL) return false;
  else return true;
#elif defined(IPCQLIB)
  ipcQClass *c = ipcQClass::find(node, task, 1);

  if(c) return true;
  return false;
#elif defined(SYSVQLIB)
  if(msgQTest(task)) return true;
  else return false;
#else
  if(pMsgQArray[task] == NULL) return false;
  else return true;
#endif
}

inline bool pMsgQCreate(int task, int size, int nmsgs, int mynode=0)
{
#ifdef THREAD_MSG_QUEUE
  if(pMsgQArray[task] != NULL) 
    return false;

  pMsgQArray[task] = new thrMsgQueue<ic_msg>(nmsgs);
  return true;
#elif defined(IPCQLIB)
  static ipcQClass *c;
  if (ipcQClass::daqTasks[task] != NULL) 
  {
    LOG(NOTE, "Task %d already used",task,0,0,0,0);
    return false;
  }
  c = new ipcQClass(task,1,mynode);  // let the static take care of saving it!!!

  LOG(NOTE, "Task %d created",task,0,0,0,0);

  return true;
#elif defined(SYSVQLIB)
  if(msgQCreate(task, nmsgs, size) == 0) return true;
  return false;
#else
  //if(pMsgQArray[task] != NULL) return false;

  pMsgQArray[task] = msgQCreate(nmsgs, size, MSG_Q_PRIORITY);
  //pMsgQArray[task] = msgQCreate(nmsgs, size, MSG_Q_FIFO);

  if(pMsgQArray[task] == NULL) {
    LOG(CRIT, "Can't create message queue %d (%s)",task, strerror(errno),0,0,0);
    return false;
  }

  LOG(DBG, "(pMsgQArray = 0x%x) msq %d = 0x%x",pMsgQArray,task,pMsgQArray[task],0,0);
  return true;
#endif
}

inline void pMsgQDelete(int task)
{
#ifdef vxworks
  msgQDelete(pMsgQArray[task]);
  pMsgQArray[task] = NULL;
#endif
}

// Returns 0 if good
// Returns -1 if bad
inline int pMsgQSend(int task, void *msg, int size, int node=0)
{
#ifdef THREAD_MSG_QUEUE
  int n=pMsgQArray[task]->free();
  if(n < 10) {
    LOG(WARN, "The queue %d is nearly full %d free",task,n,0,0,0);
  }
  if(n==0) {
    LOG(CRIT, "The queue %d is full, dropping message",task,0,0,0,0);
    return -1;
  }
  return pMsgQArray[task]->send((ic_msg *)msg);
#elif defined(IPCQLIB)

  ipcQClass *c;
  
  //if(node == 0) 
    //  c = ipcQClass::daqTasks[task];
  //  else          
    c = ipcQClass::find(node, task, 1);



  if(!c) {
    LOG("ERR", "Queue %d/0x%x does not exist",task,node,0,0,0);
    return -1;
  }

  if(c->send((void *)msg, size, 0) == size) return 0;

  else return -1;

#elif defined(SYSVQLIB)
  return msgQSend(task, (char *)msg, size);
#else
  if(msgQSend(pMsgQArray[task], (char *)msg, size, NO_WAIT, MSG_PRI_NORMAL) == OK) return 0;
  else return -1;
#endif
}

inline int pMsgQReceive(int task, void *msg, int size, int node=0)
{
#ifdef THREAD_MSG_QUEUE
  if(pMsgQArray[task]->receive((ic_msg *)msg))
  {
    return 0;
  }
  else
    return sizeof(ic_msg);

#elif defined(IPCQLIB)
  ipcQClass *c;
  
  // if(node == 0) 
  //  c = ipcQClass::daqTasks[task];
  //else          

    c = ipcQClass::find(node, task, 1);

  if(!c) {
    LOG("ERR", "Queue %d/0x%x does not exist",task,node,0,0,0);
    return -1;
  }
  return c->receive((void *)msg, size, 1);

#elif defined(SYSVQLIB)
  int ret;
  do {
    ret = msgQReceive(task, (char *)msg, size);
  } while((ret == -1) && (errno == EINTR));

  return ret;
#else
  return msgQReceive(pMsgQArray[task], (char *)msg, size, WAIT_FOREVER);
#endif
}

inline void pMemcpy(void *x, void *y, int bytes)
{
// #ifdef vxworks
//   memcpy32((UINT32 *)x,(UINT32 *)y,bytes);
// #else
  memcpy(x,y,bytes);
  //#endif
} 

#endif
