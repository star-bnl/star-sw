#ifndef TMSGQUEUE_HH
#define TMSGQUEUE_HH

#ifdef sun
#include <synch.h>
#include <thread.h>
#else
#include <semaphore.h>
#include <pthread.h>
#endif
#include <errno.h>
#include "StaticSizedDQueue.hh"

// The queue has to be protected on both ends

template <class T> class thrMsgQueue
{
public:
  thrMsgQueue(int s) ; // number of elements of type T deep
  ~thrMsgQueue() ;
  int send(T* a, int prio = 0 ) ;  // 0 = low  prio != 0 == high  
    int receive(T*, int block=1) ;              // Blocking  
#ifndef sun
  int peek(T* a);           
#endif
  int entries() { return(q->entries()) ;} ;
  int free() { return(q->free()) ;} ;
  //  int clear() ; // 0 is O.K >0 some fatal error !!!!
  // I removed clear since clear would require another set of locks
  // to avoid changing of the semaphores during wiping
private:
  sdqueue<T> *q ;
#ifdef sun
  sema_t    empty ;
  sema_t    occupied ;
  mutex_t mp ;
#else
  sem_t    empty ;
  sem_t    occupied ;
  pthread_mutexattr_t mattr ;
  pthread_mutex_t mp ;
#endif
  int elements ;
};

// Implementation
template <class T> thrMsgQueue<T>::~thrMsgQueue()
{
#ifdef sun
  sema_destroy(&empty);
  sema_destroy(&occupied) ;
  mutex_destroy(&mp) ;
#else
  sem_destroy(&empty) ;
  sem_destroy(&occupied) ;
  pthread_mutex_destroy(&mp) ;
#endif
  delete q;
};

//--------------------------------------------------------------
template <class T> thrMsgQueue<T>::thrMsgQueue(int s) 
{ 
  elements = s;
  q = new sdqueue<T>(elements);

#ifdef sun
  mutex_init(&mp,USYNC_THREAD,NULL);
  sema_init(&empty,elements,USYNC_THREAD,NULL) ;
  sema_init(&occupied,0,USYNC_THREAD,NULL) ; 
#else
  pthread_mutex_init(&mp,NULL); 
  sem_init(&empty,0,elements) ;
  sem_init(&occupied,0,0) ; 
#endif
};
//--------------------------------------------------------------
#if defined(linux) || defined(__APPLE__)
template <class T> int thrMsgQueue<T>::send(T* a, int prio) 
#else
template <class T> int thrMsgQueue<T>::send(T* a, int prio = 0) 
#endif
{
  int iret ;
 l1:
  errno = 0 ;
#ifdef sun
  iret = sema_wait(&empty);
#else
  iret = sem_wait(&empty) ;
#endif
  if(iret) 
    { 
      if(errno == EINTR) goto l1 ; // a signal 
      else return(-1) ;
    }
#ifdef sun
  mutex_lock(&mp);
#else
  pthread_mutex_lock(&mp) ;
#endif

  if(prio)
    iret = q->prepend(a) ;
  else
    iret = q->insert(a) ;

#ifdef sun
  mutex_unlock(&mp);
#else
  pthread_mutex_unlock(&mp);
#endif

  if(iret) 
  {
    return(-1) ;
  }

#ifdef sun
  sema_post(&occupied) ;
#else
  sem_post(&occupied) ;
#endif
  return(0) ; // send it !!!
};
//-----------------------------------------------------
template <class T> int thrMsgQueue<T>::receive(T* a, int block)
{
  int iret ;

 l1:
  errno = 0 ;

  if(block) {
#ifdef sun
      iret = sema_wait(&occupied);
#else
      iret = sem_wait(&occupied);
#endif
  }
  else {
      iret = sem_trywait(&occupied);
      if(iret < 0) {
	  return -1;
      }
  }
      

  if(iret) 
  { 
    if(errno == EINTR) goto l1;
    return(-10);
  }

#ifdef sun
  mutex_lock(&mp);
#else
  pthread_mutex_lock(&mp) ;
#endif

  iret = q->get(a) ;

#ifdef sun
  mutex_unlock(&mp);
#else
  pthread_mutex_unlock(&mp) ;
#endif

  if(iret) 
  { 
    return(-20) ;
  }

#ifdef sun
  sema_post(&empty);
#else
  sem_post(&empty);
#endif

  return(0);
};

#ifndef sun
template <class T> int thrMsgQueue<T>::peek(T* a)
{
  int iret ;
  int semval = 0;
  errno = 0 ;

  pthread_mutex_lock(&mp) ;
  iret = sem_getvalue(&occupied, &semval);

  if(semval == 0) { 
    iret = -1;
  }
  else {
    iret = q->first(a);
  }

  pthread_mutex_unlock(&mp) ;

  return iret;
};
#endif

//-------------------------------------------

#endif



