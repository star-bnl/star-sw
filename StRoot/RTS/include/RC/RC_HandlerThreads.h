#ifndef _RC_HANDLER_THREADS_H
#define _RC_HANDLER_THREADS_H

#define THREAD_TYPE_SENDER 1
#define THREAD_TYPE_READER 2
#define THREAD_TYPE_CLIENT_SENDER 3
#define THREAD_TYPE_CLIENT_READER 4
#define THREAD_TYPE_OTHER 5

struct HandlerThread
{
  int fd;
  int seq;
  int status;
  int port;
  int thr_addr;
  int crate_addr;
  int type;
};

struct HandlerThreads
{
  HandlerThread threads[MAX_THREADS];
  int seq;
};


#endif
