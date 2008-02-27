#ifndef _MSG_Q_LIB_
#define _MSG_Q_LIB_

/**********************************************/
/* msgQCreate:                                */
/*      Only to be called by the receiving    */
/*      process.  It's function is            */
/*         (1) Create a lock that tells other */
/*             processes its still listening  */
/*         (2) Flush the queue if it has junk */
/*             left in it.                    */
/*                                            */
/* return values:  0 OK.                      */
/*                 -1 error                   */
/**********************************************/
int msgQCreate(int task, int nmsgs, int msglen);


/**********************************************/
/* msgQSend:                                  */
/*      non-blocking call                     */
/*      if test is set, tests for listener    */
/*      otherwise, not.                       */
/*                                            */
/* return values:  0 OK.                      */
/*                 -1 error                   */
/**********************************************/
int msgQSend(int task, char *buff, int size, int *line_number=0);

/**********************************************/
/* msgQReceive:                               */
/*      Blocking call                         */
/* return values:  length of msg in bytes     */
/*                 -1 error                   */
/**********************************************/
int msgQReceive(int task, char *buff, int size);

/**********************************************/
/* returns size of Q                          */
/**********************************************/
int msgQSize(int task);

/**********************************************/
/* returns messages in Q                      */
/* ********************************************/
int msgQUsed(int task);

/**********************************************/
/* returns free buffers in Q                  */
/**********************************************/
int msgQFree(int task);

/**********************************************/
/* Check if receiver exists for message queue */
/* Returns 0 if no task,                      */
/*         pid if task...                     */
/**********************************************/
int msgQTest(int task);
#endif
