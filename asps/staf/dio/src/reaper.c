#include <sys/wait.h>
#include <signal.h>
static void reaper();
void signal_reaper();

/*-------------------------------------------------------------------*/
void signal_reaper() {
	signal(SIGCHLD, reaper);/* eliminate zombies */
}

/**********************************************************************
*
* reaper - prevent zombies
*
* "reap" child processes that have exited.
* Copy from sun IPC tutorial chapter 9.
*
* RETURNS:
*/
static void reaper()
{
        int status;

        while (wait3(&status, WNOHANG, 0) > 0)
                continue;
}

