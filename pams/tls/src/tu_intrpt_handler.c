/*  tu_intrpt_handler.c  handles keyboard interupts
 *
 */

void tu_intr_set();
void tu_intr_reset();
void tu_intr_sighandler();
int tu_interrupt();

/****** irix  *****************************************************/
#ifdef irix


void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>

static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
/*
    old_sig_func = ( char * ) sigset( SIGINT, tu_intr_sighandler );
*/
    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );
    was_interrupted = 0;
}


void tu_intr_set_() 
{
    tu_intr_set();
}


void tu_intr_reset() 
{
/*
    (void) sigset( SIGINT, (void(*))old_sig_func );
*/
    was_interrupted = 0;
}


void tu_intr_reset_() 
{
    tu_intr_reset();
}

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
    sigrelse( sig );

}

int tu_interrupt() 
{
    return was_interrupted;
}

int tu_interrupt_() 
{
    return tu_interrupt();
}


#endif /* irix */

/****** sun4os5  *****************************************************/
#ifdef sun4os5


void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>

static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );
    was_interrupted = 0;
}


void tu_intr_set_() 
{
    tu_intr_set();
}


void tu_intr_reset() 
{
    was_interrupted = 0;
}


void tu_intr_reset_() 
{
    tu_intr_reset();
}

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
    sigrelse( sig );

}

int tu_interrupt() 
{
    return was_interrupted;
}

int tu_interrupt_() 
{
    return tu_interrupt();
}


#endif /* sun4os5 */

/****** aix  ******************************************************/
#ifdef aix


void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>


static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
    int newmask, newestmask;
    
    old_sig_mask = sigblock( sigmask(SIGINT) );
    newestmask = old_sig_mask & ~sigmask( SIGINT);
    
    newmask = sigsetmask( newestmask );

    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );
    was_interrupted = 0;
}

void tu_intr_set_() 
{
    tu_intr_set();
}


void tu_intr_reset() 
{
    sigsetmask( old_sig_mask );
    (void ) signal( SIGINT, old_sig_func );
    was_interrupted = 0;
}

void tu_intr_reset_() 
{
    tu_intr_reset();
}

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
}

int tu_interrupt() 
{
    return was_interrupted;
}

int tu_interrupt_() 
{
    return tu_interrupt();
}

#endif /* aix */

/****** sun4  *** should be like aix **********************************/
#ifdef sun4


void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>


static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
    int newmask, newestmask;
    
    old_sig_mask = sigblock( sigmask(SIGINT) );
    newestmask = old_sig_mask & ~sigmask( SIGINT);
    
    newmask = sigsetmask( newestmask );

    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );
    was_interrupted = 0;
}

void tu_intr_set_() 
{
    tu_intr_set();
}


void tu_intr_reset() 
{
    sigsetmask( old_sig_mask );
    (void ) signal( SIGINT, old_sig_func );
    was_interrupted = 0;
}

void tu_intr_reset_() 
{
    tu_intr_reset();
}

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
}

int tu_interrupt() 
{
    return was_interrupted;
}

int tu_interrupt_() 
{
    return tu_interrupt();
}

#endif /* sun4 */

/****** hpux  *** should be like aix **********************************/
#ifdef hpux


void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>


static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
    int newmask, newestmask;
    
    old_sig_mask = sigblock( sigmask(SIGINT) );
    newestmask = old_sig_mask & ~sigmask( SIGINT);
    
    newmask = sigsetmask( newestmask );

    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );
    was_interrupted = 0;
}

void tu_intr_set_() 
{
    tu_intr_set();
}


void tu_intr_reset() 
{
    sigsetmask( old_sig_mask );
    (void ) signal( SIGINT, old_sig_func );
    was_interrupted = 0;
}

void tu_intr_reset_() 
{
    tu_intr_reset();
}

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
}

int tu_interrupt() 
{
    return was_interrupted;
}

int tu_interrupt_() 
{
    return tu_interrupt();
}

#endif /* hpux */

/****** VMS *******************************************************/
#ifdef VMS

#include descrip
#include iodef
static int i_chan1 = 0;


static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
    int i_stat;
    int i_func, i_mask[2];
    short i_iosb[4];
    static $DESCRIPTOR( chan_name, "sys$command");

    if(i_chan1 == 0)
    {
	i_stat = sys$assign( &chan_name, &i_chan1,0,0);
    }
    i_func = IO$_SETMODE | IO$M_OUTBAND;
    i_mask[0] = 0;
    i_mask[1] = 0x00000080;
    i_stat = sys$qiow(0,i_chan1,i_func,i_iosb,0,0,
		    tu_intr_sighandler,i_mask,
		    0,0,0,0,0);
    was_interrupted = 0;
}


void tu_intr_reset() 
{
    was_interrupted = 0;
}


void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;
}

int tu_interrupt() 
{
    return was_interrupted;
}

#endif /* VMS */
