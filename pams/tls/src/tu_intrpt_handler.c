/*  tu_intrpt_handler.c  handles keyboard interupts
 *
 */

void tu_intr_set();
void tu_intr_reset();
void tu_intr_sighandler();
int tu_interrupt();

#ifndef VMS

void tu_intr_set_();
void tu_intr_reset_();
int tu_interrupt_();

#include <signal.h>

#else

#include descrip
#include iodef
static int i_chan1 = 0;

#endif /* VMS */

static char *old_sig_func;
static int old_sig_mask = 0;
static int was_interrupted = 0;


void tu_intr_set() 
{
#ifndef VMS

#ifndef IRIX
    int newmask, newestmask;
    
    old_sig_mask = sigblock( sigmask(SIGINT) );
    newestmask = old_sig_mask & ~sigmask( SIGINT);
    
    newmask = sigsetmask( newestmask );

#ifdef DEB
    printf(" SIGINT %x, sigmask(SIGINT) %x\n", SIGINT, sigmask(SIGINT));
    printf(" newestmask %x\n",newestmask);
    
    printf("old mask %x,  new mask %x\n", old_sig_mask, newmask);
#endif /* DEB */

    old_sig_func = ( char * ) signal( SIGINT, tu_intr_sighandler );

#else /*  IRIX */
    old_sig_func = ( char * ) sigset( SIGINT, tu_intr_sighandler );
#endif /* IRIX */

#else /* VMS */
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

#endif /* not VMS elif VMS */
    was_interrupted = 0;
}

#ifndef VMS

void tu_intr_set_() 
{
    tu_intr_set();
}

#endif /* not VMS */

void tu_intr_reset() 
{
#ifndef VMS

#ifndef IRIX
    sigsetmask( old_sig_mask );
    (void ) signal( SIGINT, old_sig_func );

#else /* IRIX */

    (void) sigset( SIGINT, (void(*))old_sig_func );
#endif /* IRIX */

#endif /* not VMS */
    was_interrupted = 0;
}

#ifndef VMS

void tu_intr_reset_() 
{
    tu_intr_reset();
}

#endif /* not VMS */

void tu_intr_sighandler( sig ) 
     int sig;
{
    was_interrupted++;

#ifdef IRIX
    sigrelse( sig );
#endif /* IRIX */

#ifdef DEB
    printf("was_interrupted = %d\n",was_interrupted);
#endif /* DEB */

}

int tu_interrupt() 
{
#ifdef DEB
    printf("tu_interrupt %d\n",was_interrupted);
#endif /* DEB */
    return was_interrupted;
}

#ifndef VMS

int tu_interrupt_() 
{
    return tu_interrupt();
}
#endif /* not VMS */

