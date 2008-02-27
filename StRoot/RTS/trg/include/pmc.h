#ifndef _PMC_H_INCLUDED_

#include "pmc_user.h"

extern unsigned int PMCDebug;

struct build_evt {
  unsigned int token;
  unsigned int dsm_addr;
  unsigned int prepost;
};

/* Initialize PMC card                                *
 *  RETURNS: 0                                        */
int PMC_init(unsigned int enable, void * (*handler)(int));

/* Reset PMC card and reset counters                  *
 *  RETURNS: 0  properly reset                        *
 *           1  PMC not intialized                    *
 *           2  error resetting                       */
int PMC_reset(int debug, int card);


/* Read PMC status                                    *
 *  RETURNS: status                                   */
int PMC_status(int debug, int card);


/* Add 'handler' to list of interrupt                 *
 * handlers for PMC card                              *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_set_interrupt_handler(void * (*handler)(int), int debug, int card);


/* Clear a pending interrupt with the PMC card        *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_clear_interrupt(int debug);


/* Uninstall PMC card interrupt handler               *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_disable_interrupt_handler(int debug, int card);

/* start, setup and run a dma transfer from local     *
 *   buffer to PMC card.                              */
int PMC_run_dma(unsigned int addr, unsigned int size, int debug);


/* Start a DSM packet                                 *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_start_dsm_data(int debug, int card);


/* Set address and size (in 32bit words) of           *
 * local buffer to be transfered to PMC card          *
 *  RETURNS:  0                                       *
 *            1  PMC not intialized                   */
int PMC_setup_dma(unsigned int addr, unsigned int size, int debug, int card);


/* DMA transfer local buffer to PMC card and finish   *
 * DSM packet (control end word). This must be called *
 * after PMC_start_dsm_data and PMC_setup_dma         *
 *  RETURNS: 0   successful DMA transfer              *
 *           1   PMC not intialized                   *
 *           2   DMA transfer not setup               *
 *           3   DMA Error                            */
int PMC_do_dma(int debug, int card);


/* Read interrupts enabled bit                        *
 *  RETURNS: 0   interrupts disabled                  *
 *           1   interrupts enabled                   */
int PMC_ints_enabled(int debug, int card);


/* Read incoming build_evt packet counter             *
 *  RETURNS: build_evt packet counter                 */
int PMC_incoming_counter(int debug, int card);


/* Read outgoing build_evt packet counter             *
 *  RETURNS: outgoing build_evt packet counter        */
int PMC_outgoing_counter(int debug, int card);


/* Read DSM packet counter                            *
 *  RETURNS: DSM outgoing packet counter              */
int PMC_dsm_counter(int debug, int card);


/* Enable PMC interrupts                              *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_int_enable(int debug, int card);


/* Disable PMC interrupts                             *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_int_disable(int debug, int card);


/* Clear PMC counters                                 *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_clear_counters(int debug, int card);

/* send full build_evt command                        *
 *  RETURNS: 0 on success                             *
 *           1 on send error                          */
int PMC_send_build_evt(struct build_evt *build_evt_cmd, int debug);

/* read full build_evt command                        *
 *   RETURNS: 0 on success                            *
 *            1 on read error                         */
int PMC_read_build_evt(struct build_evt *build_evt_cmd, int debug);

/* Read build_evt available bit                       *
 *  RETURNS: 0   no build_evt words available         *
 *           1   build_evt words available            */
int PMC_build_evt_word_avail(int debug);


/* Read next build_evt word                           *
 *  RETURNS: build_evt word if available              *
 *           0 no build_evt words available           */
int PMC_read_build_evt_word(int debug, int card);

/* Start a build_evt packet                           *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_start_build_evt(int debug, int card);


/* Send build_evt word                                *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_send_build_evt_word(unsigned int send_word, int debug, int card);


/* Finish a build_evt packet                          *
 *  RETURNS: 0                                        *
 *           1  PMC not intialized                    */
int PMC_end_build_evt(int debug, int card);



/* Read  PMC status, counters and build_evt available *
 *  RETURNS: status                                   */
int PMC_status_all(int debug, int card);




inline unsigned int swap(unsigned int var) {
  return
    (var&0xff000000) >> 24 |
    (var&0x00ff0000) >> 8  |
    (var&0x0000ff00) << 8  |
    (var&0x000000ff) << 24;
}


#define _PMC_H_INCLUDED_
#endif






