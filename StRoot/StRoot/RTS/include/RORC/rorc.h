#ifndef _RORC_H_
#define _RORC_H_

/*
 ***************************************************
 * rorc.h
 *
 * Header file for pRORC and D-RORC cards
 *
 * last updated: 17/04/2007
 * written by: Ervin Denes
 ***************************************************
 */

#define DEV_RORC "/dev/prorc"
#define LOCK_DIR "/dev/prorc/"
#define MAX_DEVICE 16
#define MAX_CHANNEL 2
#define MAX_DDG 12

/*
 * Ioctls ******************************************
 */

#define RORCMAGIC 0xdc
/*
 * get number of RORC's
 */

#define RORC_COUNT      _IO(RORCMAGIC, 0)

/*
 * read PCI configuration infos
 */

#define RORC_R_VENDOR   _IO(RORCMAGIC, 10) /* get pci info */
#define RORC_R_DEVICE   _IO(RORCMAGIC, 11) /* get pci info */
#define RORC_R_IRQ      _IO(RORCMAGIC, 12) /* get pci info */
#define RORC_R_REVISION _IO(RORCMAGIC, 13) /* pRORC or D-RORC */
#define RORC_R_BAR0     _IO(RORCMAGIC, 14) /* get pci info */
#define RORC_R_BAR1     _IO(RORCMAGIC, 15) /* get pci info */
#define RORC_R_ROM      _IO(RORCMAGIC, 16) /* get pci info */
#define RORC_R_PCI_CFG  _IO(RORCMAGIC, 19) /* get pci info */

/* pRORC ---------------------------------------------------------*/

/*
 * reset functions
 */

#define PRORC_RESET      _IO(RORCMAGIC, 20) /* reset pRORC */
#define PRORC_FF_RESET   _IO(RORCMAGIC, 21) /* reset FIFO */
#define PRORC_MB_RESET   _IO(RORCMAGIC, 22) /* reset mailbox flags */
#define PRORC_SET_WTE    _IO(RORCMAGIC, 23) /* set write transfer enable */
#define PRORC_SET_WMS    _IO(RORCMAGIC, 24) /* set write management scheme */
#define PRORC_SET_WVR    _IO(RORCMAGIC, 25) /* set write versus read priority */
#define PRORC_SET_RTE    _IO(RORCMAGIC, 26) /* set read transfer enable */
#define PRORC_SET_RMS    _IO(RORCMAGIC, 27) /* set read management scheme */
#define PRORC_SET_RVW    _IO(RORCMAGIC, 28) /* set read versus write priority */

/* Driver version-------------------------------------------------*/

#define RORC_R_DR_MAJ   _IO(RORCMAGIC, 30) /* get RORC driver version major */
#define RORC_R_DR_MIN   _IO(RORCMAGIC, 31) /* get RORC driver version minor */
#define RORC_R_DR_REL   _IO(RORCMAGIC, 32) /* get RORC driver version release */

/*
 * channel usage
 */

#define RORC_R_CHAN   _IO(RORCMAGIC, 40) /* check if channel is used */
#define RORC_SET_CHAN _IO(RORCMAGIC, 41) /* set channel used */
#define RORC_RES_CHAN _IO(RORCMAGIC, 42) /* set channel free */

/*
 * serial number
 */

#define RORC_R_SERIAL  _IO(RORCMAGIC, 50) /* read serial number */

#endif /* _RORC_H */
