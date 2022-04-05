#ifndef MSG_STRUCT_H_
#define MSG_STRUCT_H_

#include <vxWorks.h>

#include <iccp.h>

// incoming doorbells to the MZ
#define DBEL_ANNCE_SL3_RES1	31	//W1->M
#define DBEL_ANNCE_SL3_RES2	30	//W2->M
#define DBEL_SND_SL3		29	//SB->M
#define DBEL_ANNCE_FMT_DTA1	28	//W1->M
#define DBEL_ANNCE_FMT_DTA2	27	//W2->M
#define DBEL_FMT_DTA		26	//SB->M, M->W
#define DBEL_SND_DTA		25	//SB->M
#define DBEL_CNFRM_RLS1		24	//W1->M, data in MBOX4
#define DBEL_CNFRM_RLS2		23	//W2->M, data in MBOX5

#define DBEL_DMA_DONE		22

#define DBEL_MZ_EMUL		11
#define DBEL_COMMAND		10	/* any slow command */
#define DBEL_REPORT		9	/* print report */
#define DBEL_CONS_DTA		0


// incoming mailboxes on the MZ PLX
#define MBOX_MZ_DMA_DONE1	0	//W1->M
#define MBOX_MZ_DMA_DONE2	1	//W2->M
#define MBOX_MZ_RLS_TOKEN	2	//SB->M, M->W

// incoming but non-IRQ MBOXes on the MZ
#define MBOX_MZ_CNFRM_RLS1	4
#define MBOX_MZ_CNFRM_RLS2	5

// outgoing doorbells - from the MZ to SB
#define DBEL_SB_MON_DTA		31
#define DBEL_SB_LOG_DTA		30
#define DBEL_SB_COMMAND		29
#define DBEL_SB_DEATH		28	/* no parameters - announces immediate death of MZ */
#define DBEL_SB_CONS_DTA	0

// alternative outgoing doorbells on the Monarch PLX
#define DBEL_SB_ANNCE_SL3_RES	1
#define DBEL_SB_ANNCE_FMT_DTA	2
#define DBEL_SB_CNFRM_RLS_TKN	3
#define DBEL_SB_CNFRM_SEND	4	// used to inform the SB


// outgoing mailboxes on the Universe - from Monarch to SB
#define MBOX_SB_ANNCE_SL3_RES	0
#define MBOX_SB_ANNCE_FMT_DTA	1
#define MBOX_SB_CNFRM_RLS_TKN	2
#define MBOX_SB_CNFRM_SEND	3	// used to inform the SB

// reasons for dying
#define MBOX_MZ_OUT_DEATH	0

#define DEATH_DELAY_QUE_FULL    1
#define DEATH_BERR              2
#define DEATH_OUT_OF_MEM        3
#define DEATH_OUT_OF_RSRC       4
#define DEATH_RSRC_MISSING      5
#define DEATH_BAD_STATE         6



//#pragma align 1
#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && I960 == 1
#pragma pack 2
#endif


typedef struct { 
	UINT32 token : 12 ;
	UINT32 status : 4 ;
	UINT32 src : 4 ;
	UINT32 dst : 4 ;
	UINT32 cmd : 8 ;
} msg_0 ;

typedef struct { 
	UINT32 token : 12 ;
	UINT32 status : 4 ;
	UINT32 src : 4 ;
	UINT32 dst : 4 ;
	UINT32 cmd : 8 ;

	UINT32 d[1] ;

} msg_1 ;

typedef struct { 
	UINT32 token : 12 ;
	UINT32 status : 4 ;
	UINT32 src : 4 ;
	UINT32 dst : 4 ;
	UINT32 cmd : 8 ;

	UINT32 d[2] ;

} msg_2 ;

typedef struct {
	UINT32 token : 12 ;
	UINT32 status : 4 ;
	UINT32 src : 4 ;
	UINT32 dst : 4 ;
	UINT32 cmd : 8 ;

	UINT32 d[13] ;
} msg_A ;


//#pragma align 0

#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && I960 == 1
#pragma pack 0
#endif

//#pragma align 4


struct mon_dta {
	UINT32 len ;
	UINT32 d[256] ;
} ;

struct console_dta { ;
	UINT32 len ;
	UINT8 d[256] ;
} ;

struct log_dta {
	UINT32 len ;
	UINT8 d[256] ;
} ;


#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && I960 == 1
#pragma pack 4 
#endif

/* resides in the Mezzanine's memory */
struct mzMsgStruct {	
	msg_1 annce_sl3_res[2] ;		/* from the Workers */
	msg_1 annce_fmt_dta[2] ;		/* from the Workers */

	msg_1 snd_sl3 ;			/* from the SB */
	msg_1 fmt_dta ;			/* from the SB , MSb '1' ->requires checksum */
	msg_1 snd_dta ;			/* from SB */

	msg_A cmd ;			/* any other slow command - from SB */

	struct console_dta console_dta;		/* from SB */
	struct mon_dta mon_dta ;		/* from SB */
} ;



/* resides in SB's memory */
struct rbMsgStruct {
	/* fast commands via mailboxes on the Universe */

	msg_1 annce_sl3_res ;		/* from the Monarch - use MBOX0 */	
	msg_1 annce_fmt_dta ;		/* from the Monarch - use MBOX1 */
	msg_0 cnfrm_rel_tkn ;		/* from the Monarch - use MBOX2 */
	msg_0 sb_dma ;			/* from Monarch - use MBOX3     */

	/* various slow commands via doorbells */


	struct mon_dta mon_dta[3] ;		/* from MZ */
	struct log_dta log_dta[3] ;		/* from MZ */
	struct console_dta console_dta[3] ;	/* from Mz */
	msg_A cmd[3] ;
} ;


//#pragma align 0

#if __GNUC__ == 2 && __GNUC_MINOR__ < 96 && I960 == 1
#pragma pack 0
#endif

#if (CPU == I960HX)

// src/dest
#define MZ_DST_SB       3
#define MZ_DST_PROC     4
#define MZ_DST_FMT      5
#define MZ_DST_DMAER    6

extern volatile struct rbMsgStruct *rbMsgStruct ;
extern volatile struct mzMsgStruct *mzMsgStruct[3], *mzMyMsgStruct ;

extern int sendMsg(msg_A *m, void (*func)(UINT32), UINT32 param) ;
extern int msg(int dst, msg_A *mp) ;


#endif


#ifdef DBG_MSG_STRUCT
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void sizer(void) ;

#ifdef __cplusplus
}
#endif


void sizer(void)
{

	printf("msg_0 %d\n",sizeof(msg_0));
	printf("msg_1 %d\n",sizeof(msg_1));
	printf("msg_2 %d\n",sizeof(msg_2));

	printf("msg_A %d\n",sizeof(msg_A)) ;
	printf("mon_dta %d\n",sizeof(struct mon_dta));
	printf("console_dta %d\n",sizeof(struct console_dta)) ;
	printf("log_dta %d\n",sizeof(struct log_dta)) ;



	printf("mzMsgStruct %d\n",sizeof(struct mzMsgStruct)) ;
	printf("rbMsgStruct %d\n",sizeof(struct rbMsgStruct)) ;



	printf("msg_0 %d\n",__alignof__(msg_0));
	printf("msg_1 %d\n",__alignof__(msg_1));
	printf("msg_2 %d\n",__alignof__(msg_2));
	printf("msg_A %d\n",__alignof__(msg_A)) ;

	printf("mon_dta %d\n",__alignof__(struct mon_dta));
	printf("console_dta %d\n",__alignof__(struct console_dta)) ;

	printf("log_dta %d\n",__alignof__(struct log_dta)) ;


	printf("mzMsgStruct %d\n",__alignof__(struct mzMsgStruct)) ;
	printf("rbMsgStruct %d\n",__alignof__(struct rbMsgStruct)) ;

	return ;
}

#endif


#endif /* _MSG_STRUCT_H_ */
