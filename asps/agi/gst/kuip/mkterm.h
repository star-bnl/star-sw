/*
 * $Id: mkterm.h,v 1.1 1998/09/18 00:29:15 fisyak Exp $
 *
 * $Log: mkterm.h,v $
 * Revision 1.1  1998/09/18 00:29:15  fisyak
 * Add break off for Linux
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */

#define ESCAPE          "#@"


typedef void (*KxtermActionProc)(
#ifndef NO_PROTOTYPES
    char**              /* params */,
    int                 /* num_params */
#endif
);

typedef struct _KxtermActionsRec{
    char               *string;
    KxtermActionProc    proc;
} KxtermActionsRec;

typedef KxtermActionsRec  *KxtermActionList;

extern void             kxterm_add_actions(
                                     KxtermActionList);
extern void             handle_kxterm_action(
                                     char *);
extern void             send_kxterm_cmd(
                                     char**);
extern void             send_single_kxterm_cmd(
                                     char*);

