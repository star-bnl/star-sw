#ifndef _ITPC_CORE_H_
#define _ITPC_CORE_H_

extern void itpc_sampa_to_rowpad(int id, int sampa, int ch, int &row, int &pad) ;
extern void itpc_rowpad_to_id(int row, int pad, int &id, int &pin)  ;
extern  int itpc_altro_to_ifee(int altro) ;
extern void itpc_altro_to_rowpad(int altro, int ch, int odd, int &row, int &pad) ;

#endif
