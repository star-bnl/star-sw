#ifndef _UNI_DMA_LIB_H
#define _UNI_DMA_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <vxWorks.h>

#define UNI_DMA_PCI_TO_VME	0x80000000
#define UNI_DMA_VME_TO_PCI	0x00000000
#define UNI_DMA_MBLT		0x00C00000
#define UNI_DMA_BLT		0x00800000


extern int uniDmaInit(void) ;
extern int uniDmaInitMaster(void) ;
extern int uniDmaGo(UINT32 *pci, UINT32 *vme, UINT32 bytes, UINT32 cntrl) ;
extern int uniDmaStart(UINT32 *pci, UINT32 *vme, UINT32 bytes, UINT32 cntrl) ;
extern int uniDmaWait(void) ;
extern int uniDma64 ;

#ifdef __cplusplus
}
#endif
#endif	/* _UNI_DMA_LIB_H */
