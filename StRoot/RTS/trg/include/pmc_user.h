#define PMC_BASE_ADDR        0x55000000

// Read Registers
#define PMC_STATUS           0x55000000
#define PMC_INTERRUPTS       0x55000004
#define PMC_BUILD_WORD       0x55000014
#define PMC_BUILD_WORD_AVAIL 0x55000018
#define PMC_IN_COUNT         0x5500001c
#define PMC_OUT_COUNT        0x55000020
#define PMC_RD_DSM_COUNTER   0x55000034

// Write Registers
#define PMC_RESET            0x55000000
#define PMC_BUILD_EVT_WORD   0x55000008
#define PMC_OUT_SETUP        0x55000010
#define PMC_START_DMA        0x55000014
#define PMC_CLEAR_INT        0x55000024
#define PMC_DMA_ADDR         0x5500002c
#define PMC_DMA_SIZE         0x55000030
#define PMC_DO_DMA           0x55000034
#define PMC_TX_SPEED         0x55000040

#define PMC_2_BASE_ADDR        0x40000000

// Read Registers
#define PMC_2_STATUS           0x40000000
#define PMC_2_INTERRUPTS       0x40000004
#define PMC_2_BUILD_WORD       0x40000014
#define PMC_2_BUILD_WORD_AVAIL 0x40000018
#define PMC_2_IN_COUNT         0x4000001c
#define PMC_2_OUT_COUNT        0x40000020
#define PMC_2_RD_DSM_COUNTER   0x40000034

// Write Registers
#define PMC_2_RESET            0x40000000
#define PMC_2_BUILD_EVT_WORD   0x40000008
#define PMC_2_OUT_SETUP        0x40000010
#define PMC_2_START_DMA        0x40000014
#define PMC_2_CLEAR_INT        0x40000024
#define PMC_2_DMA_ADDR         0x4000002c
#define PMC_2_DMA_SIZE         0x40000030
#define PMC_2_DO_DMA           0x40000034
#define PMC_2_TX_SPEED           0x40000040

// Status Bits:
// Bit                       Description
//  0                        0 = Transmitter locked
//  1                        0 = Receiver ready
//  2                        1 = Receiver Error
//  3                        1 = Receiver Sync Slip
//  4                        * 1 = Incoming Build Evt FIFO Full
//  5                        * 1 = Outgoing DSM FIFO Full
//  6                        * 1 = Outgoing Build Evt Error
//  7                        * 1 = Outgoing DSM Data Error
//  8                        0 = Incoming Build Evt FIFO Empty
//  9                        1 = One more Build Evt word available (not in FIFO)
// 10                        0 = Outgoing DSM FIFO Empty
// 11                        Unused
// 12                        1 = Sending Build Evt
// 13                        1 = Sending DSM Data
// 14                        1 = Transmitting DSM data to concentrator
//     * -> bit is only cleared on board reset
