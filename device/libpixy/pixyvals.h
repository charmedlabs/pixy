#ifndef PIXYVALS_H
#define PIXYVALS_H

#define XTALFREQ                 12000000
#define CLKFREQ                  204000000
#define CLKFREQ_MS               (CLKFREQ/1000)
#define CLKFREQ_US               (CLKFREQ/1000000)

// SRAM banks
#define SRAM0_LOC                0x10000000
#define SRAM0_SIZE               0x20000
#define SRAM1_LOC                0x10080000
#define SRAM1_SIZE               0x12000
#define SRAM2_LOC                0x20000000
#define SRAM2_SIZE               0x8000
#define SRAM3_LOC                0x20008000
#define SRAM3_SIZE               0x4000
#define SRAM4_LOC                0x2000c000
#define SRAM4_SIZE               0x4000

#endif
