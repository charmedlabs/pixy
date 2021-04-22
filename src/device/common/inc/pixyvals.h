//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#ifndef PIXYVALS_H
#define PIXYVALS_H

#define XTALFREQ                 12000000
#define CLKFREQ                  204000000
#define CLKFREQ_MS               (CLKFREQ/1000)
#define CLKFREQ_US               (CLKFREQ/1000000)

// SRAM banks
#define SRAM0_LOC                0x10000000   // Warning: Program for M4
#define SRAM0_SIZE               0x20000
#define SRAM1_LOC                0x10080000   // Store 320x200 image frame
#define SRAM1_SIZE               0x12000
#define SRAM2_LOC                0x20000000   // Warning: Stack and heap for M4
#define SRAM2_SIZE               0x8000
#define SRAM3_LOC                0x20008000   // Warning: Program and stack for M0
#define SRAM3_SIZE               0x4000
#define SRAM4_LOC                0x2000c000   // Shared memory between M0 and M4
#define SRAM4_SIZE               0x4000

#define MEM_USB_FRAME_LOC        SRAM1_LOC
#define MEM_USB_FRAME_SIZE       SRAM1_SIZE
#define MEM_SD_FRAME_LOC         (MEM_USB_FRAME_LOC + 64) // Leave room in front for USB header
#define MEM_QQ_LOC               SRAM4_LOC
#define MEM_QQ_SIZE              (0x3c00)
#define MEM_SM_LOC               (SRAM4_LOC + MEM_QQ_SIZE)
#define MEM_SM_SIZE              (SRAM4_SIZE - MEM_QQ_SIZE)
#define MEM_SM_BUFSIZE           (MEM_SM_SIZE - 4)

#endif
