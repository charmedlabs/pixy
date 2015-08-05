/*
 * GENERATED FILE - DO NOT EDIT
 * (c) Code Red Technologies Ltd, 2008-2015
 * (c) NXP Semiconductors 2013-2015
 * Linker script memory definitions
 * Created from LinkMemoryTemplate
 * By LPCXpresso v7.6.2 [Build 326] [2015-02-02]  on Wed Apr 01 17:57:00 CDT 2015)
*/

MEMORY
{
  /* Define each memory region */
  RamAHB16 (rwx) : ORIGIN = 0x20008000, LENGTH = 0x4000 /* 16K bytes */
  RamAHB32 (rwx) : ORIGIN = 0x20000000, LENGTH = 0x8000 /* 32K bytes */
  RamLoc128 (rwx) : ORIGIN = 0x10000000, LENGTH = 0x20000 /* 128K bytes */
  RamLoc72 (rwx) : ORIGIN = 0x10080000, LENGTH = 0x12000 /* 72K bytes */
  RamAHB_ETB16 (rwx) : ORIGIN = 0x2000c000, LENGTH = 0x4000 /* 16K bytes */


}
  /* Define a symbol for the top of each memory region */
  __top_RamAHB16 = 0x20008000 + 0x4000;
  __top_RamAHB32 = 0x20000000 + 0x8000;
  __top_RamLoc128 = 0x10000000 + 0x20000;
  __top_RamLoc72 = 0x10080000 + 0x12000;
  __top_RamAHB_ETB16 = 0x2000c000 + 0x4000;



