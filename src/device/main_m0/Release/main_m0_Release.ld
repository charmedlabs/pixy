/*
 * GENERATED FILE - DO NOT EDIT
 * (c) Code Red Technologies Ltd, 2008-13
 * (c) NXP Semiconductors 2013-2015
 * Generated linker script file for LPC4330-M0
 * Created from generic_c.ld (LPCXpresso v7.6 (2 [Build 326] [2015-02-02] ))
 * By LPCXpresso v7.6.2 [Build 326] [2015-02-02]  on Wed Apr 01 17:57:00 CDT 2015
 */


INCLUDE "main_m0_Release_lib.ld"
INCLUDE "main_m0_Release_mem.ld"

ENTRY(ResetISR)

SECTIONS
{

    /* MAIN TEXT SECTION */    
    .text : ALIGN(4)
    {
        FILL(0xff)
        __vectors_start__ = ABSOLUTE(.) ;
        KEEP(*(.isr_vector))
        
        /* Global Section Table */
        . = ALIGN(4) ;
        __section_table_start = .;
        __data_section_table = .;
        LONG(LOADADDR(.data));
        LONG(    ADDR(.data));
        LONG(  SIZEOF(.data));
        LONG(LOADADDR(.data_RAM2));
        LONG(    ADDR(.data_RAM2));
        LONG(  SIZEOF(.data_RAM2));
        LONG(LOADADDR(.data_RAM3));
        LONG(    ADDR(.data_RAM3));
        LONG(  SIZEOF(.data_RAM3));
        LONG(LOADADDR(.data_RAM4));
        LONG(    ADDR(.data_RAM4));
        LONG(  SIZEOF(.data_RAM4));
        LONG(LOADADDR(.data_RAM5));
        LONG(    ADDR(.data_RAM5));
        LONG(  SIZEOF(.data_RAM5));
        __data_section_table_end = .;
        __bss_section_table = .;
        LONG(    ADDR(.bss));
        LONG(  SIZEOF(.bss));
        LONG(    ADDR(.bss_RAM2));
        LONG(  SIZEOF(.bss_RAM2));
        LONG(    ADDR(.bss_RAM3));
        LONG(  SIZEOF(.bss_RAM3));
        LONG(    ADDR(.bss_RAM4));
        LONG(  SIZEOF(.bss_RAM4));
        LONG(    ADDR(.bss_RAM5));
        LONG(  SIZEOF(.bss_RAM5));
        __bss_section_table_end = .;
        __section_table_end = . ;
        /* End of Global Section Table */
        

        *(.after_vectors*)
        
    } >RamAHB16
    
    .text : ALIGN(4)    
    {
         *(.text*)
        *(.rodata .rodata.* .constdata .constdata.*)
        . = ALIGN(4);
        
    } > RamAHB16

    /*
     * for exception handling/unwind - some Newlib functions (in common
     * with C++ and STDC++) use this. 
     */
    .ARM.extab : ALIGN(4)
    {
    	*(.ARM.extab* .gnu.linkonce.armextab.*)
    } > RamAHB16
    __exidx_start = .;
    
    .ARM.exidx : ALIGN(4)
    {
    	*(.ARM.exidx* .gnu.linkonce.armexidx.*)
    } > RamAHB16
    __exidx_end = .;
    
    _etext = .;
        
    
    /* DATA section for RamAHB32 */
    .data_RAM2 : ALIGN(4)
    {
       FILL(0xff)
       PROVIDE(__start_data_RAM2 = .) ;
       *(.ramfunc.$RAM2)
       *(.ramfunc.$RamAHB32)
    	*(.data.$RAM2*)
    	*(.data.$RamAHB32*)
       . = ALIGN(4) ;
       PROVIDE(__end_data_RAM2 = .) ;
    } > RamAHB32 AT>RamAHB16
    
    /* DATA section for RamLoc128 */
    .data_RAM3 : ALIGN(4)
    {
       FILL(0xff)
       PROVIDE(__start_data_RAM3 = .) ;
       *(.ramfunc.$RAM3)
       *(.ramfunc.$RamLoc128)
    	*(.data.$RAM3*)
    	*(.data.$RamLoc128*)
       . = ALIGN(4) ;
       PROVIDE(__end_data_RAM3 = .) ;
    } > RamLoc128 AT>RamAHB16
    
    /* DATA section for RamLoc72 */
    .data_RAM4 : ALIGN(4)
    {
       FILL(0xff)
       PROVIDE(__start_data_RAM4 = .) ;
       *(.ramfunc.$RAM4)
       *(.ramfunc.$RamLoc72)
    	*(.data.$RAM4*)
    	*(.data.$RamLoc72*)
       . = ALIGN(4) ;
       PROVIDE(__end_data_RAM4 = .) ;
    } > RamLoc72 AT>RamAHB16
    
    /* DATA section for RamAHB_ETB16 */
    .data_RAM5 : ALIGN(4)
    {
       FILL(0xff)
       PROVIDE(__start_data_RAM5 = .) ;
       *(.ramfunc.$RAM5)
       *(.ramfunc.$RamAHB_ETB16)
    	*(.data.$RAM5*)
    	*(.data.$RamAHB_ETB16*)
       . = ALIGN(4) ;
       PROVIDE(__end_data_RAM5 = .) ;
    } > RamAHB_ETB16 AT>RamAHB16
    
    /* MAIN DATA SECTION */
    

    .uninit_RESERVED : ALIGN(4)
    {
        KEEP(*(.bss.$RESERVED*))
        . = ALIGN(4) ;
        _end_uninit_RESERVED = .;
    } > RamAHB16

	
	/* Main DATA section (RamAHB16) */
	.data : ALIGN(4)
	{
	   FILL(0xff)
	   _data = . ;
	   *(vtable)
	   *(.ramfunc*)
	   *(.data*)
	   . = ALIGN(4) ;
	   _edata = . ;
	} > RamAHB16 AT>RamAHB16

    /* BSS section for RamAHB32 */
    .bss_RAM2 : ALIGN(4)
    {
       PROVIDE(__start_bss_RAM2 = .) ;
    	*(.bss.$RAM2*)
    	*(.bss.$RamAHB32*)
       . = ALIGN(4) ;
       PROVIDE(__end_bss_RAM2 = .) ;
    } > RamAHB32
    /* BSS section for RamLoc128 */
    .bss_RAM3 : ALIGN(4)
    {
       PROVIDE(__start_bss_RAM3 = .) ;
    	*(.bss.$RAM3*)
    	*(.bss.$RamLoc128*)
       . = ALIGN(4) ;
       PROVIDE(__end_bss_RAM3 = .) ;
    } > RamLoc128
    /* BSS section for RamLoc72 */
    .bss_RAM4 : ALIGN(4)
    {
       PROVIDE(__start_bss_RAM4 = .) ;
    	*(.bss.$RAM4*)
    	*(.bss.$RamLoc72*)
       . = ALIGN(4) ;
       PROVIDE(__end_bss_RAM4 = .) ;
    } > RamLoc72
    /* BSS section for RamAHB_ETB16 */
    .bss_RAM5 : ALIGN(4)
    {
       PROVIDE(__start_bss_RAM5 = .) ;
    	*(.bss.$RAM5*)
    	*(.bss.$RamAHB_ETB16*)
       . = ALIGN(4) ;
       PROVIDE(__end_bss_RAM5 = .) ;
    } > RamAHB_ETB16

    /* MAIN BSS SECTION */
    .bss : ALIGN(4)
    {
        _bss = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4) ;
        _ebss = .;
        PROVIDE(end = .);
    } > RamAHB16
        
    /* NOINIT section for RamAHB32 */
    .noinit_RAM2 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM2*)
    	*(.noinit.$RamAHB32*)
       . = ALIGN(4) ;
    } > RamAHB32 
    /* NOINIT section for RamLoc128 */
    .noinit_RAM3 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM3*)
    	*(.noinit.$RamLoc128*)
       . = ALIGN(4) ;
    } > RamLoc128 
    /* NOINIT section for RamLoc72 */
    .noinit_RAM4 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM4*)
    	*(.noinit.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72 
    /* NOINIT section for RamAHB_ETB16 */
    .noinit_RAM5 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM5*)
    	*(.noinit.$RamAHB_ETB16*)
       . = ALIGN(4) ;
    } > RamAHB_ETB16 
    
    /* DEFAULT NOINIT SECTION */
    .noinit (NOLOAD): ALIGN(4)
    {
        _noinit = .;
        *(.noinit*) 
         . = ALIGN(4) ;
        _end_noinit = .;
    } > RamAHB16
    
    PROVIDE(_pvHeapStart = DEFINED(__user_heap_base) ? __user_heap_base : .);
    PROVIDE(_vStackTop = DEFINED(__user_stack_top) ? __user_stack_top : __top_RamAHB16 - 0);
}
