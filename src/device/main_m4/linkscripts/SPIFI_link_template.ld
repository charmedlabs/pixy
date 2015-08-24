INCLUDE "main_m4_SPIFI_lib.ld"

GROUP(libcr_nohost.a libcr_eabihelpers.a)

MEMORY
{
  /* Define each memory region */
  SPIflash (rx) : ORIGIN = 0x14000000, LENGTH = 0x20000 /* 128K bytes */
  RamLoc128 (rwx) : ORIGIN = 0x10000000, LENGTH = 0x20000 /* 128K bytes */
  RamAHB32 (rwx) : ORIGIN = 0x20000000, LENGTH = 0x8000 /* 32K bytes */
  RamLoc72 (rwx) : ORIGIN = 0x10080000, LENGTH = 0x12000 /* 72K bytes */
  RamAHB16 (rwx) : ORIGIN = 0x20008000, LENGTH = 0x4000 /* 16K bytes */
  RamAHB_ETB16 (rwx) : ORIGIN = 0x2000c000, LENGTH = 0x4000 /* 16K bytes */


}
  /* Define a symbol for the top of each memory region */
  __top_SPIflash = 0x14000000 + 0x20000;
  __top_RamLoc128 = 0x10000000 + 0x20000;
  __top_RamAHB32 = 0x20000000 + 0x8000;
  __top_RamLoc72 = 0x10080000 + 0x12000;
  __top_RamAHB16 = 0x20008000 + 0x4000;
  __top_RamAHB_ETB16 = 0x2000c000 + 0x4000;

ENTRY(ResetISR)

SECTIONS
{

    /* MAIN TEXT SECTION */    
    .text : ALIGN(4)
    {
        FILL(0xff)
         __rom_vectors_start__ = ABSOLUTE(.) ;
        KEEP(*(.isr_vector))
         __rom_vectors_end__ = ABSOLUTE(.) ;
        
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

/* RUN_FROM_RAM */
        LONG(LOADADDR(.text_ram));
        LONG(    ADDR(.text_ram)) ;
        LONG(  SIZEOF(.text_ram));
        LONG(__rom_vectors_start__);
        LONG(    ADDR(.text_ram)) ;
        LONG(__rom_vectors_end__ - __rom_vectors_start__) ;		        
/* END RUN_FROM_RAM */

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

 /* RUN_FROM_RAM */    
        *cr_startup_lpc43xx.o (.text* .rodata .rodata.* .constdata .constdata.*)
        *(.text.main)        
/* END RUN_FROM_RAM */ 
        
    } >SPIflash

 /* RUN_FROM_RAM */    
    .text_ram : ALIGN(4)
/* END RUN_FROM_RAM */     
    {
         __vectors_start__ = ABSOLUTE(.) ;
    	. += __rom_vectors_end__ - __rom_vectors_start__;
        *(.text*)
        *(.rodata .rodata.* .constdata .constdata.*)
        . = ALIGN(4);
        
        /* C++ constructors etc */
        . = ALIGN(4);
        KEEP(*(.init))
        
        . = ALIGN(4);
        __preinit_array_start = .;
        KEEP (*(.preinit_array))
        __preinit_array_end = .;
        
        . = ALIGN(4);
        __init_array_start = .;
        KEEP (*(SORT(.init_array.*)))
        KEEP (*(.init_array))
        __init_array_end = .;
        
        KEEP(*(.fini));
        
        . = ALIGN(4);
        KEEP (*crtbegin.o(.ctors))
        KEEP (*(EXCLUDE_FILE (*crtend.o) .ctors))
        KEEP (*(SORT(.ctors.*)))
        KEEP (*crtend.o(.ctors))
        
        . = ALIGN(4);
        KEEP (*crtbegin.o(.dtors))
        KEEP (*(EXCLUDE_FILE (*crtend.o) .dtors))
        KEEP (*(SORT(.dtors.*)))
        KEEP (*crtend.o(.dtors))
        . = ALIGN(4);
        /* End C++ */
/* RUN_FROM_RAM */          
    } > RamLoc128 AT>SPIflash
/* END RUN_FROM_RAM */

    /*
     * for exception handling/unwind - some Newlib functions (in common
     * with C++ and STDC++) use this. 
     */
    .ARM.extab : ALIGN(4)
    {
    	*(.ARM.extab* .gnu.linkonce.armextab.*)
/* RUN_FROM_RAM */          
    } > RamLoc128 AT>SPIflash
/* END RUN_FROM_RAM */

    __exidx_start = .;
    
    .ARM.exidx : ALIGN(4)
    {
    	*(.ARM.exidx* .gnu.linkonce.armexidx.*)
/* RUN_FROM_RAM */          
    } > RamLoc128 AT>SPIflash
/* END RUN_FROM_RAM */

    __exidx_end = .;
    
    _etext = .;
        
    
    /* DATA section for RamAHB32 */
    .data_RAM2 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM2)
       *(.ramfunc.$RamAHB32)
    	*(.data.$RAM2*)
    	*(.data.$RamAHB32*)
       . = ALIGN(4) ;
    } > RamAHB32 AT>SPIflash
    
    /* DATA section for RamLoc72 */
    .data_RAM3 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM3)
       *(.ramfunc.$RamLoc72)
    	*(.data.$RAM3*)
    	*(.data.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72 AT>SPIflash
    
    /* DATA section for RamAHB16 */
    .data_RAM4 : ALIGN(4)
    {
       FILL(0xff)
       __core_m0app_START__ = .; /* start of slave image */
         KEEP(*(.core_m0app*))
       __core_m0app_END__ = .; /* end of slave image */
       ASSERT(!(__core_m0app_START__ == __core_m0app_END__), "No slave code for _core_m0app");
       ASSERT( (ABSOLUTE(__core_m0app_START__) == __vectors_start___core_m0app), "M0APP execute address differs from address provided in source image");
       *(.ramfunc.$RAM4)
       *(.ramfunc.$RamAHB16)
    	*(.data.$RAM4*)
    	*(.data.$RamAHB16*)
       . = ALIGN(4) ;
    } > RamAHB16 AT>SPIflash
    
    /* DATA section for RamAHB_ETB16 */
    .data_RAM5 : ALIGN(4)
    {
       FILL(0xff)
       *(.ramfunc.$RAM5)
       *(.ramfunc.$RamAHB_ETB16)
    	*(.data.$RAM5*)
    	*(.data.$RamAHB_ETB16*)
       . = ALIGN(4) ;
    } > RamAHB_ETB16 AT>SPIflash
    
    /* MAIN DATA SECTION */
    

    .uninit_RESERVED : ALIGN(4)
    {
        KEEP(*(.bss.$RESERVED*))
        . = ALIGN(4) ;
        _end_uninit_RESERVED = .;
    } > RamLoc128

	
	/* Main DATA section (RamLoc128) */
	.data : ALIGN(4)
	{
	   FILL(0xff)
	   _data = . ;
	   *(vtable)
	   *(.ramfunc*)
	   *(.data*)
	   . = ALIGN(4) ;
	   _edata = . ;
	} > RamLoc128 AT>SPIflash

    /* BSS section for RamAHB32 */
    .bss_RAM2 : ALIGN(4)
    {
    	*(.bss.$RAM2*)
    	*(.bss.$RamAHB32*)
       . = ALIGN(4) ;
    } > RamAHB32
    /* BSS section for RamLoc72 */
    .bss_RAM3 : ALIGN(4)
    {
    	*(.bss.$RAM3*)
    	*(.bss.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72
    /* BSS section for RamAHB16 */
    .bss_RAM4 : ALIGN(4)
    {
    	*(.bss.$RAM4*)
    	*(.bss.$RamAHB16*)
       . = ALIGN(4) ;
    } > RamAHB16
    /* BSS section for RamAHB_ETB16 */
    .bss_RAM5 : ALIGN(4)
    {
    	*(.bss.$RAM5*)
    	*(.bss.$RamAHB_ETB16*)
       . = ALIGN(4) ;
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
    } > RamLoc128
        
    /* NOINIT section for RamAHB32 */
    .noinit_RAM2 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM2*)
    	*(.noinit.$RamAHB32*)
       . = ALIGN(4) ;
    } > RamAHB32 
    /* NOINIT section for RamLoc72 */
    .noinit_RAM3 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM3*)
    	*(.noinit.$RamLoc72*)
       . = ALIGN(4) ;
    } > RamLoc72 
    /* NOINIT section for RamAHB16 */
    .noinit_RAM4 (NOLOAD) : ALIGN(4)
    {
    	*(.noinit.$RAM4*)
    	*(.noinit.$RamAHB16*)
       . = ALIGN(4) ;
    } > RamAHB16 
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
    } > RamLoc128
    
PROVIDE(_pvHeapStart = ADDR(.data_RAM2));
PROVIDE(_vStackTop = __top_RamAHB32);
PROVIDE(_pvHeapLimit = _vStackTop - 0x600);   /* stack is 0x600 bytes or 1.5k  */
}
