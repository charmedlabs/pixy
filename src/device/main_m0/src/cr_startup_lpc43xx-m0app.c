//*****************************************************************************
// LPC43xx (Cortex M0 APP) Startup code for use with LPCXpresso IDE
//
// Version : 140113
//*****************************************************************************
//
// Copyright(C) NXP Semiconductors, 2013-2014
// All rights reserved.
//
// Software that is described herein is for illustrative purposes only
// which provides customers with programming information regarding the
// LPC products.  This software is supplied "AS IS" without any warranties of
// any kind, and NXP Semiconductors and its licensor disclaim any and
// all warranties, express or implied, including all implied warranties of
// merchantability, fitness for a particular purpose and non-infringement of
// intellectual property rights.  NXP Semiconductors assumes no responsibility
// or liability for the use of the software, conveys no license or rights under any
// patent, copyright, mask work right, or any other intellectual property rights in
// or to any products. NXP Semiconductors reserves the right to make changes
// in the software without notification. NXP Semiconductors also makes no
// representation or warranty that such application will be suitable for the
// specified use without further testing or modification.
//
// Permission to use, copy, modify, and distribute this software and its
// documentation is hereby granted, under NXP Semiconductors' and its
// licensor's relevant copyrights in the software, without fee, provided that it
// is used in conjunction with NXP Semiconductors microcontrollers.  This
// copyright, permission, and disclaimer notice must appear in all copies of
// this code.
//*****************************************************************************

#include "debug_frmwrk.h"

#if defined (__cplusplus)
#ifdef __REDLIB__
#error Redlib does not support C++
#else
//*****************************************************************************
//
// The entry point for the C++ library startup
//
//*****************************************************************************
extern "C" {
  extern void __libc_init_array(void);
}
#endif
#endif

#define WEAK __attribute__ ((weak))
#define ALIAS(f) __attribute__ ((weak, alias (#f)))

//*****************************************************************************
#if defined (__cplusplus)
extern "C" {
#endif

//*****************************************************************************
#if defined (__USE_CMSIS) || defined (__USE_LPCOPEN)
// Declaration of external SystemInit function
extern void SystemInit(void);
#endif
//*****************************************************************************
//
// Forward declaration of the default handlers. These are aliased.
// When the application defines a handler (with the same name), this will 
// automatically take precedence over these weak definitions
//
//*****************************************************************************
     void ResetISR(void);
#if defined (__USE_LPCOPEN)
WEAK void NMI_Handler(void);
WEAK void HardFault_Handler(void);
WEAK void SVC_Handler(void);
WEAK void PendSV_Handler(void);
WEAK void SysTick_Handler(void);
WEAK void IntDefaultHandler(void);
#else
WEAK void M0_NMI_Handler(void);
WEAK void M0_HardFault_Handler (void);
WEAK void M0_SVC_Handler(void);
WEAK void M0_PendSV_Handler(void);
WEAK void M0_SysTick_Handler(void);
WEAK void M0_IntDefaultHandler(void);
#endif

//*****************************************************************************
//
// Forward declaration of the specific IRQ handlers. These are aliased
// to the IntDefaultHandler, which is a 'forever' loop. When the application
// defines a handler (with the same name), this will automatically take 
// precedence over these weak definitions
//
//*****************************************************************************
#if defined (__USE_LPCOPEN)
void RTC_IRQHandler(void) ALIAS(IntDefaultHandler);
void MX_CORE_IRQHandler(void) ALIAS(IntDefaultHandler);
void DMA_IRQHandler(void) ALIAS(IntDefaultHandler);
void FLASHEEPROM_IRQHandler(void) ALIAS(IntDefaultHandler);
void ETH_IRQHandler(void) ALIAS(IntDefaultHandler);
void SDIO_IRQHandler(void) ALIAS(IntDefaultHandler);
void LCD_IRQHandler(void) ALIAS(IntDefaultHandler);
void USB0_IRQHandler(void) ALIAS(IntDefaultHandler);
void USB1_IRQHandler(void) ALIAS(IntDefaultHandler);
void SCT_IRQHandler(void) ALIAS(IntDefaultHandler);
void RIT_IRQHandler(void) ALIAS(IntDefaultHandler);
void TIMER0_IRQHandler(void) ALIAS(IntDefaultHandler);
void GINT1_IRQHandler(void) ALIAS(IntDefaultHandler);
void GPIO4_IRQHandler(void) ALIAS(IntDefaultHandler);
void TIMER3_IRQHandler(void) ALIAS(IntDefaultHandler);
void MCPWM_IRQHandler(void) ALIAS(IntDefaultHandler);
void ADC0_IRQHandler(void) ALIAS(IntDefaultHandler);
void I2C0_IRQHandler(void) ALIAS(IntDefaultHandler);
void SGPIO_IRQHandler(void) ALIAS(IntDefaultHandler);
void SPI_IRQHandler (void) ALIAS(IntDefaultHandler);
void ADC1_IRQHandler(void) ALIAS(IntDefaultHandler);
void SSP0_IRQHandler(void) ALIAS(IntDefaultHandler);
void EVRT_IRQHandler(void) ALIAS(IntDefaultHandler);
void UART0_IRQHandler(void) ALIAS(IntDefaultHandler);
void UART1_IRQHandler(void) ALIAS(IntDefaultHandler);
void UART2_IRQHandler(void) ALIAS(IntDefaultHandler);
void UART3_IRQHandler(void) ALIAS(IntDefaultHandler);
void I2S0_IRQHandler(void) ALIAS(IntDefaultHandler);
void CAN0_IRQHandler(void) ALIAS(IntDefaultHandler);
void SPIFI_ADCHS_IRQHandler(void) ALIAS(IntDefaultHandler);
void M0SUB_IRQHandler(void) ALIAS(IntDefaultHandler);
#else
void M0_RTC_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_M4CORE_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_DMA_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_ETH_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SDIO_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_LCD_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_USB0_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_USB1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SCT_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_RIT_OR_WWDT_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_TIMER0_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_GINT1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_TIMER3_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_MCPWM_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_ADC0_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_I2C0_OR_I2C1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SGPIO_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SPI_OR_DAC_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_ADC1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SSP0_OR_SSP1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_EVENTROUTER_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_USART0_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_USART2_OR_C_CAN1_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_USART3_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_I2S0_OR_I2S1_OR_QEI_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_C_CAN0_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_SPIFI_OR_VADC_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
void M0_M0SUB_IRQHandler(void) ALIAS(M0_IntDefaultHandler);
#endif
//*****************************************************************************
//
// The entry point for the application.
// __main() is the entry point for Redlib based applications
// main() is the entry point for Newlib based applications
//
//*****************************************************************************
#if defined (__REDLIB__)
extern void __main(void);
#endif
extern int main(void);
//*****************************************************************************
//
// External declaration for the pointer to the stack top from the Linker Script
//
//*****************************************************************************
extern void _vStackTop(void);

//*****************************************************************************
#if defined (__cplusplus)
} // extern "C"
#endif
//*****************************************************************************
//
// The vector table.
// This relies on the linker script to place at correct location in memory.
//
//*****************************************************************************
extern void (* const g_pfnVectors[])(void);
__attribute__ ((section(".isr_vector")))
void (* const g_pfnVectors[])(void) = {
#if defined (__USE_LPCOPEN)
    // Core Level - CM0
    &_vStackTop,                        // The initial stack pointer
    ResetISR,                           // 1 The reset handler
    NMI_Handler,                    // The NMI handler
    HardFault_Handler,              // The hard fault handler
    0,                                  // 4 Reserved
    0,                                  // 5 Reserved
    0,                                  // 6 Reserved
    0,                                  // 7 Reserved
    0,                                  // 8 Reserved
    0,                                  // 9 Reserved
    0,                                  // 10 Reserved
    SVC_Handler,                    // SVCall handler
    0,                              // Reserved
    0,                              // Reserved
    PendSV_Handler,                 // The PendSV handler
    SysTick_Handler,                // The SysTick handler

    // Chip Level - 43xx M0 core
    RTC_IRQHandler,                 // 16 RTC
    MX_CORE_IRQHandler,             // 17 CortexM4/M0 (LPC43XX ONLY)
    DMA_IRQHandler,                 // 18 General Purpose DMA
    0,                              // 19 Reserved
    FLASHEEPROM_IRQHandler,         // 20 ORed flash Bank A, flash Bank B, EEPROM interrupts
    ETH_IRQHandler,                 // 21 Ethernet
    SDIO_IRQHandler,                // 22 SD/MMC
    LCD_IRQHandler,                 // 23 LCD
    USB0_IRQHandler,                // 24 USB0
    USB1_IRQHandler,                // 25 USB1
    SCT_IRQHandler,                 // 26 State Configurable Timer
    RIT_IRQHandler,                 // 27 ORed Repetitive Interrupt Timer, WWDT
    TIMER0_IRQHandler,              // 28 Timer0
    GINT1_IRQHandler,               // 29 GINT1
    GPIO4_IRQHandler,               // 30 GPIO4
    TIMER3_IRQHandler,              // 31 Timer 3
    MCPWM_IRQHandler,               // 32 Motor Control PWM
    ADC0_IRQHandler,                // 33 A/D Converter 0
    I2C0_IRQHandler,                // 34 ORed I2C0, I2C1
    SGPIO_IRQHandler,               // 35 SGPIO (LPC43XX ONLY)
    SPI_IRQHandler,                 // 36 ORed SPI, DAC (LPC43XX ONLY)
    ADC1_IRQHandler,                // 37 A/D Converter 1
    SSP0_IRQHandler,                // 38 ORed SSP0, SSP1
    EVRT_IRQHandler,                // 39 Event Router
    UART0_IRQHandler,               // 40 UART0
    UART1_IRQHandler,               // 41 UART1
    UART2_IRQHandler,               // 42 UART2
    UART3_IRQHandler,               // 43 USRT3
    I2S0_IRQHandler,                // 44 ORed I2S0, I2S1
    CAN0_IRQHandler,                // 45 C_CAN0
    SPIFI_ADCHS_IRQHandler,         // 46 SPIFI OR ADCHS interrupt
    M0SUB_IRQHandler,               // 47 M0SUB core
};
#else
    // Core Level - CM0
    &_vStackTop,                        // The initial stack pointer
    ResetISR,                           // 1 The reset handler
    M0_NMI_Handler,                     // 2 The NMI handler
    M0_HardFault_Handler,               // 3 The hard fault handler
    0,                                  // 4 Reserved
    0,                                  // 5 Reserved
    0,                                  // 6 Reserved
    0,                                  // 7 Reserved
    0,                                  // 8 Reserved
    0,                                  // 9 Reserved
    0,                                  // 10 Reserved
    M0_SVC_Handler,                     // 11 SVCall handler
    0,                                  // 12 Reserved
    0,                                  // 13 Reserved
    M0_PendSV_Handler,                  // 14 The PendSV handler
    M0_SysTick_Handler,                 // 15 The SysTick handler

    // Chip Level - LPC43 (CM0 APP)
        M0_RTC_IRQHandler,              // 16 RTC
    M0_M4CORE_IRQHandler,               // 17 Interrupt from M4 Core
    M0_DMA_IRQHandler,                  // 18 General Purpose DMA
    0,                                  // 19 Reserved
    0,                                  // 20 Reserved
    M0_ETH_IRQHandler,                  // 21 Ethernet
    M0_SDIO_IRQHandler,                 // 22 SD/MMC
    M0_LCD_IRQHandler,                  // 23 LCD
    M0_USB0_IRQHandler,                 // 24 USB0
    M0_USB1_IRQHandler,                 // 25 USB1
    M0_SCT_IRQHandler ,                 // 26 State Configurable Timer
    M0_RIT_OR_WWDT_IRQHandler,          // 27 Repetitive Interrupt Timer
    M0_TIMER0_IRQHandler,               // 28 Timer0
    M0_GINT1_IRQHandler,                // 29 GINT1
    M0_TIMER3_IRQHandler,               // 30 Timer3
    0,                                  // 31 Reserved
    0 ,                                 // 32 Reserved
    M0_MCPWM_IRQHandler,                // 33 Motor Control PWM
    M0_ADC0_IRQHandler,                 // 34 ADC0
    M0_I2C0_OR_I2C1_IRQHandler,         // 35 I2C0 or I2C1
    M0_SGPIO_IRQHandler,                // 36 Serial GPIO
    M0_SPI_OR_DAC_IRQHandler,           // 37 SPI or DAC
    M0_ADC1_IRQHandler,                 // 38 ADC1
    M0_SSP0_OR_SSP1_IRQHandler,         // 39 SSP0 or SSP1
    M0_EVENTROUTER_IRQHandler,          // 40 Event Router
    M0_USART0_IRQHandler,               // 41 USART0
    M0_USART2_OR_C_CAN1_IRQHandler,     // 42 USART2 or C CAN1
    M0_USART3_IRQHandler,               // 43 USART3
    M0_I2S0_OR_I2S1_OR_QEI_IRQHandler,  // 44 I2S0 or I2S1 or QEI
    M0_C_CAN0_IRQHandler,               // 45 C CAN0
    M0_SPIFI_OR_VADC_IRQHandler,        // 46
    M0_M0SUB_IRQHandler,                // 47 Interrupt from M0SUB
  };
#endif
//*****************************************************************************
// Functions to carry out the initialization of RW and BSS data sections. These
// are written as separate functions rather than being inlined within the
// ResetISR() function in order to cope with MCUs with multiple banks of
// memory.
//*****************************************************************************
__attribute__ ((section(".after_vectors")))
void data_init(unsigned int romstart, unsigned int start, unsigned int len) {
  unsigned int *pulDest = (unsigned int*) start;
  unsigned int *pulSrc = (unsigned int*) romstart;
  unsigned int loop;
  for (loop = 0; loop < len; loop = loop + 4)
    *pulDest++ = *pulSrc++;
}

__attribute__ ((section(".after_vectors")))
void bss_init(unsigned int start, unsigned int len) {
  unsigned int *pulDest = (unsigned int*) start;
  unsigned int loop;
  for (loop = 0; loop < len; loop = loop + 4)
    *pulDest++ = 0;
}

//*****************************************************************************
// The following symbols are constructs generated by the linker, indicating
// the location of various points in the "Global Section Table". This table is
// created by the linker via the Code Red managed linker script mechanism. It
// contains the load address, execution address and length of each RW data
// section and the execution and length of each BSS (zero initialized) section.
//*****************************************************************************
extern unsigned int __data_section_table;
extern unsigned int __data_section_table_end;
extern unsigned int __bss_section_table;
extern unsigned int __bss_section_table_end;

//*****************************************************************************
// Reset entry point for your code.
// Sets up a simple runtime environment and initializes the C/C++
// library.
//
//*****************************************************************************
void
ResetISR(void) {

  // ******************************
  // Modify CREG->M0APPMAP so that M0 looks in correct place
  // for its vector table when an exception is triggered.
  // Note that we do not use the CMSIS register access mechanism,
  // as there is no guarantee that the project has been configured
  // to use CMSIS.
  unsigned int *pCREG_M0APPMAP = (unsigned int *) 0x40043404;
  // CMSIS : CREG->M0APPMAP = <address of vector table>
  *pCREG_M0APPMAP = (unsigned int)g_pfnVectors;

    //
    // Copy the data sections from flash to SRAM.
    //
  unsigned int LoadAddr, ExeAddr, SectionLen;
  unsigned int *SectionTableAddr;

  // Load base address of Global Section Table
  SectionTableAddr = &__data_section_table;

    // Copy the data sections from flash to SRAM.
  while (SectionTableAddr < &__data_section_table_end) {
    LoadAddr = *SectionTableAddr++;
    ExeAddr = *SectionTableAddr++;
    SectionLen = *SectionTableAddr++;
    data_init(LoadAddr, ExeAddr, SectionLen);
  }
  // At this point, SectionTableAddr = &__bss_section_table;
  // Zero fill the bss segment
  while (SectionTableAddr < &__bss_section_table_end) {
    ExeAddr = *SectionTableAddr++;
    SectionLen = *SectionTableAddr++;
    bss_init(ExeAddr, SectionLen);
  }

// **********************************************************
// No need to call SystemInit() here, as master CM4 cpu will
// have done the main system set up before enabling CM0.
// **********************************************************

#if defined (__cplusplus)
  //
  // Call C++ library initialisation
  //
  __libc_init_array();
#endif

#if defined (__REDLIB__)
  // Call the Redlib library, which in turn calls main()
  __main() ;
#else
  main();
#endif

  //
  // main() shouldn't return, but if it does, we'll just enter an infinite loop 
  //
  while (1) {
    ;
  }
}

//*****************************************************************************
// Default exception handlers. Override the ones here by defining your own
// handler routines in your application code.
//*****************************************************************************
__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void NMI_Handler(void)
#else
void M0_NMI_Handler(void)
#endif
{   while(1) { }
}

__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void HardFault_Handler(void)
#else
void M0_HardFault_Handler(void)
#endif
{
	_DBG("HardFault\n");
	while(1) { }
}

__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void SVC_Handler(void)
#else
void M0_SVC_Handler(void)
#endif
{   while(1) { }
}

__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void PendSV_Handler(void)
#else
void M0_PendSV_Handler(void)
#endif
{   while(1) { }
}

__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void SysTick_Handler(void)
#else
void M0_SysTick_Handler(void)
#endif
{   while(1) { }
}

//*****************************************************************************
//
// Processor ends up here if an unexpected interrupt occurs or a specific
// handler is not present in the application code.
//
//*****************************************************************************
__attribute__ ((section(".after_vectors")))
#if defined (__USE_LPCOPEN)
void IntDefaultHandler(void)
#else
void M0_IntDefaultHandler(void)
#endif
{   while(1) { }
}

