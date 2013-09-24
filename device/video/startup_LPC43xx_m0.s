;/***********************************************************************
; * $Id: startup_LPC18xx.s 6473 2011-02-16 17:40:54Z nxp27266 $
; *
; * Project: LPC18xx CMSIS Package
; *
; * Description: Cortex-M3 Core Device Startup File for the NXP LPC18xx 
; *              Device Series.
; *
; * Copyright(C) 2011, NXP Semiconductor
; * All rights reserved.
; *
; ***********************************************************************
; * Software that is described herein is for illustrative purposes only
; * which provides customers with programming information regarding the
; * products. This software is supplied "AS IS" without any warranties.
; * NXP Semiconductors assumes no responsibility or liability for the
; * use of the software, conveys no license or title under any patent,
; * copyright, or mask work right to the product. NXP Semiconductors
; * reserves the right to make changes in the software without
; * notification. NXP Semiconductors also make no representation or
; * warranty that such application will be suitable for the specified
; * use without further testing or modification.
; **********************************************************************/

; <h> Stack Configuration
;   <o> Stack Size (in Bytes) <0x0-0xFFFFFFFF:8>
; </h>

Stack_Size      EQU     0x00000400

                AREA    STACK, NOINIT, READWRITE, ALIGN=3
Stack_Mem       SPACE   Stack_Size
__initial_sp

; <h> Heap Configuration
;   <o>  Heap Size (in Bytes) <0x0-0xFFFFFFFF:8>
; </h>

Heap_Size       EQU     0x00000400

                AREA    HEAP, NOINIT, READWRITE, ALIGN=3
__heap_base
Heap_Mem        SPACE   Heap_Size
__heap_limit

                PRESERVE8
                THUMB

; Vector Table Mapped to Address 0 at Reset

                AREA    RESET, DATA, READONLY
                EXPORT  __Vectors

Sign_Value		EQU		0x5A5A5A5A

__Vectors       DCD     __initial_sp              	; 0 Top of Stack
                DCD     M0_Reset_Handler           	; 1 Reset Handler
                DCD     M0_NMI_Handler  			; 2 NMI Handler
                DCD     M0_HardFault_Handler       	; 3 Hard Fault Handler
                DCD     0							; 4 Reserved
                DCD     0							; 5 Reserved
                DCD     0							; 6 Reserved
                DCD     0							; 7 Reserved
                DCD     0                         	; 8 Reserved
                DCD     0                         	; 9 Reserved
                DCD     0                         	; 10 Reserved
                DCD     M0_SVC_Handler          	; 11 SVCall Handler
                DCD     M0_DebugMon_Handler    		; 12 Debug Monitor Handler
                DCD     0                         	; 13 Reserved
                DCD     M0_PendSV_Handler         	; 14 PendSV Handler
                DCD     M0_SysTick_Handler        	; 15 SysTick Handler

                ; External Interrupts				
				DCD		M0_RTC_IRQHandler	 		; 16 RTC
				DCD		M0_M4CORE_IRQHandler		; 17 M4 Core
				DCD		M0_DMA_IRQHandler			; 18 General Purpose DMA
				DCD		0							; 19 Reserved
				DCD		0							; 20 Reserved
				DCD		M0_ETH_IRQHandler			; 21 Ethernet
				DCD		M0_SDIO_IRQHandler			; 22 SD/MMC
				DCD		M0_LCD_IRQHandler			; 23 LCD
				DCD		M0_USB0_IRQHandler			; 24 USB0
				DCD		M0_USB1_IRQHandler			; 25 USB1
				DCD		M0_SCT_IRQHandler			; 26 State Configurable Timer
				DCD		M0_RIT_OR_WWDT_IRQHandler   ; 27 Repetitive Interrupt Timer
				DCD		M0_TIMER0_IRQHandler		; 28 Timer0
				DCD		M0_GINT1_IRQHandler			; 29 GINT1
				DCD		M0_TIMER3_IRQHandler		; 30 Timer3
				DCD		0							; 31 Reserved
				DCD		0							; 32 Reserved
				DCD		M0_MCPWM_IRQHandler			; 33 Motor Control PWM
				DCD		M0_ADC0_IRQHandler			; 34 ADC0
				DCD		M0_I2C0_OR_I2C1_IRQHandler	; 35 I2C0 or I2C1
				DCD		M0_SGPIO_IRQHandler			; 36 Serial GPIO
				DCD		M0_SPI_OR_DAC_IRQHandler	; 37 SPI or DAC
				DCD		M0_ADC1_IRQHandler			; 38 ADC1
				DCD		M0_SSP0_OR_SSP1_IRQHandler	; 39 SSP0 or SSP1
				DCD		M0_EVENTROUTER_IRQHandler	; 40 Event Router
				DCD		M0_USART0_IRQHandler		; 41 USART0
				DCD		M0_USART2_OR_C_CAN1_IRQHandler ; 42 USART2 or C CAN1
				DCD		M0_USART3_IRQHandler		; 43 USART3
				DCD		M0_I2S0_OR_I2S1_OR_QEI_IRQHandler ; 44 I2S0 or I2S1 or QEI
				DCD		M0_C_CAN0_IRQHandler		; 45 C CAN0

;                IF      :LNOT::DEF:NO_CRP
;                AREA    |.ARM.__at_0x02FC|, CODE, READONLY
;CRP_Key         DCD     0xFFFFFFFF
;                ENDIF

                AREA    |.text|, CODE, READONLY

; Reset Handler

M0_Reset_Handler   PROC
                EXPORT  M0_Reset_Handler             [WEAK]
                IMPORT  __main
                LDR     R0, =__main
                BX      R0
                ENDP

; Dummy Exception Handlers (infinite loops which can be modified)                

M0_NMI_Handler     PROC
                EXPORT  M0_NMI_Handler               [WEAK]
                B       .
                ENDP
M0_HardFault_Handler\
                PROC
                EXPORT  M0_HardFault_Handler         [WEAK]
                B       .
                ENDP
M0_SVC_Handler     PROC
                EXPORT  M0_SVC_Handler               [WEAK]
                B       .
                ENDP
M0_DebugMon_Handler\
                PROC
                EXPORT  M0_DebugMon_Handler          [WEAK]
                B       .
                ENDP
M0_PendSV_Handler  PROC
                EXPORT  M0_PendSV_Handler            [WEAK]
                B       .
                ENDP
M0_SysTick_Handler PROC
                EXPORT  M0_SysTick_Handler           [WEAK]
                B       .
                ENDP

M0_Default_Handler PROC

				EXPORT M0_RTC_IRQHandler			[WEAK]
				EXPORT M0_M4CORE_IRQHandler			[WEAK]
				EXPORT M0_DMA_IRQHandler			[WEAK]
				EXPORT M0_ETH_IRQHandler			[WEAK]
				EXPORT M0_SDIO_IRQHandler			[WEAK]
				EXPORT M0_LCD_IRQHandler			[WEAK]
				EXPORT M0_USB0_IRQHandler			[WEAK]
				EXPORT M0_USB1_IRQHandler			[WEAK]
				EXPORT M0_SCT_IRQHandler			[WEAK]
				EXPORT M0_RIT_OR_WWDT_IRQHandler   	[WEAK]
				EXPORT M0_TIMER0_IRQHandler			[WEAK]
				EXPORT M0_GINT1_IRQHandler			[WEAK]
				EXPORT M0_TIMER3_IRQHandler			[WEAK]
				EXPORT M0_MCPWM_IRQHandler			[WEAK]
				EXPORT M0_ADC0_IRQHandler			[WEAK]
				EXPORT M0_I2C0_OR_I2C1_IRQHandler	[WEAK]
				EXPORT M0_SGPIO_IRQHandler			[WEAK]
				EXPORT M0_SPI_OR_DAC_IRQHandler		[WEAK]
				EXPORT M0_ADC1_IRQHandler			[WEAK]
				EXPORT M0_SSP0_OR_SSP1_IRQHandler	[WEAK]
				EXPORT M0_EVENTROUTER_IRQHandler	[WEAK]
				EXPORT M0_USART0_IRQHandler			[WEAK]
				EXPORT M0_USART2_OR_C_CAN1_IRQHandler [WEAK]
				EXPORT M0_USART3_IRQHandler			[WEAK]
				EXPORT M0_I2S0_OR_I2S1_OR_QEI_IRQHandler [WEAK]
				EXPORT M0_C_CAN0_IRQHandler			[WEAK]


M0_RTC_IRQHandler			
M0_M4CORE_IRQHandler			
M0_DMA_IRQHandler			
M0_ETH_IRQHandler			
M0_SDIO_IRQHandler			
M0_LCD_IRQHandler			
M0_USB0_IRQHandler			
M0_USB1_IRQHandler			
M0_SCT_IRQHandler			
M0_RIT_OR_WWDT_IRQHandler   	
M0_TIMER0_IRQHandler			
M0_GINT1_IRQHandler			
M0_TIMER3_IRQHandler			
M0_MCPWM_IRQHandler			
M0_ADC0_IRQHandler			
M0_I2C0_OR_I2C1_IRQHandler	
M0_SGPIO_IRQHandler			
M0_SPI_OR_DAC_IRQHandler		
M0_ADC1_IRQHandler			
M0_SSP0_OR_SSP1_IRQHandler	
M0_EVENTROUTER_IRQHandler	
M0_USART0_IRQHandler			
M0_USART2_OR_C_CAN1_IRQHandler 
M0_USART3_IRQHandler			
M0_I2S0_OR_I2S1_OR_QEI_IRQHandler 
M0_C_CAN0_IRQHandler			

                B       .

                ENDP

                ALIGN

; User Initial Stack & Heap

                IF      :DEF:__MICROLIB
                
                EXPORT  __initial_sp
                EXPORT  __heap_base
                EXPORT  __heap_limit
                
                ELSE
                
                IMPORT  __use_two_region_memory
                EXPORT  __user_initial_stackheap
__user_initial_stackheap

                LDR     R0, =  Heap_Mem
                LDR     R1, =(Stack_Mem + Stack_Size)
                LDR     R2, = (Heap_Mem +  Heap_Size)
                LDR     R3, = Stack_Mem
                BX      LR

                ALIGN

                ENDIF

                AREA    |.text|,CODE, READONLY
getPC   		PROC
				EXPORT  getPC

				MOV     R0,LR
				BX		LR

				ENDP

                END
