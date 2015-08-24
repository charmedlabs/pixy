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



#include <stdint.h>
#include "lpc_types.h"
#include "platform_config.h"
#include "ipc_mbx.h"

/* definitions */
/* perform a callback when mailbox flag gets signaled - within IRQ - */
static void _mbxProcess(mbxId_t mbxNum);

void dummyCallback(msg_t message, msgId_t idNum, mbxParam_t parameter);


#ifdef IPC_MASTER

	#define IRQ_LOCK_KEY 
		
	#define _lockInts()		__set_BASEPRI((MASTER_MAILBOX_PRIORITY << (8 - __NVIC_PRIO_BITS) & 0xFF ))
	#define _unlockInts()	__set_BASEPRI(0)

	volatile mbxFlag_t mbxFlags[NUM_MASTER_MBX];

#endif	

#ifdef IPC_SLAVE

//	/* do not remove this, it is only to make the code compile on slave side */
//	const unsigned char LR0[1];
#ifdef KEIL
	#define _lockInts() 	do {\
	wasMasked = __disable_irq();\
	} while(0)
#else
	#define _lockInts() 	do {\
	__disable_irq();\
	} while(0)
#endif
	#define _unlockInts() do {\
	if(!wasMasked) 	__enable_irq();\
	} while(0)

	#define IRQ_LOCK_KEY	int	wasMasked;

	volatile mbxFlag_t mbxFlags[NUM_SLAVE_MBX];
					 
#endif	


Mbx* mbxLocalTablePtr;
Mbx* mbxRemoteTablePtr;

static void _plugCallbacks(CbackItem callBackTable[], uint32_t numMbx);

/* functions implementation */

/* dummy callback function */
void IPC_dummyCallback(msg_t message, msgId_t idNum, mbxParam_t parameter) {return;}

/************************************************************************
 * LOCAL MAILBOX FUNCTIONS
 ***********************************************************************/
void IPC_resetMbxFlag(mbxId_t mbxNum) {

		// on cortex M4/M3 use primask
		// on cortex M0 disable interrupts globally
		IRQ_LOCK_KEY

		_lockInts();
		mbxFlags[mbxNum] = NO_MSG;
		_unlockInts();
}

/* execute a local mailbox callback */
static void _mbxProcess(mbxId_t mbxNum)
{															 
	Mbx* lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;

	// execute the function callback
	(*(lPtr->mbxAction))(lPtr->mbxHeader.msg, lPtr->mbxHeader.msgId, lPtr->mbxParam);
}

	
/* change the local mailbox status */
void _setMbxStatus(mbxId_t mbxNum, mbxStat_t status)
{
	// if cortex M4/M3 use primask, else disable interrupts globally
	IRQ_LOCK_KEY

	Mbx* lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;

	_lockInts();

	lPtr->mbxStatus = status;
	if((status == READY) || (status == ERROR_OCCURRED)) mbxFlags[mbxNum] = NO_MSG;

	__DSB(); /* sync the data */

	_unlockInts();
}
	
/* return the local mailbox status */
mbxStat_t IPC_queryLocalMbx(mbxId_t mbxNum) 	{		

	Mbx* lPtr;
		
	lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;
	return (lPtr->mbxStatus);
}

/* return the message type */
msg_t IPC_getMsgType(mbxId_t mbxNum)	{
		
	Mbx* lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;
	return (lPtr->mbxHeader.msg);
}

/* return the message id */
msgId_t IPC_getMsgId(mbxId_t mbxNum) {
		
	Mbx* lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;
	return (lPtr->mbxHeader.msgId);
}

/* return the parameter */
mbxParam_t IPC_getMbxParameter(mbxId_t mbxNum) {
	
	Mbx* lPtr = (Mbx*) (mbxLocalTablePtr);
	lPtr += mbxNum;
	return (lPtr->mbxParam);
}


/************************************************************************
 * REMOTE MAILBOX FUNCTIONS
 ***********************************************************************/

/* return the remote mailbox status */
mbxStat_t IPC_queryRemoteMbx(mbxId_t mbxNum) {

	Mbx* rPtr = (Mbx*)(mbxRemoteTablePtr);	
	rPtr += mbxNum;
	return (rPtr->mbxStatus);
}
	
void IPC_sendMsg(mbxId_t mbxNum, msg_t msg, msgId_t msgNum, mbxParam_t param) {

	Mbx* rPtr = (Mbx*)(mbxRemoteTablePtr);	
	rPtr += mbxNum;
		
	// prepare the information
	rPtr->mbxHeader.msg = msg;
	rPtr->mbxHeader.msgId = msgNum;	
	rPtr->mbxParam = param;
	rPtr->mbxStatus = PROCESS;

	// make sure all data transactions complete before next instruction is executed
	__DSB();  	
							
	// now trigger the remote processor
#ifdef KEIL
	__sev();
#else
	__SEV();
#endif
}


/************************************************************************
 * FRAMEWORK INITIALIZATION FUNCTIONS
 ***********************************************************************/
/* initialize the slave MBX ipc framework */
void IPC_initSlaveMbx(CbackItem cbackTable[], Mbx* masterMbxPtr, Mbx* slaveMbxPtr)
{	
	mbxId_t i;			
	Mbx* lPtr;

	// initialize the pointers
	mbxLocalTablePtr = slaveMbxPtr;
	mbxRemoteTablePtr = masterMbxPtr;

	// clear the mailbox and the flags
	for(i=(mbxId_t)0, lPtr = mbxLocalTablePtr; i<NUM_SLAVE_MBX; i++,lPtr++) {
	
		lPtr->mbxStatus = READY;
		lPtr->mbxHeader.msg = MBX_MSG_DEFAULT;
		lPtr->mbxHeader.msgId = MBX_MSGID_DEFAULT;
		lPtr->mbxParam = MBX_PARAM_DEFAULT;
		mbxFlags[i] = NO_MSG;
	}

    // plug the actual callbacks functions 
	_plugCallbacks(&cbackTable[0], NUM_SLAVE_MBX);

	NVIC_DisableIRQ((IRQn_Type)MASTER_IRQn);

	MASTER_TXEV_QUIT();

	// clear the interrupt
	NVIC_ClearPendingIRQ((IRQn_Type)MASTER_IRQn);
			
	// set the default priority for the interrupts
	NVIC_SetPriority((IRQn_Type)MASTER_IRQn, SLAVE_MAILBOX_PRIORITY);
				
	// enable the interrupt
	NVIC_EnableIRQ((IRQn_Type)MASTER_IRQn);
}
	
	
/* interrupt function for the Slave mailbox */ 
/* interrupt from Master on Slave side */
void M0_M4CORE_IRQHandler() {		

	mbxId_t i;
		
	// quit the interrupt
	MASTER_TXEV_QUIT();

	for(i=(mbxId_t)0; i<NUM_SLAVE_MBX;i++) {
	
		if(PROCESS == IPC_queryLocalMbx(i)) {
			_mbxProcess(i);
			mbxFlags[i] = MSG_PENDING;
		}
	};
}


/* download a processor image to the SLAVE CPU */
void IPC_downloadSlaveImage(uint32_t slaveRomStart, const unsigned char slaveImage[], uint32_t imageSize)
{
  	uint32_t i;
	volatile uint8_t *pu8SRAM;

	IPC_haltSlave();

    //Copy application into Slave ROM 
	pu8SRAM = (uint8_t *) slaveRomStart;
	for (i = 0; i < imageSize; i++)
	{
		pu8SRAM[i] = slaveImage[i];
	 }

	// Set Slave shadow pointer to begining of rom (where application is located) 
	*(volatile uint32_t *) SLAVE_SHADOW_REG = slaveRomStart;
}


/* take SLAVE processor out of reset */
void IPC_startSlave(void)
{
	volatile uint32_t u32REG, u32Val;
	
	// Release Slave from reset, first read status 
	u32REG = LPC_RGU->RESET_ACTIVE_STATUS1;
			
	// If the M0 is being held in reset, release it... 
	// 1 = no reset, 0 = reset
	while(!(u32REG & (1u << 24)))
	{
		u32Val = (~(u32REG) & (~(1 << 24)));
		LPC_RGU->RESET_CTRL1 = u32Val;
		u32REG = LPC_RGU->RESET_ACTIVE_STATUS1;
	};
}

/* put the SLAVE processor back in reset */
void IPC_haltSlave(void) {

	volatile uint32_t u32REG, u32Val;
	
	// Check if M0 is reset by reading status
	u32REG = LPC_RGU->RESET_ACTIVE_STATUS1;
			
	// If the M0 has reset not asserted, halt it... 
	// in u32REG, status register, 1 = no reset
	while ((u32REG & (1u << 24)))
	{
		u32Val = ( (~u32REG) | (1 << 24));
		LPC_RGU->RESET_CTRL1 = u32Val;
		u32REG = LPC_RGU->RESET_ACTIVE_STATUS1;			
	}
}


static void _plugCallbacks(CbackItem callBackTable[], uint32_t numMbx)
{	 
	Mbx* lPtr;
	mbxId_t i;

	for(i=(mbxId_t)0;i<numMbx;i++) {

 		lPtr = mbxLocalTablePtr;
		lPtr += callBackTable[i].mbxNum;
		
		lPtr->mbxAction = (mbxCallback_t) callBackTable[i].func;
	};
}

	  
void IPC_initMasterMbx(CbackItem cbackTable[], Mbx* masterMbxPtr, Mbx* slaveMbxPtr)
{
	int i;
	Mbx* lPtr;

	// initialize the pointers
	mbxLocalTablePtr = masterMbxPtr;
	mbxRemoteTablePtr = slaveMbxPtr;
	
	// clear the mailbox and the flags
	for(i=0, lPtr = mbxLocalTablePtr; i<NUM_MASTER_MBX; i++,lPtr++) {
		lPtr->mbxStatus = READY;
		lPtr->mbxHeader.msg = MBX_MSG_DEFAULT;
		lPtr->mbxHeader.msgId = MBX_MSGID_DEFAULT;
		lPtr->mbxParam = MBX_PARAM_DEFAULT;
		mbxFlags[i] = NO_MSG;
   }
   
   // plug the actual callbacks
   _plugCallbacks(&cbackTable[0], NUM_MASTER_MBX);

	SLAVE_TXEV_QUIT();

	// disable IRQ
	NVIC_DisableIRQ((IRQn_Type)SLAVE_IRQn);	

	// clear any pending interrupt
	NVIC_ClearPendingIRQ((IRQn_Type)SLAVE_IRQn);

	// set the default priority for the mbx interrupts
	NVIC_SetPriority((IRQn_Type)SLAVE_IRQn, MASTER_MAILBOX_PRIORITY);
			
	// enable the interrupt
	NVIC_EnableIRQ((IRQn_Type)SLAVE_IRQn);	
}

/* interrupt function for the master mailbox */
/* interrupt to master from slave */
void M0CORE_IRQHandler() {

	mbxId_t i;
		
	// acknowledge the interrupt
	SLAVE_TXEV_QUIT();

	for(i=(mbxId_t)0;i<NUM_MASTER_MBX;i++) {

		if(PROCESS == IPC_queryLocalMbx(i)) {

			_mbxProcess(i);

			mbxFlags[i] = MSG_PENDING;			
		}
	};
}




