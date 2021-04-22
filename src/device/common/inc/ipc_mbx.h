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

#ifndef __IPC_H__
#define __IPC_H__

#include "platform_config.h"

#ifdef __cplusplus
 extern "C" {
#endif 

/************************************************/
/* types of ipc messages 						*/
/************************************************/
typedef enum mbxStatus_t {
	READY = 0,
	PROCESS,
	BUSY,
	ERROR_OCCURRED,
	NUM_IPC_CMD
} mbxStat_t;

#define MSG_PENDING (1)
#define NO_MSG		(2)

typedef uint8_t mbxFlag_t;

/************************************************/
/* type definition of IPC items 				*/
/************************************************/
#include "master_ipc_msg.h"
#include "slave_ipc_msg.h"



#define MBX_MSG_DEFAULT 	(msg_t) 0x0
#define MBX_MSGID_DEFAULT 	(msgId_t) 0x0
#define MBX_DUMMY_CALLBACK 	(mbxCallback_t) &IPC_dummyCallback
#define MBX_PARAM_DEFAULT 	(mbxParam_t) 0x0

#define LOOP_INSTRUCTION	0xE7FEE7FE

typedef uint16_t 	msgId_t;
typedef uint8_t 	msg_t;
typedef uint8_t		mbxId_t;
typedef uint32_t 	mbxParam_t;
 

/**********************************************/
/* callback definitions */
/**********************************************/
	
typedef void (*mbxCallback_t) (msg_t msg, msgId_t idNum, mbxParam_t param);

/************************************************/
/* mailbox type definition 				*/
/************************************************/
typedef struct Mbx_tag {

	/* status of the mailbox */
	mbxStat_t	mbxStatus;	

	/* header holding the ipc message and the message id */
	struct {

		msg_t	msg;	
		msgId_t msgId;	

	} mbxHeader;


	/* optional parameter for the ipc message */
	mbxParam_t mbxParam;	

	/* optional associated callback */
	mbxCallback_t mbxAction;

} Mbx;

/* pointer to the local mailbox */
extern  Mbx* mbxTableLocalPtr;

/* pointer to the remote mailbox */
extern  Mbx* mbxTableRemotePtr;


/**********************************************/
/* functions for mailbox handling */
/**********************************************/

/**********************************************/
/* local mailbox functions */
/**********************************************/
/* query the local mailbox */
mbxStat_t IPC_queryLocalMbx(mbxId_t mbxNum);

/* retrieve the type of command received */
msg_t IPC_getMsgType(mbxId_t mbxNum);

/* retrieve the msgId received */
msgId_t IPC_getMsgId(mbxId_t mbxNum);

/* retrieve the mailbox parameter (if used) */
mbxParam_t IPC_getMbxParameter(mbxId_t mbxNum);

/* reset the notification flag */
/* shortly disables interrupts */
void IPC_resetMbxFlag(mbxId_t mbxNum);

/* forward declaration */
void _setMbxStatus(mbxId_t mbxNum, mbxStat_t status);

/* set the status as busy */
#define IPC_lockMbx(mbxId) _setMbxStatus(mbxId, BUSY)

/* set the status as ready */
#define IPC_freeMbx(mbxId) _setMbxStatus(mbxId, READY)

/* signal an error */
#define IPC_setMbxErr(mbxId) _setMbxStatus(mbxId, ERROR_OCCURRED)


/**********************************************/
/* remote mailbox functions 				  */
/**********************************************/
/* function to peek the remote MBX status */
mbxStat_t IPC_queryRemoteMbx(mbxId_t mbxNum);

/* send a message to a specific mailbox */
/* configures the mailbox as process, triggers an interrupt to the remote cpu */
void IPC_sendMsg(mbxId_t mbxNum, msg_t msg, msgId_t msgNum, mbxParam_t param);


void IPC_dummyCallback(msg_t msg, msgId_t idNum, mbxParam_t param);



#include "master_mbx_callbacks.h"
 
typedef struct CbackItem_tag {

		mbxId_t 		mbxNum;
		mbxCallback_t 	func;

} CbackItem;

extern CbackItem Master_CbackTable[NUM_MASTER_MBX];
extern volatile mbxFlag_t mbxFlags[NUM_MASTER_MBX];



#include "slave_mbx_callbacks.h"


extern CbackItem Slave_CbackTable[NUM_SLAVE_MBX];
extern volatile mbxFlag_t mbxFlags[NUM_SLAVE_MBX];


/**********************************************/
/* functions to initialize the framework */
/**********************************************/
/* download a processor image to the M0 CPU */
void IPC_downloadSlaveImage(uint32_t SlaveRomStart, const unsigned char slaveImage[], uint32_t imageSize);

/* take processor out of reset */
void IPC_startSlave(void);

/* put the processor back in reset */
void IPC_haltSlave(void);

/* initialize the MBX ipc framework */
void IPC_initSlaveMbx(CbackItem cbackTable[], Mbx* masterMbxPtr, Mbx* slaveMbxPtr);
void IPC_initMasterMbx(CbackItem cbackTable[], Mbx* masterMbxPtr, Mbx* slaveMbxPtr);

#ifdef __cplusplus
}
#endif 

#endif /* __IPC_H__ */
