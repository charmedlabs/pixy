// functions.h
#ifndef __M0_FUNCTIONS_H__
#define __M0_FUNCTIONS_H__



/* define here the number of mailbox desired */
enum slaveMbxId_tag {

		SLAVE_MBX_TASKD = 0,
		SLAVE_MBX_TASKE,
		SLAVE_MBX_TASKF,
		SLAVE_MBX_CMD,
		NUM_SLAVE_MBX,
};


/* reference here all the functions that should be executed as callbacks on M0 */
void slaveCbackD(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void slaveCbackE(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void slaveCbackF(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void slaveCbackCmd(msg_t msg, msgId_t idNum, mbxParam_t parameter);

#endif /* M0_callbacks.h */

