
#ifndef __M4_FUNCTIONS_H__
#define __M4_FUNCTIONS_H__


enum masterMbxId_tag {

		MASTER_MBX_TASKA = 0,
		MASTER_MBX_TASKB,
		MASTER_MBX_TASKC,
		MASTER_MBX_CMD,
		NUM_MASTER_MBX,

};


// external functions which can be called back from the M4
void masterCbackA(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void masterCbackB(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void masterCbackC(msg_t msg, msgId_t idNum, mbxParam_t parameter);
void masterCbackCmd(msg_t msg, msgId_t idNum, mbxParam_t parameter);

#endif

