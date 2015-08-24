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

