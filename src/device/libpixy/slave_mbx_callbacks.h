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

