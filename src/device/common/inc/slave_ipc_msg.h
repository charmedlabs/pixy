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

#ifndef __SLAVE_IPC_MSG_H 
#define __SLAVE_IPC_MSG_H

#ifdef __cplusplus
 extern "C" {
#endif 

/* commands the slave shall get */
enum ipcSlaveMsg_tag {

	CMD_SLAVE_NONE = 0,
	PRINT_WELCOME_MESSAGE,
	DATA_RESULT,
	PRINT_NUM_MESSAGES
		
};

#ifdef __cplusplus
}
#endif 

#endif
