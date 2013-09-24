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
