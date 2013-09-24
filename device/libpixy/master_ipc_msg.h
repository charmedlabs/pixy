#ifndef __M4_IPC_MSG_H 
#define __M4_IPC_MSG_H

/* messages the M4 shall get */
enum ipcM4Msg_tag {

	CMD_MASTER_NONE = 0,
	NOTIFY_SLAVE_STARTED,
	REQUEST_PROCESS_DATA
		
};


#endif
