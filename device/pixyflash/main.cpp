#include <pixy_init.h>
#include "flash.h"

int main(void) 
 {	
 	pixySimpleInit();
	flash_init();
	while(1)
		g_chirpUsb->service();
}

