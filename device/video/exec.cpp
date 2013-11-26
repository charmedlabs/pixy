#include <stdio.h>
#include "pixy_init.h"
#include "misc.h"
#include "exec.h"
#include "camera.h"

static const ProcModule g_module[] =
{
	{
	"running",
	(ProcPtr)exec_running, 
	{END}, 
	"Is a program running?"
	"@r 1 if a program is running, 0 if not"
	},
	{
	"stop",
	(ProcPtr)exec_stop, 
	{END}, 
	"Stop execution of the current program"
	"@r always returns 0"
	},
	{
	"run",
	(ProcPtr)exec_run, 
	{CRP_STRING, END}, 
	"Run the specified program"
	"@p program name"
	"@r returns 0 if successful, -1 otherwise"
	},
	{
	"progs",
	(ProcPtr)exec_list, 
	{END}, 
	"List available programs"
	"@r always returns 0"
	},
	END
};

uint8_t g_running = false;
uint8_t g_run = false;
int8_t g_program = -1;

int exec_init(Chirp *chirp)
{
	chirp->registerModule(g_module);
		
	return 0;	
}

uint32_t exec_running()
{
	return (uint32_t)g_running;
}

int32_t exec_stop()
{
	g_run = 0;
	g_program = -1;
	return 0;
}

int32_t exec_run(const char *prog)
{
	g_program = 0;
	g_run = true;
	g_running = true;		
	return 0;
}
void cprintf(const char *format, ...)
{
    char  buf[128];
    va_list args;
    va_start(args, format);
    vsprintf((char *)buf, (char const *)format, args);
    va_end(args);

	CRP_SEND_XDATA(g_chirpUsb, HSTRING(buf));
}

int32_t exec_list()
{
	cprintf("video\n");
	cprintf("track\n");

 	return 0;
}

void setup0()
{
}


void loop0()
{
	static int i = 0;
	//delayus(100000);
	cprintf("hello %d\n", i++);
	cam_getFrameChirp(0x21, 0, 0, 320, 200, g_chirpUsb);
}

void exec_loop()
{
	exec_run();
	while(1)
	{
		while(!g_run)
		{
			g_chirpUsb->service();
			if (!g_chirpUsb->connected() || !USB_Configuration)
				exec_run();
		}
		 	
		setup0();
		while(g_run)
		{
			loop0();
			while(g_chirpUsb->service());
		}
		// set variable to indicate we've stopped
		g_running = false;
	}
}
