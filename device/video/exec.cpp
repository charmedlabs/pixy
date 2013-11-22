#include "pixy_init.h"
#include "misc.h"
#include "exec.h"

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
	"list",
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

int32_t exec_list()
{
 	return 0;
}

void setup0()
{
}

void loop0()
{
	delayus(100000);
	g_chirpUsb->assemble(0, HSTRING("hello\n"), END);
}

void exec_loop()
{
	exec_run("");
	while(1)
	{
		while(!g_run)
			g_chirpUsb->service();

		setup0();
		while(g_run)
		{
			loop0();
			g_chirpUsb->service();
		}
		// set variable to indicate we've stopped
		g_running = false;
	}
}
